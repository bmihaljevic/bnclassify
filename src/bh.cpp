#include <Rcpp.h>
// [[Rcpp::depends(BH)]]

#include <boost/config.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/connected_components.hpp>
#include <boost/graph/directed_graph.hpp>
#include <boost/graph/subgraph.hpp>
#include <boost/graph/graph_utility.hpp> 
#include <boost/graph/copy.hpp> 
#include <boost/property_map/property_map.hpp>
#include <boost/graph/kruskal_min_spanning_tree.hpp>
#include <boost/graph/topological_sort.hpp>
#include <boost/graph/filtered_graph.hpp>

/**
 * Boost uses integers as vertex ids, not names. 
 * I cannot have the boost object in R, thus these functions just perform an operation and go back to the R structure. 
 * And this is either a matrix or an adj list. 
 */  

using namespace boost; 
using namespace Rcpp;        



// TODO: move this to basic-misc one moved to a header
// TODO: R match was returning -2147483648 when not finding the value, and the any() test was failing. 
// Thus, avoid Rcpp for the test 
// [[Rcpp::export]]
std::vector<int> match_zero_based(const CharacterVector & subset, const CharacterVector & superset) { 
  IntegerVector subset_inds = Rcpp::match(subset, superset); 
  int min = *std::min_element(subset_inds.begin(), subset_inds.end());
  if (min <= 0)  stop("All subset must be in the superset.");
  subset_inds = subset_inds - 1; 
  return as<std::vector<int> > (subset_inds);
}     

typedef property<vertex_index_t, int, property<vertex_name_t, std::string> > VertexProperty;
typedef property<edge_index_t, int, property<edge_weight_t, double> > EdgeProperty; 
typedef adjacency_list< vecS, vecS, directedS, VertexProperty, EdgeProperty >  dgraph;    

// for connected components and such
// typedef adjacency_list <vecS, vecS, property < edge_weight_t, double >, undirectedS> ugraph;  
typedef adjacency_list < vecS, vecS, undirectedS, VertexProperty, property < edge_weight_t, double > > ugraph; 

// modifiable directed graph. Uses listS instead of vecS. vecS could lead to invalidated descriptors. <https://www.boost.org/doc/libs/1_37_0/libs/graph/doc/adjacency_list.html>
// not used cause it does not allow int descriptors
typedef adjacency_list<listS, listS, directedS, VertexProperty, EdgeProperty >  mdgraph; 
typedef adjacency_list < listS, listS, undirectedS, VertexProperty, property < edge_weight_t, double > > mugraph; 

// TODO: remove
typedef subgraph< adjacency_list< vecS, vecS, directedS, VertexProperty, EdgeProperty > > dsubgraph;

/**
 * Because num_vertices does not give the number of nodes in filtered graph.
 */
template <class T>
int robust_num_vertices(T g) {
  typedef typename graph_traits<T>::vertex_iterator vertex_iter;   
  std::pair<vertex_iter, vertex_iter> vp;
  vp = vertices(g);
  return std::distance(vp.first, vp.second); 
} 

/**
 * Because num_edges does not give the number of nodes in filtered graph.
 */
template <class T>
int robust_num_edges(T g) { 
  typedef typename graph_traits<T>::edge_iterator edge_iter;  
  std::pair<edge_iter, edge_iter> ep = edges(g) ; 
  return std::distance(ep.first, ep.second); 
} 

/**
 * Will return a list with nodes and edges. Does not have the names though unless I save them somewhere.
 * If I did not pass the subgraph by reference here, it would not print.
 */
template <class T>
Rcpp::List graph2R(T & g) { 
  typedef typename graph_traits<T>::vertex_descriptor Vertex; 
  typedef typename property_map<T, vertex_index_t>::type IndexMap;
  typedef typename property_map<T, vertex_name_t>::type NameMap;
  typedef typename graph_traits<T>::vertex_iterator vertex_iter;
  typedef typename graph_traits<T>::edge_iterator edge_iter; 
  typedef typename property_map<T, edge_weight_t>::type WeightMap;

  IndexMap index = get(vertex_index, g);
  NameMap names = get(vertex_name, g);
  WeightMap weight = get(edge_weight, g);
  
  std::pair<vertex_iter, vertex_iter> vp;
  // std::vector<int> nodes;
  std::vector<std::string> nodes;
  int nvertices = robust_num_vertices(g);
  nodes.reserve(nvertices);  
  
  for (vp = vertices(g); vp.first != vp.second; ++vp.first) {    
    Vertex v = *vp.first; 
    // nodes.push_back(index[v]);
    std::string vname = names[v];
    nodes.push_back(vname);
  }  
  
  std::pair<edge_iter, edge_iter> ep; 
  int nedges = robust_num_edges(g);
  Rcpp::IntegerMatrix edges_matrix(nedges, 2);
  colnames(edges_matrix) = CharacterVector::create("from", "to");
  Vertex u, v;
  int row  = 0;
  
  Rcpp::NumericVector weights(nedges);
  
  typedef typename graph_traits<T>::edge_descriptor edge; 
  for (ep = edges(g); ep.first != ep.second; ++ep.first) {
    edge e = *ep.first;
    u = source(*ep.first,g);
    v = target(*ep.first,g);
    edges_matrix(row, 0) = u;
    edges_matrix(row, 1) = v;
    weights[row] = get(weight, e);
    row++;
  }
  
  List output = List::create(Named("nodes") = nodes, Named("edges") = edges_matrix, Named("weights") = weights);
  return output;
}



/**
 *  Since not not all vertices need to be in edges, add vertices separately.
 */ 
template <class T>
T r2graph(CharacterVector vertices, Rcpp::IntegerMatrix edges, NumericVector weights) {   
  // Add vertices, if any 
  int n = vertices.size(); 
  T g(n);    
  
  typedef typename property_map<T, vertex_name_t>::type NameMap; 
  NameMap name = get(vertex_name_t(), g);  
  for (int i = 0; i < n; i++) { 
    name[i] = vertices[i];
  } 

  // Add edges, if any 
  int nedges = edges.nrow();
  for (std::size_t i = 0; i < nedges; i++) { 
    add_edge(edges(i, 0), edges(i, 1), weights.at(i), g);
  }    
  
  return g;  
} 

template <class T>
T r2graph(CharacterVector vertices, Rcpp::IntegerMatrix edges) {    
  NumericVector weights(edges.size());
  return r2graph<T>(vertices, edges, weights);   
}      

// Requires an undirected graph   
// [[Rcpp::export]]  
NumericVector bh_connected_components(CharacterVector vertices, Rcpp::IntegerMatrix edges) { 
  ugraph g  =  r2graph<ugraph>(vertices,  edges);
  std::vector<int> component(num_vertices(g));
  int num = connected_components(g, &component[0]);
  // TODO:: see additional checks from RBGL. Maybe connected comp might fail?
  // std::vector<int>::size_type i;
  return wrap(component);
}      


template <typename NameMap>
struct remove_names {
  remove_names() { }
  remove_names(NameMap weight, std::vector<std::string> remove) : m_weight(weight), m_remove(remove) { }
  template <typename Vertex>
  bool operator()(const Vertex& e) const {
    std::string name =  get(m_weight, e);
    return  m_remove.end() == std::find(m_remove.begin(), m_remove.end(), name );
  }
  NameMap m_weight;
  std::vector<std::string> m_remove;
};

template <typename NameMap, typename Graph>
struct remove_edge_names {
  remove_edge_names() { }
  remove_edge_names(NameMap weight, std::vector<std::string> remove_from, std::vector<std::string> remove_to, Graph graph) : 
          m_weight(weight), m_remove_from(remove_from), m_remove_to(remove_to), m_graph(graph) { } 
  bool find(const std::string & value, const std::vector<std::string> & vector) const {
    return vector.end() != std::find(vector.begin(), vector.end(), value );
  }
  template <typename Edge>
  bool operator()(const Edge& e) const { 
    std::string from =  get(m_weight, source(e, m_graph));
    std::string to =  get(m_weight, target(e, m_graph));
    // Consider undirected
    bool arc = find(from, m_remove_from) && find(to, m_remove_to); 
    bool reversed = find(to, m_remove_from) && find(from, m_remove_to); 
    return arc || reversed;
  }
  NameMap m_weight; 
  std::vector<std::string> m_remove_from;
  std::vector<std::string> m_remove_to;
  Graph m_graph;
};

// TODO rename bh_remove_nodeS
// [[Rcpp::export]]  
Rcpp::List bh_remove_node(const CharacterVector & vertices, const Rcpp::IntegerMatrix & edges, const CharacterVector & remove) {
  dgraph g  = r2graph<dgraph>(vertices,  edges);
  
  typedef property_map<dgraph, vertex_name_t>::type NameMap ;
  std::vector<std::string> remove_vec = Rcpp::as<std::vector<std::string> >(remove);
  remove_names<NameMap> filter(get(vertex_name, g), remove_vec);

  typedef filtered_graph<dgraph, keep_all, remove_names<NameMap> > fgraph;
  typedef graph_traits<fgraph>::vertex_iterator vertex_iter;
  fgraph fg(g, keep_all(), filter);
  return graph2R(fg);
} 



// [[Rcpp::export]]  
Rcpp::List bh_remove_edges(const CharacterVector & vertices, const Rcpp::IntegerMatrix & edges, const CharacterVector & remove_from, 
                          const CharacterVector & remove_to, const CharacterVector & edgemode) { 
  if (edgemode[0] != "undirected") stop("Currently not implemented for directed.");
  if (remove_from.size() != remove_to.size()) stop("From and to different lengths.");
  // Copy to graph with listS so that I can modify 
  ugraph a  = r2graph<ugraph>(vertices,  edges);   
  mugraph g;
  copy_graph(a, g);  
  typedef property_map<mugraph, vertex_name_t>::type NameMap ;
  
  std::vector<std::string> remove_from_vec = as<std::vector<std::string> >(remove_from );
  std::vector<std::string> remove_to_vec = as<std::vector<std::string> >(remove_to ); 
  remove_edge_names<NameMap, mugraph> filter(get(vertex_name, g), remove_from_vec, remove_to_vec,   g);
  remove_edge_if(filter, g); 
  
  ugraph  output;
  copy_graph(g, output);   
 
  return graph2R(output); 
}

// TODO: not currently used
// dsubgraph make_subgraph(dgraph & g, const CharacterVector & subgraph_vertices, const CharacterVector & vertices)  {  
//   dsubgraph a; 
//   boost::copy_graph(g, a); 
//   dsubgraph subgraph = a.create_subgraph(); 
// //  If you add particular vertices from global, are they kept?
//   std::vector<int> sgraph_vertices = match_zero_based(subgraph_vertices, vertices);
//   for (int i = 0; i < sgraph_vertices.size(); i++) {
//     int a = add_vertex(sgraph_vertices.at(i), subgraph); 
//   }
//   return subgraph;  
// }
// 
///// [[]][[Rcpp::export]]  
// Rcpp::List bh_subgraph(const CharacterVector & vertices, const Rcpp::IntegerMatrix & edges, const CharacterVector & subgraph_vertices) {
//   dgraph g  = r2graph<dgraph>(vertices,  edges); 
//   dsubgraph subgraph = make_subgraph (g, subgraph_vertices, vertices) ;   
//   // workaround, i do not know have to make graph2R generic enough
//   dgraph output;
//   copy_graph(subgraph, output);
//   return graph2R(output);
// }  


// [[Rcpp::export]]  
Rcpp::List bh_subgraph(const CharacterVector & vertices, const Rcpp::IntegerMatrix & edges, const CharacterVector & subgraph_vertices) { 
  CharacterVector remove = setdiff(vertices, subgraph_vertices);
  return bh_remove_node(vertices, edges, remove);  
} 

// [[Rcpp::export]]   
Rcpp::List bh_mstree_kruskal(CharacterVector vertices, Rcpp::IntegerMatrix edges, NumericVector weights) {
  ugraph g = r2graph<ugraph>(vertices, edges, weights);
  typedef graph_traits < ugraph >::edge_descriptor Edge;
  property_map < ugraph, edge_weight_t >::type weight = get(edge_weight, g);
  std::vector < Edge > spanning_tree;
  
  kruskal_minimum_spanning_tree(g, std::back_inserter(spanning_tree));
  
  int nedges = std::distance(spanning_tree.begin(), spanning_tree.end());
  Rcpp::IntegerMatrix  kruskal_edges(nedges, 2);    
  Rcpp::NumericVector weights_vector(nedges);          
  
  int row = 0;
  for (std::vector < Edge >::iterator ei = spanning_tree.begin();
       // TODO: this is replacated above. Extract to a function.
       ei != spanning_tree.end(); ++ei) {  
      kruskal_edges(row, 0) = source(*ei, g);
      kruskal_edges(row, 1) = target(*ei, g); 
      weights_vector[row] = get(weight, *ei);
      row++;
  }    
  ugraph krusk = r2graph<ugraph>(vertices, kruskal_edges, weights_vector );
  return graph2R(krusk);       
} 

// I think tsort may throw an exception
// [[Rcpp::export]]   
NumericVector bh_tsort(CharacterVector vertices, Rcpp::IntegerMatrix edges) {  
  dgraph g = r2graph<dgraph>(vertices, edges);
  std::vector<int> sorted;
  topological_sort(g, std::back_inserter(sorted)); 
  return wrap(sorted);
}
  
/*** R  
dag <- anb_make_nb('a', letters[2:6])
dag <- graphNEL2_graph_internal(dag)
bh_connected_components(dag$nodes, dag$edges) 
bh_subgraph( dag$nodes, dag$edges, dag$nodes)
bh_subgraph( dag$nodes, dag$edges, setdiff(dag$nodes, 'a'))
bh_subgraph( dag$nodes, dag$edges, setdiff(dag$nodes, 'f'))
# bh_subgraph( dag$nodes, dag$edges, 'Bojan')
test_sgraph <- function(feature) { 
  a <- bh_subgraph( dag$nodes, dag$edges, setdiff(dag$nodes, feature))
  stopifnot(all(sapply(a$nodes, nchar) >  0))
}  
a <- replicate(n = 1e3, test_sgraph('f') )


bh_subgraph( dag$nodes, dag$edges, setdiff(dag$nodes, 'f'))
bh_subgraph( dag$nodes, dag$edges, setdiff(dag$nodes, 'f'))
bh_subgraph2( dag$nodes, dag$edges, setdiff(dag$nodes, 'c'))

nedges <- length(dag$edges)
bh_mstree_kruskal(dag$nodes, dag$edges, rep(1:nedges))
bh_tsort(dag$nodes, dag$edges) 
bh_remove_edges(dag$nodes, dag$edges, dag$nodes[1], dag$nodes[2], "undirected")
bh_remove_edges(dag$nodes, dag$edges, 'a', 'b', "undirected")
bh_remove_edges(dag$nodes, dag$edges, 'a', 'f', "undirected")
# load('tmp-g-subgraph.rdata')
# bh_subgraph( g$nodes, g$edges, setdiff(g$nodes, "class"))
*/
