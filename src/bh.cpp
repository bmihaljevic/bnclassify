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
typedef property<edge_index_t, int> EdgeProperty; 
typedef adjacency_list< vecS, vecS, directedS, VertexProperty, EdgeProperty >  dgraph;  


typedef subgraph< adjacency_list< vecS, vecS, directedS, VertexProperty, EdgeProperty > > dsubgraph;
  
// for connected components and such
// typedef adjacency_list <vecS, vecS, property < edge_weight_t, double >, undirectedS> ugraph;  
typedef adjacency_list < vecS, vecS, undirectedS, VertexProperty, property < edge_weight_t, double > > ugraph;

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

  IndexMap index = get(vertex_index, g);
  NameMap names = get(vertex_name, g);
  
  std::pair<vertex_iter, vertex_iter> vp;
  // std::vector<int> nodes;
  std::vector<std::string> nodes;
  nodes.reserve(num_vertices(g));  
  
  for (vp = vertices(g); vp.first != vp.second; ++vp.first) {    
    Vertex v = *vp.first; 
    // nodes.push_back(index[v]);
    std::string vname = names[v];
    nodes.push_back(vname);
  }  
  
  std::pair<edge_iter, edge_iter> ep; 
  int nedges = num_edges(g);
  Rcpp::IntegerMatrix edges_matrix(nedges, 2);
  Vertex u, v;
  int row  = 0;
  
  typedef typename graph_traits<T>::edge_descriptor edge; 
  for (ep = edges(g); ep.first != ep.second; ++ep.first) {
    edge e = *ep.first;
    u = source(*ep.first,g);
    v = target(*ep.first,g);
    edges_matrix(row, 0) = u;
    edges_matrix(row, 1) = v;
    row++;
  }
  
  List output = List::create(Named("nodes") = nodes, Named("edges") = edges_matrix);
  return output;
}

/**
 * This is an internal function.   
 * Since not not all vertices need to be in edges, add vertices separately.
 */
dgraph bh_make_graph(CharacterVector vertices, Rcpp::IntegerMatrix edges)
{ 
  // any checks?
  // Add vertices, if any 
  int n = vertices.size(); 
  dgraph g; 
  property_map<dgraph, vertex_name_t>::type name = get(vertex_name_t(), g);  
  for (int i = 0; i < n; i++) { 
    add_vertex(g);
    name[i] = vertices[i];
  } 
  // Add edges, if any 
  int nedges = edges.nrow();
  for (int i = 0; i < nedges; i++) { 
    add_edge(edges(i, 0), edges(i, 1), g);
  }  
  return g; 
}


/**
 * This is an internal function.   
 * Since not not all vertices need to be in edges, add vertices separately.
 * This is currently separate because I needed to specify the type in order for name property access to compile. 
 * Do not know how to make a common type with shared properties for dgraph and ugraph.
 */


ugraph bh_make_ugraph(CharacterVector vertices, Rcpp::IntegerMatrix edges, NumericVector weights)
{ 
  // any checks?
  // Add vertices, if any 
  int n = vertices.size(); 
  ugraph g(n);   

  // Add edges, if any 
  int nedges = edges.nrow();
  for (std::size_t i = 0; i < nedges; i++) { 
    add_edge(edges(i, 0), edges(i, 1), weights.at(i), g);
  }  
  
  // 
  // Graph g(num_nodes);
  // property_map<Graph, edge_weight_t>::type weightmap = get(edge_weight, g);
  // for (j = 0; j < num_edges; ++j) {
  //   Edge e; bool inserted;
  //   tie(e, inserted) = add_edge(edge_array[j].first, edge_array[j].second, g);
  //   weightmap[e] = weights[j];
  // }
  
  return g; 
}
ugraph bh_make_ugraph(CharacterVector vertices, Rcpp::IntegerMatrix edges) {
  NumericVector weights(edges.size());
  return bh_make_ugraph(vertices, edges, weights);
}


// [[Rcpp::export]]  
void test_make(CharacterVector vertices, Rcpp::IntegerMatrix edges) {
  dgraph g  = bh_make_graph(vertices,  edges);
  // print_vertices(g); 
}      

// Requires an undirected graph   
// [[Rcpp::export]]  
NumericVector bh_connected_components(CharacterVector vertices, Rcpp::IntegerMatrix edges) { 
  ugraph g  = bh_make_ugraph(vertices,  edges);
  std::vector<int> component(num_vertices(g));
  int num = connected_components(g, &component[0]);
  // TODO:: see additional checks from RBGL. Maybe connected comp might fail?
  // std::vector<int>::size_type i;
  return wrap(component);
}      

dsubgraph make_subgraph(dgraph & g, const CharacterVector & subgraph_vertices, const CharacterVector & vertices)  {  
  dsubgraph a; 
  boost::copy_graph(g, a); 
  dsubgraph subgraph = a.create_subgraph(); 
//  If you add particular vertices from global, are they kept?
  std::vector<int> sgraph_vertices = match_zero_based(subgraph_vertices, vertices);
  for (int i = 0; i < sgraph_vertices.size(); i++) {
    int a = add_vertex(sgraph_vertices.at(i), subgraph); 
  }
  return subgraph;  
}

// [[Rcpp::export]]  
Rcpp::List bh_subgraph(const CharacterVector & vertices, const Rcpp::IntegerMatrix & edges, const CharacterVector & subgraph_vertices) {
  dgraph g  = bh_make_graph(vertices,  edges); 
  dsubgraph subgraph = make_subgraph (g, subgraph_vertices, vertices) ;   
  // workaround, i do not know have to make graph2R generic enough
  dgraph output;
  copy_graph(subgraph, output);
  return graph2R(output);
}  

// [[Rcpp::export]]  
Rcpp::List bh_remove_node(const CharacterVector & vertices, const Rcpp::IntegerMatrix & edges, const CharacterVector & remove) { 
  dgraph g  = bh_make_graph(vertices,  edges);  
  std::vector<int> remove_ind = match_zero_based(remove, vertices);
  if (remove_ind.size() > 1) stop("More than one match!");
  int remove_index = remove_ind.at(0);
  clear_vertex(remove_index, g);
  remove_vertex(remove_index, g);    
  return graph2R(g);
}

// [[Rcpp::export]]   
Rcpp::List kruskal(CharacterVector vertices, Rcpp::IntegerMatrix edges, NumericVector weights) {
  ugraph g = bh_make_ugraph(vertices, edges, weights);
  typedef graph_traits < ugraph >::edge_descriptor Edge;
  property_map < ugraph, edge_weight_t >::type weight = get(edge_weight, g);
  std::vector < Edge > spanning_tree;
  
  kruskal_minimum_spanning_tree(g, std::back_inserter(spanning_tree));
  
  int nedges = std::distance(spanning_tree.begin(), spanning_tree.end());
  Rcpp::IntegerMatrix  kruskal_edges(nedges, 2); 
  int row = 0;
  for (std::vector < Edge >::iterator ei = spanning_tree.begin();
       // TODO: this is replacated above. Extract to a function.
       ei != spanning_tree.end(); ++ei) {  
      kruskal_edges(row, 0) = source(*ei, g);
      kruskal_edges(row, 1) = target(*ei, g);
      row++;
    // add_edge(source(*ei, g), target(*ei, g), weight[*ei], krusk);
  }   
  
  ugraph krusk = bh_make_ugraph(vertices, kruskal_edges);
  return graph2R(krusk);    
  
  // int n = num_vertices(g);
  // ugraph krusk(n);  
  // property_map<ugraph, vertex_name_t>::type name = get(vertex_name_t(), krusk);  
  // for (int i = 0; i < n; i++) { 
  //   name[i] = vertices[i];
  // } 
  
}

  
/*** R  
dag <- anb_make_nb('a', letters[2:6])
test_make(dag$nodes, dag$edges)
bh_connected_components(dag$nodes, dag$edges) 
bh_subgraph( dag$nodes, dag$edges, dag$nodes)
bh_subgraph( dag$nodes, dag$edges, setdiff(dag$nodes, 'a'))
bh_subgraph( dag$nodes, dag$edges, setdiff(dag$nodes, 'f'))
# bh_subgraph( dag$nodes, dag$edges, 'Bojan')
test_sgraph <- function(feature) { 
  a <- bh_subgraph( dag$nodes, dag$edges, setdiff(dag$nodes, feature))
  stopifnot(all(sapply(a$nodes, nchar) >  0))
}  
a <- replicate(n = 1000, test_sgraph('f') )
nedges <- length(dag$edges)
kruskal(dag$nodes, dag$edges, rep(1:nedges))

# load('tmp-g-subgraph.rdata')
# bh_subgraph( g$nodes, g$edges, setdiff(g$nodes, "class"))
*/
