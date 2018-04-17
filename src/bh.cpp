#include <Rcpp.h>
// [[Rcpp::depends(BH)]]

#include <boost/config.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/connected_components.hpp>
#include <boost/graph/directed_graph.hpp>
#include <boost/graph/subgraph.hpp>

// TODO: pass from R by const reference!! 

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
  Rcout << subset_inds << std::endl;
  return as<std::vector<int> > (subset_inds);
} 

// The graph type I will use
typedef adjacency_list<vecS, vecS, directedS> dgraph;
// for connected components and such
typedef adjacency_list <vecS, vecS, undirectedS> ugraph; 
// typedef boost::directed_graph<> dgraph;
// this one did not work:
// typedef adjacency_list<boost::directedS> dgraph;  
typedef subgraph< adjacency_list< vecS, vecS, directedS, vertex_index_t, property< edge_index_t, int > > > dsubgraph;   

/**
 * Will return a list with nodes and edges. Does not have the names though unless I save them somewhere.
 */
Rcpp::List graph2R(dsubgraph g) {
  typedef graph_traits<dsubgraph>::vertex_descriptor Vertex; 
  typedef property_map<dsubgraph, vertex_index_t>::type IndexMap;
  IndexMap index = get(vertex_index, g);
  
  typedef graph_traits<dsubgraph>::vertex_iterator vertex_iter;
  std::pair<vertex_iter, vertex_iter> vp;
  std::vector<int> nodes;
  nodes.reserve(num_vertices(g)); 
  for (vp = vertices(g); vp.first != vp.second; ++vp.first) {
    Vertex v = *vp.first;
    nodes.push_back(index[v]);
  } 
  // TODO: add the edges.  
  
  List output = List::create(Named("nodes") = nodes);
  return output;
}

/**
 * This is an internal function.   
 * Since not not all vertices need to be in edges, add vertices separately.
 */
template <class T>
T bh_make_graph(CharacterVector vertices, Rcpp::IntegerMatrix edges)
{
  // any checks?
  // Add vertices, if any 
  int n = vertices.size(); 
  T g;
  for (int i = 0; i < n; i++) { 
    add_vertex(g);
  } 
  // Add edges, if any 
  int nedges = edges.nrow();
  for (int i = 0; i < nedges; i++) { 
    add_edge(edges(i, 0), edges(i, 1), g);
  }  
  return g; 
}

// [[Rcpp::export]]  
void test_make(CharacterVector vertices, Rcpp::IntegerMatrix edges) {
  dgraph g  = bh_make_graph<dgraph>(vertices,  edges);
  print_vertices(g); 
}      

// Requires an undirected graph   
// [[Rcpp::export]]  
NumericVector bh_connected_components(CharacterVector vertices, Rcpp::IntegerMatrix edges) { 
  ugraph g  = bh_make_graph<ugraph>(vertices,  edges);
  // print_vertices(g);
  std::vector<int> component(num_vertices(g));
  int num = connected_components(g, &component[0]); 
  // TODO:: see additional checks from RBGL. Maybe connected comp might fail?
  // std::vector<int>::size_type i;   
  // TODO: the split done by RBGL!!! 
  return wrap(component);  
}     


dsubgraph  make_subgraph(CharacterVector subgraph_vertices, CharacterVector vertices, Rcpp::IntegerMatrix edges)  {
  dsubgraph g  = bh_make_graph<dsubgraph>(vertices,  edges);
  dsubgraph& subgraph = g.create_subgraph(); 
//  If you add particular vertices from global, are they kept?
  std::vector<int> sgraph_vertices = match_zero_based(subgraph_vertices, vertices);
  for (int i = 0; i < sgraph_vertices.size(); i++) {
    add_vertex(sgraph_vertices.at(i), subgraph);
  }
  return subgraph;  
}

// [[Rcpp::export]]  
void bh_subgraph(CharacterVector subgraph_vertices, CharacterVector vertices, Rcpp::IntegerMatrix edges) {
  const dsubgraph  & subgraph = make_subgraph (subgraph_vertices, vertices, edges) ;
  graph2R(subgraph );
} 

  
/*** R
dag <- anb_make_nb('a', letters[2:6])
test_make(dag$nodes, dag$edges)
bh_connected_components(dag$nodes, dag$edges) 
bh_subgraph(dag$nodes, dag$nodes, dag$edges)
bh_subgraph('Bojan', dag$nodes, dag$edges)
*/
