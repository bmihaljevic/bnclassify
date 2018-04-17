#include <Rcpp.h>
// [[Rcpp::depends(BH)]]

#include <boost/config.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/connected_components.hpp>
#include <boost/graph/directed_graph.hpp>
#include <boost/graph/subgraph.hpp>

/**
 * Boost uses integers as vertex ids, not names. 
 * I cannot have the boost object in R, thus these functions just perform an operation and go back to the R structure. 
 * And this is either a matrix or an adj list. 
 */  

using namespace boost; 
using namespace Rcpp;       

// The graph type I will use
typedef adjacency_list<vecS, vecS, directedS> dgraph;
// for connected components and such
typedef adjacency_list <vecS, vecS, undirectedS> ugraph; 
// typedef boost::directed_graph<> dgraph;
// this one did not work:
// typedef adjacency_list<boost::directedS> dgraph;  
typedef subgraph< adjacency_list< vecS, vecS, directedS, no_property, property< edge_index_t, int > > > dsubgraph;
 
void print_vertices(dgraph g) {  
  typedef graph_traits<dgraph>::vertex_descriptor Vertex; 
  // get the property map for vertex indices
  typedef property_map<dgraph, vertex_index_t>::type IndexMap;
  IndexMap index = get(vertex_index, g);
  
  Rcout << "Num vert" << num_vertices(g) << std::endl;
  Rcout << "vertices(g) = ";
  typedef graph_traits<dgraph>::vertex_iterator vertex_iter;
  std::pair<vertex_iter, vertex_iter> vp;
  for (vp = vertices(g); vp.first != vp.second; ++vp.first) {
    Vertex v = *vp.first;
    Rcout << index[v] <<  " ";
  }
  Rcout << std::endl;   
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


// std::vector<int> match_zero_based
// // -1 because we need 0-based indices
//   IntegerVector  mtch = Rcpp::match(subgraph_vertices, vertices) - 1;
//   std::vector<int> sgraph_vertices = as<std::vector<int> >(mtch );

// [[Rcpp::export]]  
void bh_subgraph(CharacterVector subgraph_vertices, CharacterVector vertices, Rcpp::IntegerMatrix edges) {  
  
  
  Rcout << mtch << std::endl;
  dsubgraph g  = bh_make_graph<dsubgraph>(vertices,  edges);
  dsubgraph& subgraph = g.create_subgraph(); 
//  If you add particular vertices from global, are they kept?
  add_vertex(0, subgraph);
  add_vertex(1, subgraph); 
  Rcout << "Num edges" << num_edges(subgraph) << std::endl;
  // vertices are ignored when building graph. 
  // need to find vertices by their id.  Or need to simply map their names to their ids. 
  // If these were int, it would be very simple.
  // print_vertices(subgraph);
}

  
/*** R
dag <- anb_make_nb('a', letters[2:6])
test_make(dag$nodes, dag$edges)
bh_connected_components(dag$nodes, dag$edges) 
bh_subgraph(dag$nodes, dag$nodes, dag$edges) 
bh_subgraph('Bojan', dag$nodes, dag$edges) 
# For connected:  split(0:5, a)  
*/
