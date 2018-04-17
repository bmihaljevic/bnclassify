#include <Rcpp.h>
// [[Rcpp::depends(BH)]]

#include <boost/config.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/connected_components.hpp>
#include <boost/graph/directed_graph.hpp>

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
T make_graph(CharacterVector vertices, Rcpp::IntegerMatrix edges)
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
  dgraph g  = make_graph<dgraph>(vertices,  edges);
  print_vertices(g); 
}      

// Requires an undirected graph   
// [[Rcpp::export]]  
NumericVector connected_components(CharacterVector vertices, Rcpp::IntegerMatrix edges) { 
  ugraph g  = make_graph<ugraph>(vertices,  edges);
  // print_vertices(g);
  std::vector<int> component(num_vertices(g));
  int num = connected_components(g, &component[0]); 
  // TODO:: see additional checks from RBGL. Maybe connected comp might fail?
  // std::vector<int>::size_type i;   
  // TODO: the split done by RBGL!!! 
  return wrap(component);  
  // Rcpp::split()? Do it in R. 
}  

  
/*** R
dag <- anb_make_nb('a', letters[2:6])
dag <- graph_internal2bgl(dag)
test_make(dag$nodes, dag$edges)
connected_components(dag$nodes, dag$edges) 
# For connected:  split(0:5, a)
*/
