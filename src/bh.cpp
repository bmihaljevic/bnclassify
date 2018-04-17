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
typedef adjacency_list<vecS, vecS, directedS> Graph;
// for connected components and such
typedef adjacency_list <vecS, vecS, undirectedS> ugraph; 
// typedef boost::directed_graph<> Graph;
// this one did not work:
// typedef adjacency_list<boost::directedS> Graph;  

// TODO: Currently not sure what this is doing.
void print_vertices(Graph g) {
  typedef graph_traits<Graph>::vertex_iterator vertex_iterator;
  std::pair<vertex_iterator, vertex_iterator> viter;
  for (viter = vertices(g); viter.first != viter.second; ++viter.first) {
    Rcout << *viter.first << std::endl;
  }
  Rcout << std::endl;
} 

/**
 * This is an internal function.   
 * Since not not all vertices need to be in edges, add vertices separately.
 */
Graph make_graph(CharacterVector vertices, Rcpp::IntegerMatrix edges)
{
  // any checks?
  // Add vertices, if any 
  int n = vertices.size(); 
  Graph g(n);
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
  Graph g  = make_graph(vertices,  edges);
  print_vertices(g); 
}      

// Requires an undirected graph 
NumericVector bh_connected_comp(ugraph g) 
{   
  std::vector<int> component(num_vertices(g));
  int num = connected_components(g, &component[0]); 
  // TODO:: see additional checks from RBGL. Maybe connected comp might fail?
  std::vector<int>::size_type i;
  Rcout << "Total number of components: " << num << std::endl;  
  for (i = 0; i != component.size(); ++i)
    Rcout << "Vertex " << i <<" is in component " << component[i] << std::endl;
  Rcout << std::endl;
  
 return wrap(component);
}    


// NumericVector connected_components(CharacterVector vertices, Rcpp::IntegerMatrix edges) {
//   
// } 

// [[Rcpp::export]]
void test_connected() {
  ugraph g(3);
  add_edge(0, 1, g); 
  bh_connected_comp(g);
} 
  
/*** R
dag <- anb_make_nb('a', letters[2:6])
dag <- graph_internal2bgl(dag)
test_make(dag$nodes, dag$edges)
# For connected:  split(0:5, a)
*/
