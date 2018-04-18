#include <Rcpp.h>
// [[Rcpp::depends(BH)]]

#include <boost/config.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/connected_components.hpp>
#include <boost/graph/directed_graph.hpp>
#include <boost/graph/subgraph.hpp>
#include <boost/graph/graph_utility.hpp> 
#include <boost/property_map/property_map.hpp>

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
typedef subgraph< adjacency_list< vecS, vecS, directedS, VertexProperty, EdgeProperty > > dgraph;   
// for connected components and such
typedef adjacency_list <vecS, vecS, undirectedS> ugraph; 

/**
 * Will return a list with nodes and edges. Does not have the names though unless I save them somewhere.
 * If I did not pass the subgraph by reference here, it would not print.
 */
Rcpp::List graph2R(dgraph & g) {
  typedef graph_traits<dgraph>::vertex_descriptor Vertex; 
  typedef property_map<dgraph, vertex_index_t>::type IndexMap;
  typedef property_map<dgraph, vertex_name_t>::type NameMap;
  IndexMap index = get(vertex_index, g);
  NameMap names = get(vertex_name, g);
  
  typedef graph_traits<dgraph>::vertex_iterator vertex_iter;
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
  
  typedef graph_traits<dgraph>::edge_descriptor edge; 
  typedef graph_traits<dgraph>::edge_iterator edge_iter;
  std::pair<edge_iter, edge_iter> ep; 
  int nedges = num_edges(g);
  Rcpp::IntegerMatrix edges_matrix(nedges, 2);
  Vertex u, v;
  int row  = 0;
  for (ep = edges(g); ep.first != ep.second; ++ep.first) {
    edge e = *ep.first;
    u = source(*ep.first,g);
    v = target(*ep.first,g);
    row++;
    edges_matrix(row, 1) = u;
    edges_matrix(row, 2) = v;
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

// [[Rcpp::export]]  
void test_make(CharacterVector vertices, Rcpp::IntegerMatrix edges) {
  dgraph g  = bh_make_graph(vertices,  edges);
  // print_vertices(g); 
}      

// Requires an undirected graph   
// [[Rcpp::export]]  
NumericVector bh_connected_components(CharacterVector vertices, Rcpp::IntegerMatrix edges) { 
  // ugraph g  = bh_make_graph<ugraph>(vertices,  edges);
  // // print_vertices(g);
  // std::vector<int> component(num_vertices(g));
  // int num = connected_components(g, &component[0]); 
  // // TODO:: see additional checks from RBGL. Maybe connected comp might fail?
  // // std::vector<int>::size_type i;   
  // return wrap(component);  
  return NumericVector::create(1); 
}      

dgraph  make_subgraph(dgraph & g, const CharacterVector & subgraph_vertices, const CharacterVector & vertices)  { 
  dgraph subgraph = g.create_subgraph(); 
//  If you add particular vertices from global, are they kept?
  std::vector<int> sgraph_vertices = match_zero_based(subgraph_vertices, vertices);
  for (int i = 0; i < sgraph_vertices.size(); i++) {
    int a = add_vertex(sgraph_vertices.at(i), subgraph); 
  }
  return subgraph;  
}

// [[Rcpp::export]]  
Rcpp::List bh_subgraph(const CharacterVector & subgraph_vertices, const CharacterVector & vertices, const Rcpp::IntegerMatrix & edges) {
  dgraph g  = bh_make_graph(vertices,  edges); 
  print_graph(g);
  dgraph subgraph = make_subgraph (g, subgraph_vertices, vertices) ;   
  // print_graph(subgraph); 
  return graph2R(subgraph );
} 

  
/*** R
dag <- anb_make_nb('a', letters[2:6])
test_make(dag$nodes, dag$edges)
bh_connected_components(dag$nodes, dag$edges) 
bh_subgraph(dag$nodes, dag$nodes, dag$edges)
bh_subgraph('Bojan', dag$nodes, dag$edges)
*/
