#include <Rcpp.h>
// [[Rcpp::depends(BH)]]

#include <boost/config.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/connected_components.hpp>
#include <boost/graph/directed_graph.hpp>

/**
 * Basic adjacency list object api
 * Data: 
 *  mapping from names to ids.
 *  Matrix of edges between 
 * adj_list {
 *    string_vec nodes : all the string nodes I need. 
 *    edge matrix: uses ids as entries 
 *   public 
 *    int vec ids(): 
 *    edgeMatrix(names=FALSE): uses ids as entries 
 *    edgeMatrix(names=FALSE): uses names as entries 
 *    print(): print nicely
 * }     
 */

/**
 * Remember that I cannot have the boost object in R. Thus I need to go back to R from boost. 
 * And the going back is either a matrix or an adj list
 */

// TODO!!!!!!!!!!!! Not all vertices are in edges! Always add vertices!!!!!!!!!!!!!!!!! 

// TODO!!!!!!!!!!!! I only need to call boost if I add types or not. 
// ************** type bnc_dag_internal. **************  
// So, my internal is just by 

using namespace boost; 
using namespace Rcpp;    

/**
 * TODO: implement the interfac of dag.r. 
 */   

/**
 * I wanted to use names as IDs but in bgl it is not possible. Those will have to be 
 */

typedef adjacency_list<vecS, vecS, directedS> Graph;
// typedef boost::directed_graph<> Graph;
// this one did not work:
// typedef adjacency_list<boost::directedS> Graph;

void print_vertices(Graph g) {
  typedef graph_traits<Graph>::vertex_iterator vertex_iterator;
  std::pair<vertex_iterator, vertex_iterator> viter;
  for (viter = vertices(g); viter.first != viter.second; ++viter.first) {
    Rcout << *viter.first;
  }
  Rcout << std::endl;
}


/**
 * Will do this first instead of a generic adj list -> graph. Will do that if required.
 */
// [[Rcpp::export]]
void make_graph(std::vector<std::string> vertices, Rcpp::IntegerMatrix edges)
{
  // Initialize empty graph 
  // Add any vertices
  // Add any edges 
  // Return graph. This is an internal function.  
  // 
  int n = vertices.size();
  Graph g(n);
  
  typedef std::pair<int,int> Pair;
  int nedges = edges.nrow();
  std::vector<Pair> pairs;
  pairs.reserve(nedges);
  for (int i = 0; i < nedges; i++) { 
    pairs.push_back(Pair(0,1));    
    add_edge(0, 1, g);
  } 

  print_vertices(g); 
  // typedef property<first_name_t, std::string> FirstNameProperty;
  // typedef adjacency_list<vecS, vecS, directedS,
  //                        FirstNameProperty> MyGraphType;
}


// [[Rcpp::export]]
void make_nb2(std::string class_var, std::vector<std::string> features) { 
  // typedef property<first_name_t, std::string> FirstNameProperty;
  typedef adjacency_list<vecS, vecS, directedS> MyGraphType;
  
  typedef std::pair<int,int> Pair;
  Pair edge_array[11] = { Pair(0,1), Pair(0,2), Pair(0,3), 
                          Pair(0,4), Pair(2,0), Pair(3,0), 
                          Pair(2,4), Pair(3,1), Pair(3,4), 
                          Pair(4,0), Pair(4,1) };
  
  MyGraphType G(5);
  for (int i = 0; i < 11; ++i)
    add_edge(edge_array[i].first, edge_array[i].second, G);   
}

// [[Rcpp::export]]
NumericVector bh_connected_comp(NumericVector x) 
{
  typedef adjacency_list <vecS, vecS, undirectedS> Graph;
  
  Graph G;
  add_edge(0, 1, G);
  add_edge(1, 4, G);
  add_edge(4, 0, G);
  add_edge(2, 5, G);
  
  std::vector<int> component(num_vertices(G));
  int num = connected_components(G, &component[0]); 
  std::vector<int>::size_type i;
  Rcout << "Total number of components: " << num << std::endl;  
  for (i = 0; i != component.size(); ++i)
    Rcout << "Vertex " << i <<" is in component " << component[i] << std::endl;
  Rcout << std::endl;
  
 return wrap(component);
}    
  
/*** R
make_nb('a', letters[1:5])
# For connected:  split(0:5, a)
*/
