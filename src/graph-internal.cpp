#include <Rcpp.h>
using namespace Rcpp;   

// [[Rcpp::export]]
LogicalVector graph_node_parents_inds(CharacterMatrix edges, CharacterVector node) { 
  if (node.size() != 1) stop("Must be a single element.");
  // TODO: access by column name!!! 
  const CharacterMatrix::Column & to = edges(_, 1);
  LogicalVector ind = to == node;   
  // const CharacterMatrix::Column & from = edges(_, 0);
  // return from(ind); 
  // unname(edges[ind, 'from'])
  return ind;
}

