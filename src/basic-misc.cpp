#include <Rcpp.h>
using namespace Rcpp;  

// [[Rcpp::export]]
bool are_disjoint(Nullable<CharacterVector> x, Nullable<CharacterVector> y) {
  if (x.isNotNull() & y.isNotNull()) {
    Rcpp::CharacterVector xx(x);
    Rcpp::CharacterVector yy(y);
    LogicalVector init = in(xx, yy)  ;
    return  !is_true(any(init)); 
  }
  return true;
}  