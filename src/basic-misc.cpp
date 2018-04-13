#include <basic-misc.h>
using namespace Rcpp;  


// [[Rcpp::export]]
bool are_disjoint(Rcpp::Nullable<Rcpp::CharacterVector> x, Rcpp::Nullable<Rcpp::CharacterVector> y) {
  if (x.isNotNull() & y.isNotNull()) {
    Rcpp::CharacterVector xx(x);
    Rcpp::CharacterVector yy(y);
    Rcpp::LogicalVector init = in(xx, yy)  ;
    return  !is_true(any(init)); 
  }
  return true;
}

// Normalizes a vector or a segment. If division by the sum is Nan then returns a uniform distribution. 
// Not checking the input for NAs for speed. Caller must do that. 
void normalize(NumericVector::iterator begin, NumericVector::iterator end) {
  double sum = std::accumulate(begin, end, 0.0);
  for (NumericVector::iterator iter = begin; iter != end; iter++) {
    (*iter) = (*iter) / sum; 
  } 
  // # check the quotient is NaN since it is unclear which tolerance to use if checking
  // # sum(x) == 0
  NumericVector n = NumericVector(begin, end);
  if (is_true(any(is_nan(n)))) {
    if (! is_true(all(is_nan(n)))) stop("Unexpected.");
    for (NumericVector::iterator iter = begin; iter != end; iter++) {
      (*iter) = 1.0 / n.size();
    } 
  }
}
// [[Rcpp::export]]
NumericVector normalize(NumericVector & x) {
  normalize(x.begin(), x.end());
  return x;
}
// Normalizes the contigency table on the first dimension. Returns a table.
// It modifes its argument and returns it.
// [[Rcpp::export]]
NumericVector normalize_ctgt(NumericVector & ctgt) {
  if (is_true(any(is_na(ctgt)))) stop("NAs in contigency table."); // TODO: Should check for NaN
  // # Keep attributes (e.g., class and dimension names); just change entries
  NumericVector &  cpt = ctgt;
  NumericVector dim = ctgt.attr("dim");
  int ndims = dim.size();
  if (ndims == 1) {
    normalize(cpt.begin(), cpt.end());
  }
  else if (ndims > 1) {
    int first_dim = dim[0];
    int prod_rest_dims = std::accumulate(dim.begin() + 1, dim.end(), 1, std::multiplies<int>()); 
    int offset = 0;
    int final = offset; 
    for (int i = 0; i < prod_rest_dims; i++ ) { 
      final += first_dim; 
      normalize(ctgt.begin() + offset, ctgt.begin() + final );
      offset = final;
    }
  }
  else {
    stop("0 dimension of contigency table");
  } 
  return cpt; 
}

// Todo: ensure is is numeric; not integer, otherwise it won't be modified!!  
/***R  
a <- c(0.0, 0.0)
dim(a) <- c(2)
normalize_ctgt(a) 
a

a <- c(0, 0)
dim(a) <- c(2)
normalize_ctgt(a) 
a

a <- c(1:5, 0.0)
dim(a) <- c(6)
normalize_ctgt(a)
a

a <- table(letters, letters)
a <- a + 1
normalize_ctgt(a)
a

a <- table(letters, letters)
a <- a + 1 
normalize_ctgt(a)
a <- a ^ 0.5  
normalize_ctgt(a)
a  

a <- table(letters, letters)
a <- a + 1 

microbenchmark::microbenchmark(normalize_ctgt(a), times = 1e3)
*/