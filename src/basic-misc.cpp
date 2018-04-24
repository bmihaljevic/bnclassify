#include <Rcpp.h>
#include <basic-misc.h>

using namespace Rcpp;

/**
 * Maps the features to columns in the data frame.
 * The indices are 0-based.
 * TODO: probably should receive evidence or data frame as input, not columns_db character vector. Or it should be a vector of strings.
 * TODO: make this comments generic. do not refer to columns.
 */ 
// [[Rcpp::export]]
std::vector<int> match_zero_based(const std::vector<std::string> & features, const CharacterVector & columns_db) { 
  CharacterVector feature_fam = wrap(features); 
  IntegerVector feature_fam_inds = match(feature_fam, columns_db);
  if (is_true(any(feature_fam_inds == 0)))  stop("All features must be in the dataset.");
  feature_fam_inds = feature_fam_inds - 1; 
  return as<std::vector<int> > (feature_fam_inds);
} 

// TODO: move this to basic-misc one moved to a header
// TODO: R match was returning -2147483648 when not finding the value, and the any() test was failing. 
// Thus, avoid Rcpp for the test 
// [[Rcpp::export]]
std::vector<int> match_zero_based2(const CharacterVector & subset, const CharacterVector & superset, const std::string error_message) { 
  IntegerVector subset_inds = Rcpp::match(subset, superset); 
  int min = *std::min_element(subset_inds.begin(), subset_inds.end());
  if (min <= 0)  stop(error_message);
  subset_inds = subset_inds - 1; 
  return as<std::vector<int> > (subset_inds);
}     


// [[Rcpp::export]]
std::vector<std::string> ordersetdiff(CharacterVector vector, CharacterVector remove) {
  std::vector<std::string> vec = as<std::vector<std::string> >(vector);
  std::string move = as<std::string>(remove);
  std::vector<std::string>::iterator index = std::find(vec.begin(), vec.end(), move);
  vec.erase(index);
  return vec;
}  

bool safediff(unsigned int x, int y) {
  return (y >= 0) && (x != static_cast<unsigned int>(y));
};

// TODO: This should be called at instance level, not data frame! This way, if the data set is complete, it goes through it a couple of times.
// [[Rcpp::export]]
bool hasna(const DataFrame & newdata) {  
  for (int i = 0; i < newdata.size(); i++) { 
   const IntegerVector & vec = newdata.at(i);
   if (is_true(any(is_na(vec)))) return true;  
  }  
  return false;
} 

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