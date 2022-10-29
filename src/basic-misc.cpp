#include <Rcpp.h>
#include <basic-misc.h>

using namespace Rcpp;  

// [[Rcpp::export]]
std::vector<int> match_zero_based(const CharacterVector & subset, const CharacterVector & superset, const std::string error_message) {
  IntegerVector subset_inds = Rcpp::match(subset, superset); 
  if (subset_inds.size() == 0) throw std::logic_error("No class");
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
}

// [[Rcpp::export]]
bool are_disjoint(Rcpp::Nullable<Rcpp::CharacterVector> x, Rcpp::Nullable<Rcpp::CharacterVector> y) {
  if (x.isNotNull() && y.isNotNull()) {
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

// Note: changes original vector!! Hence sideeffect postfix
// [[Rcpp::export]]
CharacterVector make_last_sideeffect(CharacterVector & x, const CharacterVector & last) {
// OK if the vector is emtpy, because the parents of class are empty.
    if (x.size() == 0) return x;
    if (!(last.size() == 1)) stop("last should be a single string.");
// #   Just asume there is no more than a a single last in x 
    IntegerVector first_match = match(last, x); 
    // CharacterVector::iterator fmatch = std::find(x.begin(), x.end(), l);
    // int ind  =  first_match.at(0) - 1;
    // Rcout << " ind " << ind << std::endl;
    // if (ind < 0) stop("last not found.");
    // CharacterVector::iterator fmatch  = x.begin() + ind;   
    CharacterVector::iterator fmatch  = x.begin() + first_match.at(0) - 1;  
    if (fmatch < x.begin() || fmatch > x.end()) stop("last not found.");
    // if (fmatch == x.end()) stop("last not found.");
    // std::iter_swap(fmatch, x.end() - 1); 
    // use rotate to preserve previous behaviour
    std::rotate(fmatch, fmatch + 1, x.end());
    return x;
} 

// Normalizes the contigency table on the first dimension. Returns a table.
// It modifes its argument and returns it. 
// Todo: ensure is is numeric; not integer, otherwise it won't be modified!!  
// TOOD: move to array
// [[Rcpp::export]]
NumericVector normalize_ctgt(NumericVector & ctgt) {
  if (is_true(any(is_na(ctgt)))) stop("NAs in contigency table."); 
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

// [[Rcpp::export]]
int count_equal(const RObject & a, const RObject & b) {
    if(!Rf_isFactor(a)) stop("Not a factor."); 
    if(!Rf_isFactor(b)) stop("Not a factor.");   
    IntegerVector avec = as<IntegerVector>(a);
    IntegerVector bvec = as<IntegerVector>(b); 
    if (avec.size()  != bvec.size()) stop("Not same length.");
    int equal = 0;
    for (IntegerVector::const_iterator itA = avec.begin(), itB = bvec.begin(), endA = avec.end(); itA != endA; ++itA, ++itB) {
      if (*itA == *itB) {
        equal += 1;
      }
    }
    return equal;
}


// [[Rcpp::export]]
bool in_rcpp(CharacterVector element, CharacterVector vector) {
  // How to ensure only CharacterVector are passed?
   if (element.size() != 1) stop("Must be a single element.");
   return is_true(Rcpp::any(Rcpp::in(element, vector)));
}


// TODO: move to test!!   
/***R     
in_rcpp('a', letters)

count_equal(car$class, car$class)
count_equal(car$class, rev(car$class))
count_equal(car$class[1], car$class[nrow(car)])

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