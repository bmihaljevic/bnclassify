#include <Rcpp.h>
using namespace Rcpp;    

// [[Rcpp::export]]  
Rcpp::IntegerVector tabulate_cpp(const Rcpp::IntegerVector & v, R_xlen_t nlevels) {
  // Using R_xlen_t to avoid checking for entries < 0
  std::vector<R_xlen_t> table(nlevels);   
  R_xlen_t n =  v.size();
  for (R_xlen_t i = 0; i < n; ++i) { 
    table.at( v.at(i) - 1 ) ++;
  }    
  // Wrapp may throw errors with R_xlen_t
  // return wrap(table); 
  IntegerVector  a(table.size());
  std::copy(table.begin(), table.end(), a.begin());
  return a;
}

// Based on table()  
// dataframe {
//   for each column get the num of dims.tfm 
//   get size of the resulting talbe 
//   finally tabulate by the bins meaning how many are there 
//   each value will correspond to its index in the dim array.
//   this is just indexing by a set of values, then you go to there and find it. 
// }
// [[Rcpp::export]]
Rcpp::IntegerVector table_cpp(const RObject & input, const RObject & columns) { 
  if(!is<DataFrame>(input)) stop("Must be a data frame.");
  DataFrame data = as<DataFrame>(input);  
  if(!is<CharacterVector>(columns)) stop("Must be character vector."); 
  CharacterVector cols = as<CharacterVector>(columns);  
  data = data[cols];
  
  const R_xlen_t ncols = data.ncol();  
  if (ncols == 0) stop("No columns in data frame.");  
  const IntegerVector & column = data.at(0);
  // There is a single entry for each row
  IntegerVector to_tabulate = no_init(column.size());
  to_tabulate.fill(1);
  // The product of dimensions.
  R_xlen_t pd = 1;
  IntegerVector  dims(ncols);
  List  dimnames(ncols);  
  dimnames.names() = data.names();
  
  for (R_xlen_t i = 0; i < ncols; i++) {
    const IntegerVector & a = data.at(i);  
    if(!Rf_isFactor(a)) stop("Not a factor."); 
    const CharacterVector & factorLevels = a.attr("levels"); 
    R_xlen_t nl = factorLevels.size();
    to_tabulate = to_tabulate + pd * (a - 1L);
    pd = pd * nl ; 
    dims.at(i) = nl; 
    dimnames.at(i) = factorLevels;
  }    
  
  to_tabulate = na_omit(to_tabulate);
  IntegerVector tbl = tabulate_cpp(to_tabulate, pd);
  tbl.attr("dim") =  dims;
  tbl.attr("dimnames") =  dimnames;
  tbl.attr("class") =  "table";
  
  return tbl;
}


/*** R 
kr <- foreign::read.arff('~/gd/phd/code/works-aug-semi-bayes/data/original/kr-vs-kp.arff')
dbor <- kr
fd <- dbor[, 1:3]
microbenchmark::microbenchmark( a <- table_cpp(fd), tbl <- table(fd)   )
*/
