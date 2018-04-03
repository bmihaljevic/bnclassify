#include <Rcpp.h>
using namespace Rcpp;

#include <Rcpp.h>

// Save on the typing... 
typedef std::pair<double, int>  ptype; 

// A comparison function to rank values in descending order
bool compare_values(const ptype &p1, const ptype &p2)
{
  return p1.second > p2.second;
}

// Get the top number of observations
// [[Rcpp::export]]
Rcpp::IntegerVector table_cpp(const Rcpp::IntegerVector & v)
{ 
  if(!Rf_isFactor(v)) stop("Not a factor."); 
  CharacterVector factorLevels = v.attr("levels"); 
  std::vector<unsigned int> table(factorLevels.size());   

  std::size_t n =  v.size();
  for (int i = 0; i != n; ++i) { 
    table[ v[i] - 1 ] ++;
  }   
  
  IntegerVector rcpptable = wrap(table); 
  rcpptable.names() = factorLevels;
  return rcpptable;  
}   

Rcpp::IntegerVector tabulate(const Rcpp::IntegerVector & v, int levels) { 
  std::vector<unsigned int> table(levels);   
  std::size_t n =  v.size();
  for (int i = 0; i != n; ++i) { 
    table[ v[i] - 1 ] ++;
  }    
  return wrap(table); 
}

// [[Rcpp::export]]
Rcpp::IntegerVector unidim_values(const DataFrame & data) {   
  const IntegerVector & column = data.at(0);
  unsigned int nrow = column.size(); 
  IntegerVector bin = no_init(nrow );
  bin.fill(1);
  unsigned int pd = 1;
  int ncols = data.size(); 
  IntegerVector  dims(ncols);
  List  dimnames(ncols); 
  dimnames.names() = data.names();
  
  for (int i = 0; i < ncols; i++) {
    const IntegerVector & a = data.at(i);  
    if(!Rf_isFactor(a)) stop("Not a factor."); 
    const CharacterVector & factorLevels = a.attr("levels"); 
    int nl = factorLevels.size();
    bin = bin + pd * (a - 1L);
    pd = pd * nl ; 
    dims.at(i) = nl; 
    dimnames.at(i) = factorLevels;
  }    
  
  IntegerVector tbl = tabulate(bin, pd);
  tbl.attr("dim") =  dims;
  tbl.attr("dimnames") =  dimnames;
  tbl.attr("class") =  "table";
  return tbl;
}

// Rcpp::IntegerVector unidim_table(const DataFrame & columns) {
//   dn <- c(dn, list(ll))
//   bin <- bin + pd * (a - 1L)
//   pd <- pd * nl
//   names(dn) <- dnn
//   bin <- bin[!is.na(bin)]
//   if (length(bin)) 
//     bin <- bin + 1L 
// }

// dataframe {
//   for each column get the num of dims.tfm 
//   get size of the resulting talbe 
//   finally tabulate by the bins meaning how many are there 
//   each value will correspond to its index in the dim array.
//  so, again, this is just indexing by a set of values, then you go to there and find it. 
// }

/*** R
mktbl <- function(data) {  
  bin <- 0L
  lens <- NULL
  dims <- integer()
  pd <- 1L
  dn <- NULL
  for (i in seq_along(data)) {
    a <- data[[i]]
    ll <- levels(a) 
    nl <- length(ll)
    # dims <- c(dims, nl)
    # if (prod(dims) > .Machine$integer.max) 
    #   stop("attempt to make a table with >= 2^31 elements")
    # dn <- c(dn, list(ll))
    a <- as.integer(a)
    bin <- bin + pd * (a - 1L)
    pd <- pd * nl 
  }
  bin <- bin[!is.na(bin)]
  if (length(bin)) 
    bin <- bin + 1L
  bin
}
tbl <- table(dbor[, 1:3])  
bin <- mktbl(dbor[, 1:3]) 
y <- array(tabulate(bin, 8), dim(tbl), dimnames = dimnames(tbl))
y
tbl
all.equal(y, tbl)

a <- unidim_values(dbor[, 1:3])
a  

set.seed(223)
(a = sample(0:1, 1e5, replace = T))
a <- factor(a)
bin <- table_cpp(a) 
bin
microbenchmark::microbenchmark(table_cpp(a), table(a))
microbenchmark::microbenchmark(table_cpp(a), table(a))

a <- unidim_values(dbor[, 1:3])
b <- table(dbor[, 1:3])
all.equal(a, b)  

tbl <- table(dbor[, 1:3])  

fd <- dbor[, 1:3]
microbenchmark::microbenchmark( a <- unidim_values(fd), tbl <- table(fd)   ) 

*/
