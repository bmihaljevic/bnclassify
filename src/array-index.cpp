#include <Rcpp.h>
using namespace Rcpp;

// TODO: maybe have an array class. Yet, it is possibly all in Eigen or Armadillo already.


// [[Rcpp::export]]
int entry_index(const std::vector<int> & indices, const std::vector<int> & dim_prod) {
  // TODO: inddices and dim prod same length. length > 0. entries positive (1-based indices.) 
// TODO: faster with iterators? Try a second version of the function.
// use variable as compiler does not cache the size 
 int index = indices.at(0);
  // -1 because indices are 1-based.
 int sum = index - 1;
 int ndb_inds = indices.size();
 for (int k = 1; k < ndb_inds ; k++) {
   int index = indices.at(k);
   index = index - 1;  
   sum += index * dim_prod.at(k - 1);
 }
 return sum; 
} 


/*** R
# todo: move to test.
test_ind <- function() {
  samp <-  function(n) {
    sample(1:n, size = 1)
  } 
  dim <- c(samp(10), samp(10) , samp(10) )
  index <- c(samp(dim[1]), samp(dim[2]), 1)
  ind <- entry_index(index, dim)
  target <- arrayInd(ind + 1, dim)
  # print(all(index == target))
  stopifnot(all(index == target))
}
test_ind()
for (i in 1:1e4 ) {
 test_ind()
}
a <- replicate( 1e3, test_ind)
*/
