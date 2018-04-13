#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int entry_index(const std::vector<double> & indices, const std::vector<double> & dim_prod) {
 int index = indices.at(0);
 int sum = index - 1;
 int ndb_inds = indices.size();
 for (int k = 1; k < ndb_inds ; k++) {
   int index = indices.at(k);
   index = index - 1;  // delete
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
*/
