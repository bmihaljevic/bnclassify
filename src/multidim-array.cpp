#include <Rcpp.h>  
#include <multidim-array.h>

using namespace Rcpp;

 // int index = indices.at(0);
 //  // -1 because indices are 1-based.
 // int sum = index - 1;
 // int ndb_inds = indices.size();
 // for (int k = 1; k < ndb_inds ; k++) {
 //   int index = indices.at(k);
 //   index = index - 1;  
 //   sum += index * dim_prod.at(k - 1);
 // }
 // return sum; 

 /** 
  * It returns the index for fully specified entries.
  * TODO: the end is completely unused.
  */
int entry_index(std::vector<int>::const_iterator begin, std::vector<int>::const_iterator end, const std::vector<int> & dim_prod) {
  // TODO: inddices and dim prod same length. length > 0. entries positive (1-based indices.) 
// TODO: faster with iterators? Try a second version of the function.
// use variable as compiler does not cache the size 
 int index = *begin;
  // -1 because indices are 1-based.
 int sum = index - 1;
 int ndb_inds = dim_prod.size();
 for (int k = 1; k < ndb_inds ; k++) {
   int index = *(begin + k);
   index = index - 1;  
   sum += index * dim_prod.at(k - 1);
 }
 return sum; 
} 

// [[Rcpp::export]]
int entry_index(const std::vector<int> & indices, const std::vector<int> & dim_prod) {
 return entry_index( indices.begin(), indices.end(), dim_prod); 
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
