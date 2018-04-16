#ifndef bnclassify_multidimarray_H
#define bnclassify_multidimarray_H

#include <Rcpp.h>
using namespace Rcpp;

// TODO: maybe have an array class. Yet, it is possibly all in Eigen or Armadillo already.    
// int entry_index(std::vector<int>::const_iterator begin, const std::vector<int> & dim_prod);

// /**
//  * Returns a subset of the array, when the last dimension of indices is not specified.
//  */
// subset_last_dim(const std::vector<double> & array, const std::vector<int> & dim_prod, const std::vector<int> & indices) {
//   // length indices = length dim prod minus one 
//   // dim prod cum prod = array length
//   
// }

 /** 
  * It returns the index for fully specified entries.
  */
inline int entry_index(std::vector<int>::const_iterator begin, const std::vector<int> & dim_prod) {
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

int entry_index(const std::vector<int> & indices, const std::vector<int> & dim_prod);   


#endif