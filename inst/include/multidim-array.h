#ifndef bnclassify_multidimarray_H
#define bnclassify_multidimarray_H

#include <Rcpp.h>
using namespace Rcpp;

// TODO: maybe have an array class. 
// int entry_index(std::vector<int>::const_iterator begin, const std::vector<int> & dim_prod);



 /** 
  * It returns the index for fully specified entries.
  * The entries must be 0-based.
  */
inline int entry_index(std::vector<int>::const_iterator begin, const std::vector<int> & dim_prod) {   
 return std::inner_product(begin + 1, begin + dim_prod.size(), dim_prod.begin(), *begin);
} 

int entry_index(const std::vector<int> & indices, const std::vector<int> & dim_prod);    

/**
* Returns a subset of the array, when the last dimension of indices is not specified.
*/
inline void subset_free_last_dim(const std::vector<double> & array, const std::vector<int> & dim_prod, std::vector<int>::iterator indices_begin, 
                     std::vector<double> & output) {
  // length indices = length dim prod minus one
  // dim prod cum prod = array length    
  // otuput size = dim size
  int ndim = dim_prod.size();
   std::vector<int>::iterator last_dim = indices_begin + ndim - 1;
  // Start with first value for last dimension.
    *last_dim = 0;
   int sum = entry_index(indices_begin, dim_prod);
   // // Add an entry per each class
   int per_class_entries   = dim_prod.at(ndim - 2);
  int ncpts = output.size();
  // most of the time is spent in this loop.
   for (int i = 0; i < ncpts; i++ ) {
     output[i] =  array[sum];
     sum += per_class_entries; 
   }  
}


#endif