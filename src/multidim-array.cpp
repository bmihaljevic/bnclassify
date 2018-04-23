#include <Rcpp.h>  
#include <multidim-array.h>

using namespace Rcpp;   

// [[Rcpp::export]]
int entry_index(const std::vector<int> & indices, const std::vector<int> & dim_prod) {
 return entry_index( indices.begin(), dim_prod); 
}    