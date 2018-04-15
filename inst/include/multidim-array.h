#include <Rcpp.h>
using namespace Rcpp;

// TODO: maybe have an array class. Yet, it is possibly all in Eigen or Armadillo already.    
int entry_index(std::vector<int>::const_iterator begin, std::vector<int>::const_iterator end, const std::vector<int> & dim_prod);
int entry_index(const std::vector<int> & indices, const std::vector<int> & dim_prod);   