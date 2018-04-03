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

// dataframe {
//   for each column get the num of dims.tfm 
//   get size of the resulting talbe 
//   finally tabulate by the bins meaning how many are there 
//   each value will correspond to its index in the dim array.
//  so, again, this is just indexing by a set of values, then you go to there and find it. 
// }

/*** R
set.seed(223)
(a = sample(0:1, 1e5, replace = T))
a <- factor(a)
table_cpp(a)
microbenchmark::microbenchmark(table_cpp(a), table(a))
*/
