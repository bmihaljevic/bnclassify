#include <Rcpp.h>
using namespace Rcpp;

// Returns a subset of the edges so that any reversed ones are removed.
// The last ones are kept, that is, those lower in the matrix 
// This should be  a DAG function; 
// [[Rcpp::export]]
LogicalVector find_non_reversed(CharacterMatrix x) {  
  int n = x.nrow();
  if (n == 0) {
    return LogicalVector(0); 
  } 
  std::vector<bool> unique(n, true);    
  // Skip last element in the loop, consider it unique
  for (int row = n - 2; row >= 0; row--) {
    const CharacterMatrix::Row & this_row = x(row, _);
     // a row is unique, if non of those before is identical to it
     // in the reversed matrix 
    int j = row;
    while (j < n - 1) {
      j++;
       // no need to check for those known to be duplicates, as that would be repeating
      if (unique.at(j)) { 
        const CharacterMatrix::Row & reversed_row = x(j, _);
        if (reversed_row[0]  == this_row[1] && reversed_row[1] == this_row[0]) {
          unique.at(row) = false ;
          // j <- n
          break;
        } 
      }
    }
  }
  return wrap(unique);
}