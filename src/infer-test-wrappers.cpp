#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector test_dims2columns(const NumericVector cpt, const CharacterVector class_var,  const CharacterVector columns_db) {  
  const List & dimnames = cpt.attr("dimnames");
  const CharacterVector & fam = dimnames.attr("names");
  CharacterVector feature_fam = wrap(ordersetdiff(fam, class_var)); 
  IntegerVector feature_fam_inds = match(feature_fam, columns_db);
  if (is_true(any(feature_fam_inds == 0)))  stop("All features must be in the dataset.");
  feature_fam_inds = feature_fam_inds - 1; 
  return feature_fam_inds; 
}

/*** R
# a <- aode('class', car)  
# a <- lp(a, car, smooth = 1)
# cpt <- a$.models$persons$.params$buying
# colnames(car)  
# names(dimnames(cpt))
# test_dims2columns(cpt,"class", columns_db = colnames(car))
*/
