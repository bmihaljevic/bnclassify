#include <Rcpp.h> 
#include <infer.h>

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

// Delete?
//[[Rcpp::export]]
NumericVector get_row(List x, DataFrame df, int cptind) { 
  Model mod(x);
  Evidence ds(df, mod.getFeatures()); 
  CPT c = CPT(mod.get_cpt(cptind), mod.getClassVar(), ds);
  std::vector<double> entries(mod.get_nclass());
  c.get_entries(1, entries);
  return wrap(entries);
}   


/*** R
# a <- aode('class', car)  
# a <- lp(a, car, smooth = 1)
# cpt <- a$.models$persons$.params$buying
# colnames(car)  
# names(dimnames(cpt))
# test_dims2columns(cpt,"class", columns_db = colnames(car))
*/
