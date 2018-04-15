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
  // Model mod(x);
  // Evidence ds(df, mod.getFeatures()); 
  // MappedCPT c = MappedCPT(mod.get_cpt(cptind), mod.getClassVar(), ds);
  // std::vector<double> entries(mod.get_nclass());
  // c.get_entries(1, entries);
  // return wrap(entries);
  return NumericVector::create(1);
}   


//[[Rcpp::export]]
void make_cpt_object(const NumericVector & x) {
 CPT cpt(x); 
 NumericVector nv = wrap(cpt.get_entries());
 Rcout << nv << std::endl; 
 
 IntegerVector iv = wrap(cpt.get_dimprod());
 Rcout << iv << std::endl; 
}


/*** R
# a <- aode('class', car)  
# a <- lp(a, car, smooth = 1)
# cpt <- a$.models$persons$.params$buying
# colnames(car)  
# names(dimnames(cpt))
# test_dims2columns(cpt,"class", columns_db = colnames(car))

# microbenchmark::microbenchmark(  { g = do_mapped(t, dbor)} )
# microbenchmark::microbenchmark(    { d = get_row(t$.params$bkblk, f, class_var(t), dbor)  })
# microbenchmark::microbenchmark(    { d = get_row(t$.params$bkblk, f, class_var(t), dbor)  })

# simple_wrap <- function(x, dataset) {
#   if (!anyNA(dataset)) {
#     compute_joint(x, dataset)
#   }
# }  
# microbenchmark::microbenchmark( { f = compute_joint(t, dbor)},
#                                   { h  = simple_wrap(t, dbor)},
#                                 times = 1e3 )
*/

