#include <infer.h>

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;
using Eigen::MatrixXd;      

// A function with the bnc model ( a list) as only parameter and CharacterVector output
// [[Rcpp::export(rng=false)]]
Rcpp::CharacterVector call_model_fun(const Rcpp::List& x, const std::string funct) { 
   // Obtain environment containing function
   Rcpp::Environment base("package:bnclassify");  
   // Make function callable from C++
   Rcpp::Function features = base[funct];     
   // Call the function and receive its list output
   Rcpp::CharacterVector res = features(Rcpp::_["x"] = x);  
   return res;
}    




/**
 * Wraps a bnc fit model.  Holds copies of its MappedCPTs.
 * Takes log of MappedCPTs. 
 */
Model::Model(List x)  { 
// TODO: check model has basic bnc_fit properties. e.g., at least a class. 
// TODO: no big checks; just calls back to R code; no need for re-implementing things. 
// TODO: I should not hold a pointer to the underlying MappedCPTs. Just the logged copies.  
  // makes a copy of cpts, and logs them 
  // gets list of features.
  // get the class name. 
  // could simply achieve this by calling back to R. This is done just once.  
  // Log after the copies. The class should point to that copy and never have access to original.
  
  // TODO: call class()
  this->class_var = x[".class"];
  this->features  = call_model_fun(x, "features");
  CharacterVector classes = call_model_fun(x, "classes");
  this->nclass = classes.size();
  
  // TODO: call function. params()
  Rcpp::List all_cpts = x[".params"];
  this->log_cpts = std::vector<NumericVector>(); 
  this->log_cpts.reserve(all_cpts.size());
  for (int i = 0; i < all_cpts.size(); i++) {
    // a copy so that log does not modify original 
   const NumericVector & cpt = all_cpts.at(i);
   NumericVector cloned = clone(cpt);
   float (*flog)(float) = &std::log;
   std::transform(cloned.begin(), cloned.end(), cloned.begin(), flog);
   this->log_cpts.push_back(cloned);
  }        
  
  // get index of class in all cpts
  // take class cpt from log, not from original ones 
  // this-> class_cpt = log cpts [] 
}             
// needs not be a member function as it uses no members of MappedCPT 
// Get the DB indices of a family
// maps the cpt inds to the columns of the data set 
IntegerVector MappedCPT::dims2columns(const NumericVector cpt, const CharacterVector class_var,  const CharacterVector columns_db) { 
  const List & dimnames = cpt.attr("dimnames");
  const CharacterVector & fam = dimnames.attr("names"); 
  CharacterVector feature_fam = wrap(ordersetdiff(fam, class_var)); 
  IntegerVector feature_fam_inds = match(feature_fam, columns_db);
  if (is_true(any(feature_fam_inds == 0)))  stop("All features must be in the dataset.");
  feature_fam_inds = feature_fam_inds - 1; 
  if (safediff(feature_fam_inds.size(), this->dim_prod.size() - 1)) stop("Wrong cpt size.");
  return feature_fam_inds;
}

// Mapping of model to cpts
// check all features in data set. Well, I do not need class in data set.
// this is done by each cpt check
// make sure data levels and cpt levels match 
class MappedModel {
 const Model model;
  // no copies of the original cpts 
 std::vector<MappedCPT> cpts;   
 // class cpt is the only unmapped one 
 NumericVector class_cpt;
public:
  MappedModel(Model & x, Evidence & test): model(x) { 
    const std::size_t n = x.get_n();
    cpts.reserve(n);  
    for (unsigned int i = 0; i < n; i++) {
      NumericVector cpt = x.get_cpt(i);
      MappedCPT c(cpt, model.getClassVar(), test);
      // cpts.push_back(std::move(c));
      cpts.push_back(c);
    }
    // // TODO: this must be better done!!! And must work with more cases, etc.
    // This will be copied and logged above; all must be logged. 
    // But only features will go to  MappedCPT; class stays as numeric vector.
    // TODO:: This onw will not hold the class MappedCPT; only original model will. 
  //  This is just for evidence.
    this->class_cpt = x.get_cpt(n);
  }  
  inline MappedCPT& get_mapped_cpt(int i) {
    // TODO: change to []?
    return this->cpts.at(i);
  } 
  inline NumericVector& get_class_cpt() {
    return this->class_cpt;
  }
};     

// [[Rcpp::export(rng=false)]]
NumericMatrix compute_joint(List x, DataFrame newdata) {  
 Model mod(x);
 Evidence test(newdata, mod.getFeatures());
 MappedModel model(mod, test);
 int N = test.getN();
 int n = mod.get_n();
 int nclass = mod.get_nclass();
 // NumericMatrix output(N, nclass);
 MatrixXd output(N, nclass);
 NumericVector & class_cpt = model.get_class_cpt();
 // TODO: class cpt should be a std::vector.
 std::vector<int> instance(n);
 std::vector<double> per_class_cpt_entries(nclass);
 for (int instance_ind = 0; instance_ind  < N ; instance_ind++) {
    // initialize output with log class prior 
     for (int theta_ind = 0; theta_ind < nclass; theta_ind++) { 
       output(instance_ind, theta_ind) = class_cpt[theta_ind];
     }
     // add the entries for each feature:
     for (int j = 0; j < n; j++) { 
        model.get_mapped_cpt(j).get_entries(instance_ind, per_class_cpt_entries);
        for (int theta_ind = 0; theta_ind < nclass; theta_ind++) {
             output(instance_ind, theta_ind) += per_class_cpt_entries[theta_ind];
        }
     } // features
 } // instances
 NumericMatrix result = wrap(output);
 CharacterVector classes = class_cpt.names();
 colnames(result) = classes;
 return result;  
}  


/*** R   
source('tests/infer-test.R', print.eval = TRUE)
*/

