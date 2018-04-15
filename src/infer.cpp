#include <infer.h>

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;
using Eigen::MatrixXd;      

// Debug stuff
void printv(std::vector<double> v) { 
 Rcpp::NumericVector nv = Rcpp::wrap(v);
 Rcpp::Rcout << nv << "." << std::endl; 
}

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

// TODO: probably remove this and use some code in R
// TODO: or this should just do search but know nothing about classes and such
int get_class_index(const CharacterVector & class_var, const List & cpts) { 
  const CharacterVector & vars_model = cpts.names(); 
  IntegerVector index = match(class_var, vars_model );
  if (index.size() != 1) stop("Class CPT missing.");
  return as<int> (index) ;
}      

/**
 * Wraps a bnc fit model.  Holds copies of its MappedCPTs.
 * Takes log of MappedCPTs. 
 * Provide features, class name. 
 */
Model::Model(List x)  { 
// Maybe just check this is a bnc_fit. All other logic kept in already available checks. just calls back to R code; no need for re-implementing things. 
// TODO: I should not hold a pointer to the underlying MappedCPTs. Just the logged copies.  
  // makes a copy of cpts
  // Log after the copies. The class should point to that copy and never have access to original.
  // gets list of features.
  // get the class name. 
  
  // TODO: call class()
  this->class_var = x[".class"];
  // TODO: use just C++ types I guess
  std::string cpp_class_var = as<std::string >(this ->class_var);
  this->features  = call_model_fun(x, "features");
  this->classes = call_model_fun(x, "classes");
  this->nclass = classes.size();
  
  // TODO: call function. params()
  Rcpp::List all_cpts = x[".params"]; 
  this->cpts.reserve(all_cpts.size()); 
  for (int i = 0; i < all_cpts.size(); i++) {
   const NumericVector & cpt = all_cpts.at(i); 
   this->cpts.push_back(CPT(cpt,  cpp_class_var));
  }        
  
  // get index of class in all cpts
  // take class cpt from log, not from original ones 
  // this-> class_cpt = log cpts [] 
  this->ind_class = get_class_index(class_var, all_cpts);
  // The above is a 1-based index. Fix it.  
  this->ind_class = this->ind_class  - 1;
}                

// [[Rcpp::export(rng=false)]]
NumericMatrix compute_joint(List x, DataFrame newdata) {
 Model mod(x);
 Evidence test(newdata, mod.getFeatures());
 MappedModel model(mod, test);
 int N = test.getN();
 int n = mod.get_n();
 int nclass = mod.get_nclass();
 MatrixXd output(N, nclass); 
 const std::vector<double> & class_cpt = mod.getClassCPT().get_entries();
 std::vector<double> per_class_cpt_entries(nclass);
 std::vector<int> instance_cpt_inds(n);
 for (int instance_ind = 0; instance_ind  < N ; instance_ind++) {
    // initialize output with log class prior 
     for (int theta_ind = 0; theta_ind < nclass; theta_ind++) { 
       output(instance_ind, theta_ind) = class_cpt[theta_ind];
     }
     // add the entries for each feature:
     for (int j = 0; j < n; j++) { 
       // Get CPT indices from the instance: 
        std::vector<int>::iterator cpt_inds_end = model.get_mapped_cpt(j).fill_instance_indices(instance_ind, instance_cpt_inds.begin());
        model.get_mapped_cpt(j).get_entries(instance_cpt_inds.begin(), cpt_inds_end, per_class_cpt_entries);
        for (int theta_ind = 0; theta_ind < nclass; theta_ind++) {
             output(instance_ind, theta_ind) += per_class_cpt_entries[theta_ind];
        }
     } // features
 } // instances
 NumericMatrix result = wrap(output);
 const CharacterVector classes = mod.get_classes(); 
 colnames(result) = classes;
 return result;  
}  


/*** R   
source('tests/infer-test.R', print.eval = TRUE)
*/

