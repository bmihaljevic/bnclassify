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

// TODO: probably remove this and use some code in R
// TODO: or this should just do search but know nothing about classes and such
int get_class_index(const CharacterVector & class_var, const CharacterVector & vars_model) { 
  IntegerVector index = match(class_var, vars_model );
  if (index.size() != 1) stop("Class CPT missing.");
  return as<int> (index) ;
}      

CPT::CPT(const Rcpp::NumericVector & cpt, const std::string & class_var) 
{ 
    const Rcpp::List & dimnames = cpt.attr("dimnames");
    const Rcpp::CharacterVector & fam = dimnames.attr("names"); 
    
    this -> variables = Rcpp::as< std::vector<std::string> >(fam);  
    if (!(variables[variables.size() - 1] == class_var)) Rcpp::stop("Class not last dimension in CPT."); 
    this -> features = this -> variables;
    this -> features.pop_back();
  
    // Copy and log entries 
    entries.resize(cpt.size());
    std::copy(cpt.begin(), cpt.end(),   entries.begin());      
    
    double (*dlog)(double) = &std::log;
    std::transform(entries.begin(), entries.end(), entries.begin(), dlog);  
    
    const Rcpp::IntegerVector & dim = cpt.attr("dim");
    Rcpp::IntegerVector dimprod = Rcpp::cumprod(dim); 
    this->dimprod = Rcpp::as<std::vector <int> >(dimprod);
}  
Model::Model(List x)  
{ 
  // TODO: call class()
  this->class_var = x[".class"];
  // TODO: use just C++ types I guess
  std::string cpp_class_var = as<std::string >(this ->class_var);
  // TODO: call features
  // this->features  = call_model_fun(x, "features"); 
  // TODO: call function. params()
  Rcpp::List all_cpts = x[".params"]; 
  const CharacterVector & vars_model = all_cpts.names(); 
  this->features =  ordersetdiff(vars_model, class_var); 
  // TODO: I can move those accessor functions to Rcpp, not a problem.
  const NumericVector & class_cpt =  all_cpts[cpp_class_var];  
  const Rcpp::List & dimnames = class_cpt.attr("dimnames");
  this->classes = dimnames.at(0);
  // this->classes = call_model_fun(x, "classes");
  // TODO: call R.
  // this->classes = call_model_fun(x, "classes");
  this->nclass = classes.size();
  // TODO: names(t$.families)  
  
  this->cpts.reserve(all_cpts.size()); 
  for (int i = 0; i < all_cpts.size(); i++) {
   const NumericVector & cpt = all_cpts.at(i); 
   this->cpts.push_back(CPT(cpt,  cpp_class_var));
  }        
  
  // get index of class in all cpts
  // take class cpt from log, not from original ones 
  // TODO: The above is a 1-based index. Fix it.  
  this->ind_class = get_class_index(class_var, vars_model);
  this->ind_class = this->ind_class  - 1;
}                 
Evidence::Evidence(Rcpp::DataFrame & test, const Rcpp::CharacterVector & features) 
{ 
     // I could also try using an Eigen row matrix to see if access is faster.
     const Rcpp::CharacterVector & vec = test.names();
     if (!is_true(all(in(features, vec)))) {
       Rcpp::stop("Some features missing from data set.");
     }
     // using intersect does not alter order of columns,  unlike test[features]. 
     Rcpp::CharacterVector keep = Rcpp::intersect(vec, features);
     test = test[keep];
     // TODO: remove this from here. Do it at instance level. 
     if (hasna(test)) Rcpp::stop("NA entries in data set.");  
     
     this->columns = test.names();  
     this->N = test.nrow();
     this->data = Rcpp::as<std::vector<std::vector<int> > > (test);   
  
    // Reduce the entries by 1, so they could serve as 0-based indices.
     for (int i = 0; i < data.size(); i++ ) {
       std::vector<int> & vec = data.at(i);
       for (std::vector<int>::iterator iter = vec.begin(); iter != vec.end(); iter ++ ) {
        (*iter)--;
       }
       // std::transform(vec.begin(), vec.end(), vec.begin(), std::bind(std::minus<int>(), 1));       
     } 
} 
MappedCPT::MappedCPT(const CPT & cpt, const Evidence & test) :
                    test(test), cpt(cpt) 
{  
  Rcpp::CharacterVector columns_db = test.getColumns();
  // this is because class in unobserved. if we have more unobserved, it would need a different procedure.
  this->db_indices = match_zero_based(cpt.get_features(), columns_db); 
}   
MappedModel::MappedModel(const Model & x, const Evidence & evidence): 
  model(x),  class_cpt(x.getClassCPT().get_entries()), nclass(x.get_nclass()), n(x.get_n()), evidence(evidence) 
{ 
    const std::size_t n = x.get_n();
    cpts.reserve(n);  
    for (unsigned int i = 0; i < n; i++) {
      MappedCPT c(x.get_cpt(i), evidence);
      // TODO: With C++11 this moves, does not copy
      cpts.push_back(c);
    }   
    output_buffer.resize(nclass); 
    instance_buffer.resize(n);
}       
NumericMatrix MappedModel::predict() 
{ 
 int N = evidence.getN();
 MatrixXd output(N, nclass);  
 
 for (int instance_ind = 0; instance_ind  < N ; instance_ind++) { 
    // initialize output with log class prior 
     for (int theta_ind = 0; theta_ind < nclass; theta_ind++) { 
       output(instance_ind, theta_ind) = class_cpt[theta_ind];
     }
     // add the entries for each feature:
     for (int j = 0; j < n; j++) { 
       // Get CPT indices from the instance:  
        fill_class_entries(instance_ind, j);
        for (int theta_ind = 0; theta_ind < nclass; theta_ind++) {
             output(instance_ind, theta_ind) += output_buffer[theta_ind];
        }
     } // features
  }// instances

  NumericMatrix result = wrap(output);
  const CharacterVector classes = model.get_classes(); 
  colnames(result) = classes;
  return result;   
} 
// [[Rcpp::export(rng=false)]]
NumericMatrix compute_joint(List x, DataFrame newdata) {
 Model mod(x); 
 Evidence test(newdata, mod.getFeatures());
 MappedModel model(mod, test);
 return model.predict(); 
}  


/*** R   
source('tests/infer-test.R', print.eval = TRUE)
*/  