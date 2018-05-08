#include <infer.h>
#include <data.h>

// [[Rcpp::plugins(cpp11)]] 

// =================================================
// TODO: this logic should also not be here. Should be check_cpt()
// Reduce the entries by 1, so they could serve as 0-based indices.
  // Do this or not?
// !!!! Does evidence change underlying data frame when filtering columns?????
// Remove trimdataset R funct
// I could also try using an Eigen row matrix for evidence to see if access is faster.
//    Becuase if it makes copies then it might be slow.
// TODO: why would I need ind class in model?
// TODO: check evidence levels match the cpts.
// =================================================

using namespace Rcpp;

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
 * CPT. Copy the CPTs and log them. 
 */
CPT::CPT(const Rcpp::NumericVector & cpt, const std::string & class_var) 
{ 
    const Rcpp::List & dimnames = cpt.attr("dimnames");
    const Rcpp::CharacterVector & fam = dimnames.attr("names"); 
    
    this -> variables = fam;  
    // TODO: this logic should also not be here. Should be check_cpt()
    if (!(variables[variables.size() - 1] == class_var)) Rcpp::stop("Class not last dimension in CPT."); 
    this -> features = this -> variables;
    this -> features.erase(variables.size() - 1); 
  
    // Copy and log entries 
    entries.resize(cpt.size());
    std::copy(cpt.begin(), cpt.end(),   entries.begin());      
    
    double (*dlog)(double) = &std::log;
    std::transform(entries.begin(), entries.end(), entries.begin(), dlog);  
    
    const Rcpp::IntegerVector & dim = cpt.attr("dim");
    Rcpp::IntegerVector dimprod = Rcpp::cumprod(dim); 
    this->dimprod = Rcpp::as<std::vector <int> >(dimprod);
}   
/**
 * Model.
 */
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
  this->nclass = classes.size();
  // TODO: names(t$.families)  
  
  this->cpts.reserve(all_cpts.size()); 
  for (int i = 0; i < all_cpts.size(); i++) {
   const NumericVector & cpt = all_cpts.at(i); 
   this->cpts.push_back(CPT(cpt,  cpp_class_var));
  }        
  
  // get index of class in all cpts
  std::vector<int> inds = match_zero_based(class_var, vars_model, "Class CPT missing.");
  this->ind_class = inds.at(0); 
}                  
/**
 * Makes a copy of the input dataset and reduced entries by 1. 
 */
Evidence::Evidence(Rcpp::DataFrame & test, const Rcpp::CharacterVector & features) 
{ 
     // Only checks for NAs in the relevant columns, not in all of them. 
     if (hasna_features(test, features)) Rcpp::stop("NA entries in data set.");
     test = trim_dataset_cpp(test, features);  
     this->columns = test.names();  
     this->N = test.nrow(); 
     
     this->data_columns = Rcpp::as<std::vector<IntegerVector > > (test); 
}  
/**
 * MappedCPT.
 */  
MappedCPT::MappedCPT(const CPT & cpt, const Evidence & test) :
                    cpt(cpt), test(test) 
{  
  Rcpp::CharacterVector columns_db = test.getColumns();
  // this is because class in unobserved. if we have more unobserved, it would need a different procedure.
  this->db_indices = match_zero_based(cpt.get_features(), columns_db, "Some features missing from the dataset."); 
}     
/**
 * MappedModel. It holds the instance_buffer and the output_buffer, which are filled during computation.
 */  
MappedModel::MappedModel(const Model & x, const Evidence & evidence): 
  model(x),  class_cpt(x.getClassCPT().get_entries()), nclass(x.get_nclass()), n(x.get_n()), evidence(evidence) 
{ 
    const std::size_t n = x.get_n();
    cpts.reserve(n);  
    for (unsigned int i = 0; i < n; i++) {
      MappedCPT c(x.get_cpt(i), evidence);
      // With C++11 this moves, does not copy
      cpts.push_back(c);
    }   
    output_buffer.resize(nclass); 
    // We need n + 1 for a dummy class variable value at the last position of a of CPT with n + 1 dimensions.
    // It is a hack and would be best to do it another way
    instance_buffer.resize(n + 1);
}       
/**
 * Computes the log of the joint for complete data.
 */
NumericMatrix MappedModel::predict() 
{ 
 int N = evidence.getN();
 NumericMatrix output(N, nclass);
 
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

  const CharacterVector classes = model.get_classes(); 
  colnames(output) = classes;
  return output;   
}  
/**
 * Computes the log of the joint for complete data.
 */
// [[Rcpp::export]]
NumericMatrix compute_joint(List x, DataFrame newdata) {
 Model mod(x); 
 Evidence test(newdata, mod.getFeatures());
 MappedModel model(mod, test);
 return model.predict(); 
}     