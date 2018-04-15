#ifndef bnclassify_infer_H
#define bnclassify_infer_H

#include <Rcpp.h>
#include <RcppEigen.h>
#include <cmath> 
#include <basic-misc.h>

// Debug stuff. TODO: remove.
void printv(std::vector<double> v); 


/**
 * All CPT internal logics and rules here. 
 * Users should not need to think of dimnames and similar, just of variables, features, etc.
 * It hold the log of original CPT entries.
 */
class CPT {
private:
    std::vector<std::string> variables; 
    std::vector<std::string> features; 
    std::vector<double> entries; 
    std::vector<int> dimprod; 
public: 
  // Do not store the class, though. Just the features.
  CPT(const Rcpp::NumericVector & cpt, const std::string & class_var) { 
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
  
  const std::vector<double> & get_entries() const {
    return  entries; 
  }
  
  // TODO: maybe remove this one
  const std::vector<std::string> & get_variables() const { 
    return variables; 
  } 
  
  const std::vector<std::string> & get_features() const { 
    return features; 
  } 
  
  
  const std::vector<int> & get_dimprod() const { 
    return dimprod; 
  } 
  
};


// Quaestions MODEL
//    Keep array dimnmes. 
//    Make it a std::vector because it is faster access to than  Rcpp
//    Copy and log 
//        Making a vector of std::vector would solve all the later, but would lose the dimension data. Howvever, I could keep the dim data apart in the MappedCPT.  
//        Thus, I do not want gRbase code, as it works on Rcpp

/**
 *  Encapsulates a TODO bnc? model. 
 */
class Model { 
  private:   
    Rcpp::CharacterVector features;
    Rcpp::CharacterVector class_var;  
    Rcpp::CharacterVector classes;  
    std::vector<Rcpp::NumericVector> log_cpts;
    std::vector<CPT> cpts;
    int nclass = -1;
    int ind_class = -1;
  public:
    Model(Rcpp::List model);    
    const CPT & get_cpt(int i) const {
      return this->cpts.at(i);
    }
    // TODO: const return and reference! 
    Rcpp::CharacterVector getFeatures() const {      
      return this->features;
    } 
    Rcpp::CharacterVector getClassVar() const {      
      return this->class_var;
    }    
    const Rcpp::CharacterVector & get_classes() const {      
      return this->classes;
    }   
    const CPT & getClassCPT() const {
      return this->cpts.at(ind_class);
    }  
    //n excludes the class    
    std::size_t get_n() const {
      return features.size();  
    }
    int get_nclass() const { 
      return nclass;
    } 
}; 


/**
 *  Holds the data with evidence for inference. 
 *  Current implementation holds a copy of the underlying data and thus only a single instance should exist per call to predict (no copies). 
 *  If the data are factors, then the values returned will correspond to 1-based indices for the MappedCPTs 
 *  
 * The indices for rows and columns are 0-based. 
 */
class Evidence {
  Rcpp::CharacterVector columns;
  // use a vector because I think it is faster to access than DataFrame
  // Currently, int allows only for factors, yet it could be generic.
  std::vector<std::vector<int> > data; 
  int N;   
public:
  inline double get(int i, int j) const {  
    return data.at(i).at(j); 
  }   
  inline Rcpp::CharacterVector getColumns() const {
   return  columns;
  } 
  inline int getN() const {
   return  N;
  }
  Evidence(Rcpp::DataFrame & test, const Rcpp::CharacterVector & features) {
     // TODO:  Check all are factors?
     // I could also reduce all entries - 1 and make a transpose, that is, a matrix that goes by instance and then iterate that way.
     // If I go to integer, I ought to store the levels of the cpts somewhere.
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
  }
}; 

class MappedCPT2 {
  // It was faster using c++ storage than Rcpp
  // Are the indices 1-based or one based?  I think 0 based because they are in Rcpp.
  std::vector<int> db_indices;
  const CPT & cpt;
  // A reference to a unique instance of Evidence
  const Evidence & test;
public: 
  MappedCPT2(const CPT & cpt, const Evidence & test) :
                    test(test), cpt(cpt) 
  {  
    Rcpp::CharacterVector columns_db = test.getColumns();
    // Comment: this is because class in unobserved. if we have more unobserved, it would need a different procedure.
    this->db_indices = match_zero_based(cpt.get_features(), columns_db); 
 }    
  
  /**
   * Fills in the instance's entries into the sequence. Returns iterator to one after last added.
   * The number of elements added is that of the dimensions of the CPT. It adds them into the output sequence whose start is begin.
   */
  std::vector<int>::iterator fill_instance_indices(int row, std::vector<int>::iterator output_begin) {
   int cpt_index = test.get(db_indices.at(0), row);
    // could store ndb_inds as member. But maybe not much speed up. 
   int ndb_inds = db_indices.size();
   for (int k = 0; k < ndb_inds ; k++) {
     *output_begin= test.get(db_indices.at(k), row);
     output_begin++; 
   } 
   return output_begin;
  }  
};
 
/** 
 * EVdenceMappedMappedCPT, which knows which MappedCPT entry to return for a given instance.
 * It actually returns nclass entries, one for each class. 
 * It assumes that a factor value i for n-th variable corresponds to i-1 th entry i n-th dimension
 */
class MappedCPT {
  // It was faster using c++ storage than Rcpp
  std::vector<int> db_indices;
  std::vector<double> cpt;
  // A reference to a unique instance of Evidence
  Evidence & test;
  Rcpp::CharacterVector columns; 
public: 
  MappedCPT(Rcpp::NumericVector cpt, const Rcpp::CharacterVector class_var,  Evidence & test) :
                    test(test) {
    // Do I want this to make a copy? Its OK to make a copy because it is a lightweight object.
    this->cpt = Rcpp::as<std::vector <double> >(cpt); 
    
    Rcpp::CharacterVector columns_db = test.getColumns();
    Rcpp::IntegerVector dim_inds = dims2columns(cpt, class_var, columns_db);
    this->db_indices = Rcpp::as<std::vector <int> >(dim_inds ); 
    const Rcpp::List & dimnames = cpt.attr("dimnames");
    columns  = dimnames.attr("names"); 
 }    
  
// get all classes entries, passing the index of the row 
void get_entries(int row, std::vector<double> & cpt_entries) {
 // int cpt_index = test.get(db_indices.at(0), row);
 // int sum = cpt_index - 1;
 // int ndb_inds = db_indices.size();
 // for (int k = 1; k < ndb_inds ; k++) {
 //   cpt_index = test.get(db_indices.at(k), row);
 //   cpt_index -= 1;  // delete
 //   sum += cpt_index * this->dim_prod.at(k  - 1);
 // }
 // // // Add an entry per each class 
 // int per_class_entries   = this->dim_prod.at(this->dim_prod.size() - 2); 
 // int ncpts = cpt_entries.size();
 // for (int i = 0; i < ncpts ; i++ ) {
 //   cpt_entries[i] =  this->cpt.at(sum + i * per_class_entries );
 //   // cpt_entries[i] = this->cpt[sum];
 // }   
}
 
private:  
  // matches the dims of the MappedCPT to columns of the db 
  Rcpp::IntegerVector dims2columns(const Rcpp::NumericVector cpt, const Rcpp::CharacterVector class_var,  const Rcpp::CharacterVector columns_db);
};

#endif