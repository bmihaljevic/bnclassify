#ifndef bnclassify_infer_H
#define bnclassify_infer_H

#include <Rcpp.h>
#include <cmath> 
#include <basic-misc.h>
#include <multidim-array.h>   

/**
 * All CPT internal logics and rules here. 
 * Users of this class should not need to think of dimnames and similar, just of variables, features, etc.
 * It hold the log of original CPT entries.
 * TODO: rather than giving access to entries, I should let them index thrugh the CPT, but not get to the underlying storage.
 */
class CPT {
private:
    Rcpp::CharacterVector variables; 
    Rcpp::CharacterVector features; 
    std::vector<double> entries; 
    std::vector<int> dimprod; 
public: 
  // Do not store the class, though. Just the features.
  CPT(const Rcpp::NumericVector & cpt, const std::string & class_var); 
  const std::vector<double> & get_entries() const {
    return  entries; 
  } 
  // TODO: maybe remove this one
  const Rcpp::CharacterVector & get_variables() const { 
    return variables; 
  }  
  const Rcpp::CharacterVector & get_features() const {
    return features; 
  }   
  // Returns the cumulative product of the dimensions
  const std::vector<int> & get_dimprod() const { 
    return dimprod; 
  }  
};     
/**
 * Wraps a bnc fit model.  
 * Provide features, class name. 
 * makes a copy of cpts and logs entires after the copies. Keeps only these copies.
 * Call R functions to extract those data from the CPT, to avoid re-implementing things.    
 */
class Model { 
  private:   
    Rcpp::CharacterVector features;
    Rcpp::CharacterVector class_var;  
    Rcpp::CharacterVector classes;  
    std::vector<CPT> cpts;
    // int nclass = -1;
    // int ind_class = -1; 
    int nclass;
    int ind_class;
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
 *  If the data are factors, then the values returned will correspond to 0-based indices for the MappedCPTs 
 *  
 * The indices for rows and columns are 0-based. 
 */
class Evidence {
  Rcpp::CharacterVector columns;
  // Currently, int allows only for factors, yet it could be generic.
  std::vector<Rcpp::IntegerVector> data_columns;
  int N;   
public:
  inline int get(int column, int row) const {
    return data_columns.at(column)(row) - 1;
  }   
  inline Rcpp::CharacterVector getColumns() const {
   return  columns;
  } 
  inline int getN() const {
   return  N;
  }
  Evidence(Rcpp::DataFrame & test, const Rcpp::CharacterVector & features);
};  
/**
 * Obtains the CPT indices corresponding to a data instance.
 * An option is to use it as a proxy for CPT, in order to subset it, but 
 * CPT currently does not provide subsetting.
 */
class MappedCPT {
  // It was faster using c++ storage than Rcpp
  // Are the indices 1-based or one based?  I think 0 based because they are in Rcpp.
  std::vector<int> db_indices;
  const CPT & cpt;
  // A reference to a unique instance of Evidence
  const Evidence & test;
public:  
  MappedCPT(const CPT & cpt, const Evidence & test);
  /**
   * Fills in the instance's entries into the sequence. Returns iterator to one after last added.
   * The number of elements added is that of the dimensions of the CPT. It adds them into the output sequence whose start is begin.
   */
  inline std::vector<int>::iterator fill_instance_indices(int row, std::vector<int>::iterator output_begin) const {
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
 * Currently keep reference to both CPT and MappedCPT; i.e, the latter is not a proxy for the former.
 * Maps feature CPTs to the evidence. 
 */
class MappedModel {
 const Model & model;
 const Evidence & evidence;
 int nclass; 
 int n;
 std::vector<MappedCPT> cpts;     
 const std::vector<double> & class_cpt; 
 // Buffer for a row of per-class probabilities
 std::vector<double> output_buffer; 
 // Buffer for an instance 
 std::vector<int> instance_buffer; 
public: 
  MappedModel(const Model & x, const Evidence & test); 
  inline void fill_class_entries(int row, int feature) {
    const MappedCPT & mcpt = this->cpts.at(feature);
    mcpt.fill_instance_indices(row, instance_buffer.begin());  
    const CPT & cpt = model.get_cpt(feature); 
    const std::vector<int> & dimprod = cpt.get_dimprod();
    const std::vector<double> & cpt_entries = cpt.get_entries();
    // roughly half the time is spent here:
    subset_free_last_dim(cpt_entries, dimprod, instance_buffer.begin(),  output_buffer);
  }
  NumericMatrix predict(); 
};     

#endif