#ifndef bnclassify_infer_H
#define bnclassify_infer_H

#include <Rcpp.h>
#include <RcppEigen.h>
#include <cmath> 
#include <basic-misc.h>
#include <multidim-array.h>

// Debug stuff. TODO: remove.
void printv(std::vector<double> v); 


/**
 * All CPT internal logics and rules here. 
 * Users of this class should not need to think of dimnames and similar, just of variables, features, etc.
 * It hold the log of original CPT entries.
 * TODO: rather than giving access to entries, I should let them index thrugh the CPT, but not get to the underlying storage.
 */
class CPT {
private:
    std::vector<std::string> variables; 
    std::vector<std::string> features; 
    std::vector<double> entries; 
    std::vector<int> dimprod; 
public: 
  // Do not store the class, though. Just the features.
  CPT(const Rcpp::NumericVector & cpt, const std::string & class_var);
  
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


/**
 *  Encapsulates a TODO bnc? model. 
 */
class Model { 
  private:   
    Rcpp::CharacterVector features;
    Rcpp::CharacterVector class_var;  
    Rcpp::CharacterVector classes;  
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
  Evidence(Rcpp::DataFrame & test, const Rcpp::CharacterVector & features);
}; 

/**
 * A proxy to the CPT, obtaining the CPT indices corresponding to a data instance.
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
  // get all classes entries, passing the index of the row
  // TODO: this should be implemented in CPT. Not here.
  inline void get_entries(std::vector<int>::iterator begin, std::vector<int>::iterator end, std::vector<double> & output) const {
    // Start with first class. Assumes that end is writable. That is why end should be part of Mapped Model or something, which is where this 
    // function should be
    *end = 1;
    const std::vector<int> & dimprod = this->cpt.get_dimprod();
    int sum = entry_index(begin, dimprod);  
   // // Add an entry per each class
   int per_class_entries   = dimprod.at(dimprod.size() - 2);
   int ncpts = output.size(); 
   const std::vector<double> & cpt_entries = this->cpt.get_entries();
   for (int i = 0; i < ncpts ; i++ ) {
     output[i] =  cpt_entries.at(sum + i * per_class_entries );
   }
  }
};  

// Mapping of model  cpts to evidence
// check all features in data set. Well, I do not need class in data set.
// this is done by each cpt check
// make sure data levels and cpt levels match 
// Only features mapped, not class.
// no copies of the original cpts 
// Know about n and nclass, but not about size of the data set.
class MappedModel {
 const Model & model;
 const Evidence & evidence;
 int nclass; 
 int n;
 std::vector<MappedCPT> cpts;     
 const std::vector<double> & class_cpt; 
 std::vector<double> per_class_cpt_entries; 
 std::vector<int> instance_cpt_inds;
 
public: 
  MappedModel(const Model & x, const Evidence & test); 
  inline void fill_class_entries(int row, int feature) {
    // const MappedCPT & mcpt = get_mapped_cpt(j);
    const MappedCPT & mcpt = this->cpts.at(feature);
    std::vector<int>::iterator cpt_inds_end = mcpt.fill_instance_indices(row, instance_cpt_inds.begin());  
    mcpt.get_entries(instance_cpt_inds.begin(), cpt_inds_end, per_class_cpt_entries);
  }
  // TODO: dont know if this will make a copy? It will. 
  NumericMatrix predict();
  // TODO: remove?
  // inline const MappedCPT& get_mapped_cpt(int i) const {
  //   // TODO: change to []?
  //   return this->cpts.at(i);
  // }  
};     

#endif