#ifndef bnclassify_infer_H
#define bnclassify_infer_H

#include <Rcpp.h>
#include <RcppEigen.h>
#include <cmath> 
#include <basic-misc.h>

/**
 *  Encapsulates a TODO bnc? model. 
 */
class Model { 
  public:
    Model(Rcpp::List model);    
    const Rcpp::NumericVector & get_cpt(int i) const {
      return this->log_cpts.at(i);
    }
    Rcpp::CharacterVector getFeatures() const {      
      return this->features;
    } 
    Rcpp::CharacterVector getClassVar() const {      
      return this->class_var;
    }  
    //n excludes the class    
    std::size_t get_n() const {
      return features.size();  
    }
    int get_nclass() const { 
      return nclass;
    }
  private:   
    Rcpp::List model;
    Rcpp::CharacterVector features;
    Rcpp::CharacterVector class_var;  
    Rcpp::List all_cpts;
    std::vector<Rcpp::NumericVector> log_cpts;
    int nclass;
    Rcpp::IntegerVector get_class_index( ) ;
}; 

/**
 *  Holds the data with evidence for inference. 
 *  Current implementation holds a copy of the underlying data and thus only a single instance should exist per call to predict (no copies). 
 *  If the data are factors, then the values returned will correspond to 1-based indices for the CPTs 
 */
class Newdata {
  Rcpp::CharacterVector columns;
  // use a vector because I think it is faster to access than DataFrame
  // Currently, int allows only for factors, yet it could be generic.
  std::vector<std::vector<int> > data; 
  int N;   
public:
  inline double get(int i, int j) const {  
    return data.at(i).at(j); 
  }   
  inline Rcpp::CharacterVector getColumns() {
   return  columns;
  } 
  inline int getN() const {
   return  N;
  }
  Newdata(Rcpp::DataFrame & test, const Rcpp::CharacterVector & features) {
     // TODO:  Check all are factors?
     // I could also reduce all entries - 1 and make a transpose, that is, a matrix that goes by instance and then iterate that way.
     // If I go to integer, I ought to store the levels of the cpts somewhere.
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

#endif