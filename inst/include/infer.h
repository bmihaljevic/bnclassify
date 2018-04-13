#ifndef bnclassify_infer_H
#define bnclassify_infer_H

#include <Rcpp.h>
#include <RcppEigen.h>
#include <cmath>

/**
 *  Encapsulates a bnc model. 
 */
class Model { 
  public:
    Model(List model);    
    const NumericVector & get_cpt(int i) const {
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
    std::vector<NumericVector> log_cpts;
    int nclass;
    Rcpp::IntegerVector get_class_index( ) ;
};

#endif