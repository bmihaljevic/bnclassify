#ifndef bnclassify_infer_H
#define bnclassify_infer_H

#include <Rcpp.h>
#include <RcppEigen.h>
#include <cmath>

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

// HERE THE INDICES ARE 0 BASED!!! 
// they could be 1 based; that could be part of the mapping from data to here
// has the columns, N, but also contains the data? 
// Copying instances of test data would copy the whole matrix. Thus, I need only a single instance of this object during execution lifetime.
// TODO: Rename to Newdata
class Testdata {
  Rcpp::CharacterVector columns;
  Rcpp::std::vector<std::vector<int> > data; 
  int N;   
public:
  // check length of class var, check columns, etc.   
  // the underlying storage will be irrelevant. it will be hidden inside. could simply go and advance over the df, could hold it in std vector; whatever.
  inline double get(int i, int j) const {  
  // check range? done by ()
    return data.at(i).at(j); 
  }   
  inline Rcpp::CharacterVector getColumns() {
   return  columns;
  } 
  inline int getN() const {
   return  N;
  }
  Testdata(Rcpp::DataFrame & test, const Rcpp::CharacterVector & features) {
     // TODO: features optional. Check all are factors?
     // keep df storage  
     // get columns and class var
     // check at least 1 row and count N. 
     // Get number of classes? or not? Or that is elsewhere? That is, e.g., in model.
     // I could also reduce all entries - 1 and make a transpose, that is, a matrix that goes by instance and then iterate that way.
     // If I go to integer, I ought to store the levels of the cpts somewhere.
     // This could also be the initial matrix split  
     // checks: has all from model, right? no nas. 
     const Rcpp::CharacterVector & vec = test.names();
     if (!is_true(all(in(features, vec)))) {
       Rcpp::stop("Some features missing from data set.");
     }
     // using intersect does not alter order of columns,  unlike test[features]. it can be useful for debugging. 
     Rcpp::CharacterVector keep = Rcpp::intersect(vec, features);
     test = test[keep];
     // TODO: remove this from here. Do it at instance level. 
     if (hasna(test)) stop("NA entries in data set.");  
     
     this->columns = test.names();  
     this->N = test.nrow();
     this->data = Rcpp::as<std::vector<std::vector<int> > > (test);   
  }
};


#endif