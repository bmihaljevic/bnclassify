#ifndef bnclassify_infer_H
#define bnclassify_infer_H

#include <Rcpp.h>
#include <RcppEigen.h>
#include <cmath> 
#include <basic-misc.h>

// TODO MODEL
//    Keep array dimnmes. 
//    Make it a std::vector because it is faster access to than  Rcpp
//    Copy and log 
//        Making a vector of std::vector would solve all the later, but would lose the dimension data. Howvever, I could keep the dim data apart in the CPT.  
//        Thus, I do not want gRbase code, as it works on Rcpp

/**
 *  Encapsulates a TODO bnc? model. 
 */
class Model { 
  private:   
    Rcpp::CharacterVector features;
    Rcpp::CharacterVector class_var;  
    std::vector<Rcpp::NumericVector> log_cpts;
    int nclass;
    Rcpp::IntegerVector get_class_index( ) ;
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
}; 

/**
 *  Holds the data with evidence for inference. 
 *  Current implementation holds a copy of the underlying data and thus only a single instance should exist per call to predict (no copies). 
 *  If the data are factors, then the values returned will correspond to 1-based indices for the CPTs 
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
  inline Rcpp::CharacterVector getColumns() {
   return  columns;
  } 
  inline int getN() const {
   return  N;
  }
  Evidence(Rcpp::DataFrame & test, const Rcpp::CharacterVector & features) {
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
 
/** 
 * EVdenceMappedCPT, which knows which CPT entry to return for a given instance.
 * It actually returns nclass entries, one for each class. 
 * It assumes that a factor value i for n-th variable corresponds to i-1 th entry i n-th dimension
 */
class CPT {
  // It was faster using c++ storage than Rcpp
  std::vector<int> dim_prod;
  std::vector<int> db_indices;
  std::vector<double> cpt;
  // A reference to a unique instance of Evidence
  Evidence & test;
  Rcpp::CharacterVector columns; 
public: 
  CPT(Rcpp::NumericVector cpt, const Rcpp::CharacterVector class_var,  Evidence & test) :
                    test(test) {
    // Do I want this to make a copy? Its OK to make a copy because it is a lightweight object.
    this->cpt = Rcpp::as<std::vector <double> >(cpt); 
    // check class is the last dimension of the cpt? 
    // TODO: this could also be the class cpt!!! remember that. No, I do not need to do this for class cpt. Class is a special cpt.
    const Rcpp::IntegerVector & dim = cpt.attr("dim");
    Rcpp::IntegerVector dim_prods = Rcpp::cumprod(dim);
    // note: i am using the local test here. it might do something non const to the object 
    this->dim_prod = Rcpp::as<std::vector <int> >(dim_prods);
    Rcpp::CharacterVector columns_db = test.getColumns();
    Rcpp::IntegerVector dim_inds = dims2columns(cpt, class_var, columns_db);
    this->db_indices = Rcpp::as<std::vector <int> >(dim_inds ); 
    const Rcpp::List & dimnames = cpt.attr("dimnames");
    columns  = dimnames.attr("names"); 
 }    
  
// get all classes entries, passing the index of the row 
void get_entries(int row, std::vector<double> & cpt_entries) {
 int cpt_index = test.get(db_indices.at(0), row);
 int sum = cpt_index - 1;
 int ndb_inds = db_indices.size();
 for (int k = 1; k < ndb_inds ; k++) {
   cpt_index = test.get(db_indices.at(k), row);
   cpt_index -= 1;  // delete
   sum += cpt_index * this->dim_prod.at(k  - 1);
 }
 // // Add an entry per each class 
 int per_class_entries   = this->dim_prod.at(this->dim_prod.size() - 2); 
 int ncpts = cpt_entries.size();
 for (int i = 0; i < ncpts ; i++ ) {
   cpt_entries[i] =  this->cpt.at(sum + i * per_class_entries );
   // cpt_entries[i] = this->cpt[sum];
 }   
}
 
private:  
  // matches the dims of the CPT to columns of the db 
  Rcpp::IntegerVector dims2columns(const Rcpp::NumericVector cpt, const Rcpp::CharacterVector class_var,  const Rcpp::CharacterVector columns_db);
};

#endif