#include <Rcpp.h>
using namespace Rcpp;


// Commonalities:
// the data set is common across iterations
// many of the cpts are common
// the thetas corresponding to the data are common as long as the cpts are fixed
// many times the product is fixed given the same thetas

// DAtaset initial optimize
 // Do all entries - 1. 
 // Arrange by instance, not by column?
 // Then do the splitting into the folds 

class Dataset {
  CharacterVector columns;
  CharacterVector class_var;
  // check length of class var, check columns, etc. 
} ;

class Task { 
  CharacterVector features;
  CharacterVector class_var;
  CharacterVector data_columns;
};

class Model {
  CharacterVector features;
  CharacterVector class_var;
  // check all features and class are in data set. Well, I do not need class in data set.
};


 
// R and C++
 // most code could be in R, with certain critical parts in C++
 // Thus greedy could be in R, but parts where I update the prediction matrix output or similar could be C++

// TODO: NEW NAME: dB_feature_cpt
class CPT {
// get entries for classes, passing simply the instance values
// invariant: `rows` sum to one
  IntegerVector dim_prod; // this memory will reside in R rather than in c++ 
  IntegerVector db_indices;  //maps of columns to indices in a data set
  NumericVector cpt; 
public: 
  CPT(NumericVector cpt, const CharacterVector features, const CharacterVector class_var, const CharacterVector columns_db) {
    // Do I want this to make a copy? Its OK to make a copy because it is a lightweight object.
    this->cpt = cpt; 
    // check class is the last dimension of the cpt? 
    // TODO: this could also be the class cpt!!! remember that. 
    // No, I do not need to do this for class cpt. Class is a special cpt.
    const IntegerVector & dim = cpt.attr("dim"); 
    IntegerVector dim_prods = cumprod(dim);
    this->dim_prod = dim_prods; 
    this->db_indices = dims2columns(features, class_var, columns_db);
  }  
 
  // // get the entries of the cpt based on values in the dataset. the values are 1-based indices, because of factors. 
  void get_entries(IntegerVector values, std::vector<double> & cpt_entries) {
    // maybe i could iterate the values with iterator; but more cumbersome
    // Do the - 1 outside of the loop
    // IntegerVector cpt_values = values[db_indices];
    // each cpt values corresponds to a dim + value - 1 because it is 0-based

   // match - 1, because factors are 1-based.
   // When converting db to set of indices in the CPTs, discount 1; that way it is useful.
   // int size =  fam.size();
   // if (!(size  + 1 == dimension_prods.size())) stop("Must specify n-1 dimensions.");
   
   // get the index for the first class
   int index = values[db_indices[0]];
   index = index - 1;
   int sum = index;
   for (int k = 1; k < db_indices.size(); k++) {
     index = values[db_indices[k]];
     index = index - 1;  // delete
      sum += index * this->dim_prod[k - 1];
   }
   int per_class_entries   = this->dim_prod[this->dim_prod.size() - 2];
   // Add an entry per each class 
   for (int i = 0; i < cpt_entries.size(); i++ ) {
     cpt_entries[i] =  this->cpt[sum + i * per_class_entries ];
   }
  }
private:  
  // matches the dims of the CPT to columns of the db 
  IntegerVector dims2columns(const CharacterVector features, const CharacterVector class_var,  const CharacterVector columns_db);
};
//            
 
// Get the DB indices of a family
// maps the cpt inds to the columns of the data set 
IntegerVector CPT::dims2columns(const CharacterVector features, const CharacterVector class_var,  const CharacterVector columns_db) {
  const List & dimnames = this->cpt.attr("dimnames");
  const CharacterVector & fam = dimnames.attr("names");
  CharacterVector feature_fam = setdiff(fam, class_var);
  // TODO: check fam is last
  IntegerVector feature_fam_inds = match(feature_fam, columns_db);
  if (is_true(any(feature_fam_inds == 0)))  stop("All features must be in the dataset.");
  feature_fam_inds = feature_fam_inds - 1; 
  if (feature_fam_inds.size() != (this->dim_prod.size() - 1))  stop("Wrong cpt size.");
  return feature_fam_inds;
}

// [[Rcpp::export]]
NumericVector make_cpt(NumericVector cpt, const CharacterVector features, const CharacterVector class_var, const CharacterVector columns_db) { 
  CPT c = CPT(cpt, features, class_var, columns_db); 
  IntegerVector inds = IntegerVector::create(1);
  inds[0] = 1;
  std::vector<double> entries;
  entries.reserve(2);
  c.get_entries(inds, entries);
  // nothing added to vector
  Rcout << entries.at(0) << std::endl;
  return wrap(entries);
}

/*** R 
t
make_cpt(t$.params$bkblk, features(t), class_var(t), colnames(dbor))
*/
