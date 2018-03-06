#include <Rcpp.h>
using namespace Rcpp;

// R and C++
// most code could be in R, with certain critical parts in C++
// Thus greedy could be in R, but parts where I update the prediction matrix output or similar could be C++

// Commonalities:
// the data set is common across iterations
// many of the cpts are common
// the thetas corresponding to the data are common as long as the cpts are fixed
// many times the product is fixed given the same thetas
// **check that factor levels correspond to cpt levels; this is also done initially, at this first point, so the accessing code can be safe.**

// The data could be a integer matrix rather than a data frame, because all entries are integers

// DAtaset initial optimize
 // Do all entries - 1. 
 // Arrange by instance, not by column?
 // Then do the splitting into the folds 
 
class Task { 
  CharacterVector features;
  CharacterVector class_var;
  CharacterVector data_columns;
};

class Model { 
public:
  List get_cpts();
  Model(List model);
private:   
  List model;
  CharacterVector features;
  std::string class_var; 
};   

Model::Model(List x): model(x) { 
  this->class_var = as<std::string>(model[".class"]);
  // const NumericVector & class_cpt = all_cpts[class_var];
  // could also get this form n levels of class in the db? no, the model is the truth.
  // int nclass = class_cpt.size(); 
}

class Testdata {
  CharacterVector columns;
  std::vector<std::vector<int> > data; 
  int N;  
public:
  // check length of class var, check columns, etc.   
  // the underlying storage will be irrelevant. it will be hidden inside. could simply go and advance over the df, could hold it in std vector; whatever.
  inline double get(int i, int j) const {  
  // check range? done by ()
  // HERE THE INDICES ARE 0 BASED!!! 
    return data.at(i).at(j); 
  }   
  inline CharacterVector& getColumns() {
   return  columns;
  }
  Testdata(DataFrame test) {
     // keep df storage  
     // get columns and class var
     // check at least 1 row and count N. 
     // Get number of classes? or not? Or that is elsewhere? That is, e.g., in model.
     // I could also reduce all entries - 1 and make a transpose, that is, a matrix that goes by instance and then iterate that way.
     // If I go to integer, I ought to store the levels of the cpts somewhere.
     // This could also be the initial matrix split 
     this->N = test.nrow();
     this->columns = test.names();   
     this->data = Rcpp::as<std::vector<std::vector<int> > > (test);   
  }
} ;  

// TODO: NEW NAME: dB_feature_cpt 
// get_entries int row. db is a member of the cpt. 

class CPT {
// get entries for classes, passing simply the instance values
// invariant: `rows` sum to one
  IntegerVector dim_prod; // this memory will reside in R rather than in c++ 
  IntegerVector db_indices;  //maps of columns to indices in a data set
  NumericVector cpt; 
  const Testdata & test;
public: 
  CPT(NumericVector cpt, const CharacterVector features, const CharacterVector class_var,  Testdata & test) :
                    test(test) {
    // Do I want this to make a copy? Its OK to make a copy because it is a lightweight object.
    this->cpt = cpt; 
    // check class is the last dimension of the cpt? 
    // TODO: this could also be the class cpt!!! remember that. No, I do not need to do this for class cpt. Class is a special cpt.
    const IntegerVector & dim = cpt.attr("dim"); 
    IntegerVector dim_prods = cumprod(dim); 
    // note: i am using the local test here. it might do something non const to the object 
    this->dim_prod = dim_prods;  
    CharacterVector columns_db = test.getColumns();
    this->db_indices = dims2columns(features, class_var, columns_db);  
  }  
  
 // get all classes entries, passing the index of the row 
  void get_entries(int row, std::vector<double> & cpt_entries) { 
   int index = test.get(db_indices(0), row);
   int sum = index - 1;
   for (int k = 1; k < db_indices.size(); k++) {
     int index = test.get(db_indices(k), row);
     index = index - 1;  // delete
     sum += index * this->dim_prod(k - 1);
   }
   // Add an entry per each class 
   int per_class_entries   = this->dim_prod(this->dim_prod.size() - 2);
   for (int i = 0; i < cpt_entries.size(); i++ ) {
     cpt_entries[i] =  this->cpt(sum + i * per_class_entries );
   }
  }
 
  // // get the entries of the cpt based on values in the dataset. the values are 1-based indices, because of factors. 
  void get_entries(IntegerVector values, std::vector<double> & cpt_entries) {
    // maybe i could iterate the values with iterator; but more cumbersome
    // Do the - 1 outside of the loop
    // each cpt values corresponds to a dim + value - 1 because it is 0-based
    // IntegerVector cpt_values = values[db_indices]; 
   // if (!(size  + 1 == dimension_prods.size())) stop("Must specify n-1 dimensions."); 
   // get the index for the first class
   int index = values(db_indices[0]);
   int sum = index - 1;
   for (int k = 1; k < db_indices.size(); k++) {
     int index = values(db_indices(k));
     index = index - 1;  // delete
     sum += index * this->dim_prod(k - 1);
   }
   // Add an entry per each class 
   int per_class_entries   = this->dim_prod(this->dim_prod.size() - 2);
   for (int i = 0; i < cpt_entries.size(); i++ ) {
     cpt_entries[i] =  this->cpt(sum + i * per_class_entries );
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

// Mapping of model to cpts
// check all features in data set. Well, I do not need class in data set.
     // this is done by each cpt check
class MappedModel {
 const Model model;
  // no copies of the original cpts 
 std::vector<CPT> cpts;  
public:
  MappedModel(Model x, Testdata test): model(x) {
    
  } 
};  

// maybe distinguish train set and test set?


// [[Rcpp::export]]
NumericVector make_cpt(NumericVector cpt, const CharacterVector features, const CharacterVector class_var, DataFrame df) { 
  Testdata ds(df);
  CPT c = CPT(cpt, features, class_var, ds); 
  IntegerVector inds = IntegerVector::create(1);
  inds[0] = 2;
  // must initialize vector  of entries
  std::vector<double> entries(2);
  c.get_entries(inds, entries);
  return wrap(entries);
}

//[[Rcpp::export]]
IntegerMatrix df2matrix(DataFrame x) {
  IntegerMatrix y = internal::convert_using_rfunction(x, "data.matrix");  
  return y;
}

//[[Rcpp::export]]
NumericVector get_instance(NumericVector cpt, const CharacterVector features, const CharacterVector class_var, DataFrame df) { 
  Testdata ds(df);
  CPT c = CPT(cpt, features, class_var, ds);
  IntegerMatrix mat = df2matrix(df);
  IntegerVector row = mat.row(1);
  std::vector<double> entries(2);
  c.get_entries(row, entries);
  return wrap(entries);
}

//[[Rcpp::export]]
NumericVector get_row(NumericVector cpt, const CharacterVector features, const CharacterVector class_var, DataFrame df) { 
  Testdata ds(df);
  CPT c = CPT(cpt, features, class_var, ds);
  std::vector<double> entries(2);
  c.get_entries(1, entries);
  return wrap(entries);
}

// [[Rcpp::export]] 
double get_dataset(DataFrame df, int i, int j) {
 Testdata dset(df);
 return dset.get(i, j);
}   

// [[Rcpp::export]] 
NumericVector predict_rcpp(const List x, const DataFrame newdata) { 
  Model model(x);
  // cpts to log. or not?  
  
  // model: has features, class, etc. that is independent of the dataset. 
  // cpt: mapping of the model to the test data. dataset is the test data, only knows its columns.  
   
  // get all the cpts. this requires initializing the data set.
  // you would get the names of the things from the data set
  // then, for each row in the thing, get the cpts entries and multiply them 
 // make sure levels match the levels in my data set.
  
  NumericVector res;
  return res;
}     

/*** R   
kr <- foreign::read.arff('~/gd/phd/code/works-aug-semi-bayes/data/original/kr-vs-kp.arff')
library(bnclassify)
dbor <- kr 
t <- lp(nb('class', dbor), dbor, smooth = 1)    
make_cpt(t$.params$bkblk, features(t), class_var(t), dbor)
get_instance(t$.params$bkblk, features(t), class_var(t), dbor) 
get_row(t$.params$bkblk, features(t), class_var(t), dbor) 
t$.params$bkblk 
get_dataset(dbor, 0, 0)
get_dataset(dbor, 36, 0)
# get_dataset(dbor, 37, 0) # check out of
f <- features(t)

microbenchmark::microbenchmark({a = make_cpt(t$.params$bkblk, f, class_var(t), dbor)},
                               { b = get_instance(t$.params$bkblk, f, class_var(t),  dbor)  },
                               { d = get_row(t$.params$bkblk, f, class_var(t), dbor)  },
                               { e = get_dataset(dbor, 0, 25) }
                               )
*/
