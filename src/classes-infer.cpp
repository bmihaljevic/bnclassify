#include <Rcpp.h>
#include <RcppEigen.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::plugins(cpp11)]]

using Eigen::MatrixXd;

// R and C++
// most code could be in R, with certain critical parts in C++
// Thus greedy could be in R, but parts where I update the prediction matrix output or similar could be C++
// The data could be a integer matrix rather than a data frame, because all entries are integers
// maybe distinguish train set and test set?
  // yes, in greedy distinguish the full data set

// Commonalities:
// the data set is common across iterations
// many of the cpts are common
// the thetas corresponding to the data are common as long as the cpts are fixed
// many times the product is fixed given the same thetas
// **check that factor levels correspond to cpt levels; this is also done initially, at this first point, so the accessing code can be safe.**  

// DAtaset initial optimize
 // Do all entries - 1. 
 // Arrange by instance, not by column?
 // Then do the splitting into the folds 
 
 // predict_rcpp
//   Model model(x);
//   // cpts to log. or not? 
//   Testdata test(newdata); 
//   // make sure levels match the levels in my data set.
//   MappedModel mm(model, test);  
// model: has features, class, etc. that is independent of the dataset. 

class Model { 
  public:
    Model(List model);   
    // this should return a copy? how to return a const reference
    // NumericVector get_cpt(int i) const {      
    //   return this->all_cpts.at(i);
    // }  
    const NumericVector & get_cpt(int i) const {      
      return this->log_cpts.at(i);
    } 
    CharacterVector getFeatures() const {      
      return this->features;
    } 
    CharacterVector getClassVar() const {      
      return this->class_var;
    }  
    //n excludes the class    
    int get_n() const {
      return features.size();  
    }
    int get_nclass() const { 
      return nclass;
    }
  private:   
    List model;
    CharacterVector features;
    CharacterVector class_var;  
    List all_cpts;
    std::vector<NumericVector> log_cpts;
    int nclass;
    IntegerVector get_class_index( ) ;
};      
IntegerVector Model::get_class_index() {  
  const CharacterVector & vars_model = this->all_cpts.names(); 
  IntegerVector index = match(class_var, vars_model );
  if (index.size() != 1) stop("Class CPT missing.");
  return index ;
}
Model::Model(List x): model(x) { 
  // this->class_var = as<std::string>(model[".class"]); 
  this->class_var = model[".class"];
  this->all_cpts = x[".params"];
  this->log_cpts = std::vector<NumericVector>(this->all_cpts.size()); 
  for (int i = 0; i < this->all_cpts.size(); i++) {
    // a copy so that log does not modify original 
   // this->log_cpts.push_back(as<std::vector<double> >(this->log_cpts.at(i)));
   const NumericVector & cpt = this->all_cpts.at(i);
   NumericVector cloned = clone(cpt);
   std::transform(cloned, cloned, std::log);
   this->log_cpts.push_back(cloned);
  }
  // const NumericVector & class_cpt = all_cpts[class_var];
  // could also get this form n levels of class in the db? no, the model is the truth.
  // int nclass = class_cpt.size();   
  this->nclass = 2;
  const CharacterVector & vars_model = all_cpts.names(); 
  // extract this to a function
  const CharacterVector & class_var_rcpp = wrap(class_var);
  IntegerVector index = match(class_var_rcpp, vars_model );
  IntegerVector allinds =  seq_along(vars_model);
  index = setdiff(allinds, index) - 1;
  const List & feature_cpts = all_cpts[index];
  // I could get it in c++ and pass it to std::vector instead 
  this->features = feature_cpts.names();   
  IntegerVector class_index = get_class_index() ; 
  allinds =  seq_along(this->all_cpts);
  IntegerVector features_index = setdiff(allinds, class_index) - 1; 
  const NumericVector & class_cpt = this->all_cpts.at(class_index[0] - 1);
  // std::vector<double> class_cpt = as<std::vector <double>> (this->all_cpts[features_index]);
  // this->nclass = class_cpt.size();
  this->nclass = std::distance(class_cpt.begin(), class_cpt.end());
  // this->n = features.size();
}  

// HERE THE INDICES ARE 0 BASED!!! 
// they could be 1 based; that could be part of the mapping from data to here
// has the columns, N, but also contains the data? 
// Copying instances of test data would copy the whole matrix. Thus, I need only a single instance of this object during execution lifetime.
class Testdata {
  CharacterVector columns;
  std::vector<std::vector<int> > data; 
  int N;  
public:
  // check length of class var, check columns, etc.   
  // the underlying storage will be irrelevant. it will be hidden inside. could simply go and advance over the df, could hold it in std vector; whatever.
  inline double get(int i, int j) const {  
  // check range? done by ()
    return data.at(i).at(j); 
  }   
  inline CharacterVector getColumns() {
   return  columns;
  } 
  inline int getN() const {
   return  N;
  }
  Testdata(DataFrame test): columns(test.names()) {
     // keep df storage  
     // get columns and class var
     // check at least 1 row and count N. 
     // Get number of classes? or not? Or that is elsewhere? That is, e.g., in model.
     // I could also reduce all entries - 1 and make a transpose, that is, a matrix that goes by instance and then iterate that way.
     // If I go to integer, I ought to store the levels of the cpts somewhere.
     // This could also be the initial matrix split 
     this->N = test.nrow();
     this->data = Rcpp::as<std::vector<std::vector<int> > > (test);   
  }
} ;  

// TODO: NEW NAME: dB_feature_cpt 
// get_entries int row. db is a member of the cpt.  
class CPT {
// get entries for classes, passing simply the instance values
// invariant: `rows` sum to one
  // IntegerVector dim_prod; // this memory will reside in R rather than in c++ 
  std::vector<int> dim_prod;
  // IntegerVector db_indices;  //maps of columns to indices in a data set
  std::vector<int> db_indices;
  // NumericVector cpt; 
  std::vector<double> cpt;
  Testdata & test;
public: 
  CPT(NumericVector cpt, const CharacterVector features, const CharacterVector class_var,  Testdata & test) :
                    test(test) {
    // Do I want this to make a copy? Its OK to make a copy because it is a lightweight object.
    this->cpt = as<std::vector <double> >(cpt); 
    // check class is the last dimension of the cpt? 
    // TODO: this could also be the class cpt!!! remember that. No, I do not need to do this for class cpt. Class is a special cpt.
    const IntegerVector & dim = cpt.attr("dim");
    IntegerVector dim_prods = cumprod(dim);
    // note: i am using the local test here. it might do something non const to the object 
    this->dim_prod = as<std::vector <int> >(dim_prods);
    CharacterVector columns_db = test.getColumns();
    IntegerVector dim_inds = dims2columns(features, cpt, class_var, columns_db);
    this->db_indices = as<std::vector <int> >(dim_inds );
 }  
  
 // get all classes entries, passing the index of the row 
  void get_entries(int row, std::vector<double> & cpt_entries) { 
   int index = test.get(db_indices[0], row);
   int sum = index - 1;
   int ndb_inds = db_indices.size();
   for (int k = 1; k < ndb_inds ; k++) {
     int index = test.get(db_indices[k], row);
     index = index - 1;  // delete
     sum += index * this->dim_prod[k - 1];
   }
   // // Add an entry per each class 
   int per_class_entries   = this->dim_prod[this->dim_prod.size() - 2];
   int ncpts = cpt_entries.size();
   for (int i = 0; i < ncpts ; i++ ) {
     cpt_entries[i] =  this->cpt[sum + i * per_class_entries ];
     // cpt_entries[i] = this->cpt[sum];
   }
  }
 
  // // get the entries of the cpt based on values in the dataset. the values are 1-based indices, because of factors. 
  void get_entries(const IntegerVector & values, std::vector<double> & cpt_entries) {
    // maybe i could iterate the values with iterator; but more cumbersome
    // Do the - 1 outside of the loop
    // each cpt values corresponds to a dim + value - 1 because it is 0-based
    // IntegerVector cpt_values = values[db_indices]; 
   // if (!(size  + 1 == dimension_prods.size())) stop("Must specify n-1 dimensions."); 
   // get the index for the first class
   int index = values(db_indices[0]);
   int sum = index - 1;
   for (int k = 1; k < db_indices.size(); k++) {
     int index = values(db_indices[k]);
     index = index - 1;  // delete
     sum += index * this->dim_prod[k - 1];
   }
   // Add an entry per each class 
   int per_class_entries   = this->dim_prod[this->dim_prod.size() - 2];
   for (int i = 0; i < cpt_entries.size(); i++ ) {
     cpt_entries[i] =  this->cpt[sum + i * per_class_entries ];
   }
 }
private:  
  // matches the dims of the CPT to columns of the db 
  IntegerVector dims2columns(const CharacterVector features, const NumericVector cpt, const CharacterVector class_var,  const CharacterVector columns_db);
};
 
// Get the DB indices of a family
// maps the cpt inds to the columns of the data set 
IntegerVector CPT::dims2columns(const CharacterVector features, const NumericVector cpt, const CharacterVector class_var,  const CharacterVector columns_db) { 
  const List & dimnames = cpt.attr("dimnames");
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
// make sure data levels and cpt levels match 
class MappedModel {
 const Model model;
  // no copies of the original cpts 
 std::vector<CPT> cpts;   
 // class cpt is the only unmapped one 
 NumericVector class_cpt;
public:
  MappedModel(Model x, Testdata & test): model(x) { 
    const int n = x.get_n();
    cpts.reserve(n);  
    // for (List::iterator iter = x.get_cpts().begin(); iter != x.get_cpts().begin() + 5; iter++) {
    for (unsigned int i = 0; i < n; i++) {
      // NumericVector cpt = (*iter);
      NumericVector cpt = x.get_cpt(i);
      // could extract these function calls
      CPT c(cpt, model.getFeatures(), model.getClassVar(), test);
      // cpts.push_back(CPT(cpt, model.getFeatures(), model.getClassVar(), test));
      // cpts.push_back(std::move(c));
      cpts.push_back(c);
    }
    // // TODO: this must be better done!!! And must work with more cases, etc.
    this->class_cpt = x.get_cpt(n);
  }  
  inline CPT& get_mapped_cpt(int i) {
    // TODO: change to []?
    return this->cpts.at(i);
  } 
  inline NumericVector& get_class_cpt() {
    return this->class_cpt;
  }
};    

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
  // return NumericVector::create(1);
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
  // std::vector<double> entries(2);
  // c.get_entries(1, entries);
  // return wrap(entries);
  return NumericVector::create(1);
}

// [[Rcpp::export]] 
double get_dataset(DataFrame df, int i, int j) {
 Testdata dset(df);
 return dset.get(i, j);
}      

// 
// // [[Rcpp::export]] 
// NumericVector compute_joint_instance(MappedModel model) {
//  // from 1 to N get the cpt entries of all classes from the cpts; multiply them.
//  // get all cpts; for all get the entries, then multiply the entries to get the row of output
//  // output could simply be a vector, thus do not increase to output 
//  // i could have iterators over the data and just go across it 
//  return NumericVector::create(1); 
// }

// output: N x nclass
// instances: N x whatever. Follow the feature indices.  
 
// NumericVector compute_joint(MappedModel model) { 

// [[Rcpp::export]]
NumericMatrix compute_joint(List x, DataFrame newdata) { 
 Model mod(x);
 Testdata test(newdata);
 MappedModel model(mod, test);
 int N = test.getN();
 int n = mod.get_n();
 int nclass = mod.get_nclass();
 // NumericMatrix output(N, nclass);
 MatrixXd output(N, nclass);
 NumericVector & class_cpt = model.get_class_cpt();
 std::vector<int> instance(n);
 std::vector<double> per_class_cpt_entries(nclass);
 for (int instance_ind = 0; instance_ind  < N ; instance_ind++) {
    // set output to copies of class cpt
     for (int theta_ind = 0; theta_ind < nclass; theta_ind++) {
       // int ind_column = theta_ind * N;
       // output.at(ind_column + instance_ind) = class_cpt[theta_ind];
       output(instance_ind, theta_ind) = class_cpt[theta_ind];
     }
     // add the cpt entry of a single feature:
     for (int j = 0; j < n; j++) {
        // CPT & cpt  = model.get_mapped_cpt(j);
        // cpt.get_entries(instance_ind, per_class_cpt_entries);
        model.get_mapped_cpt(j).get_entries(instance_ind, per_class_cpt_entries);
        for (int theta_ind = 0; theta_ind < nclass; theta_ind++) {
             // output.at(ind_column + instance_ind) += per_class_cpt_entries[theta_ind];
             output(instance_ind, theta_ind) += per_class_cpt_entries[theta_ind];
             // output.at(instance_ind, theta_ind) += theta_ind;
        }
     } // features
 } // instances
 return wrap(output);
 // NumericMatrix out(2,2);
 // return out;
}

// [[Rcpp::export]]
NumericVector do_mapped(List x, DataFrame newdata) {
 Model model(x);
 Testdata test(newdata);
 MappedModel mm(model, test);
 // CPT c = mm.get_mapped_cpt(0);
 // std::vector<double> entries(2);
 // c.get_entries(1, entries);
 // return wrap(entries);
 return NumericVector::create(1);
}

// todo: logarithm of the cpts
// class names for all cpts
// after joint, all times go up

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
do_mapped(t, dbor)
outp <- compute_joint(t, dbor)
head(outp)
tail(outp)
# 
f <- features(t)
cpt <- t$.params$bkblk
cvar <- class_var(t)
# microbenchmark::microbenchmark(
#                                { d = get_row(cpt, f, cvar, dbor)  },
#                                {a = make_cpt(cpt, f, cvar, dbor)},
#                                { b = get_instance(cpt, f, cvar,  dbor)  },
#                                { e = get_dataset(dbor, 0, 25) },
#                                { g = do_mapped(t, dbor)})  

# microbenchmark::microbenchmark(  { g = do_mapped(t, dbor)} )
# microbenchmark::microbenchmark(    { d = get_row(t$.params$bkblk, f, class_var(t), dbor)  })
# microbenchmark::microbenchmark(    { d = get_row(t$.params$bkblk, f, class_var(t), dbor)  })
# microbenchmark::microbenchmark(    { d = get_row(t$.params$bkblk, f, class_var(t), dbor)  })

microbenchmark::microbenchmark( { f = compute_joint(t, dbor)},
                                  { h  = bnclassify:::compute_log_joint(t, dbor)}, times = 2e3 ) 
*/
