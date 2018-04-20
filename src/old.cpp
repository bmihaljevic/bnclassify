#include <Rcpp.h>
using namespace Rcpp;

// /** 
//  * EVdenceMappedMappedCPT, which knows which MappedCPT entry to return for a given instance.
//  * It actually returns nclass entries, one for each class. 
//  * It assumes that a factor value i for n-th variable corresponds to i-1 th entry i n-th dimension
//  */ 
// class MappedCPT ;
// {
//   // It was faster using c++ storage than Rcpp
//   std::vector<int> db_indices;
//   std::vector<double> cpt;
//   // A reference to a unique instance of Evidence
//   Evidence & test;
//   Rcpp::CharacterVector columns; 
// public: 
//   MappedCPT(Rcpp::NumericVector cpt, const Rcpp::CharacterVector class_var,  Evidence & test) :
//                     test(test) {
//     // Do I want this to make a copy? Its OK to make a copy because it is a lightweight object.
//     this->cpt = Rcpp::as<std::vector <double> >(cpt); 
//     
//     Rcpp::CharacterVector columns_db = test.getColumns();
//     Rcpp::IntegerVector dim_inds = dims2columns(cpt, class_var, columns_db);
//     this->db_indices = Rcpp::as<std::vector <int> >(dim_inds ); 
//     const Rcpp::List & dimnames = cpt.attr("dimnames");
//     columns  = dimnames.attr("names"); 
//  }    
//   
// // get all classes entries, passing the index of the row 
// void get_entries(int row, std::vector<double> & cpt_entries) {
//  // int cpt_index = test.get(db_indices.at(0), row);
//  // int sum = cpt_index - 1;
//  // int ndb_inds = db_indices.size();
//  // for (int k = 1; k < ndb_inds ; k++) {
//  //   cpt_index = test.get(db_indices.at(k), row);
//  //   cpt_index -= 1;  // delete
//  //   sum += cpt_index * this->dim_prod.at(k  - 1);
//  // }
//  // // // Add an entry per each class 
//  // int per_class_entries   = this->dim_prod.at(this->dim_prod.size() - 2); 
//  // int ncpts = cpt_entries.size();
//  // for (int i = 0; i < ncpts ; i++ ) {
//  //   cpt_entries[i] =  this->cpt.at(sum + i * per_class_entries );
//  //   // cpt_entries[i] = this->cpt[sum];
//  // }   
// }
//  
// private:  
//   // matches the dims of the MappedCPT to columns of the db 
//   Rcpp::IntegerVector dims2columns(const Rcpp::NumericVector cpt, const Rcpp::CharacterVector class_var,  const Rcpp::CharacterVector columns_db);
// };


// needs not be a member function as it uses no members of MappedCPT 
// Get the DB indices of a family
// maps the cpt inds to the columns of the data set 
// IntegerVector MappedCPT::dims2columns(const NumericVector cpt, const CharacterVector class_var,  const CharacterVector columns_db) { 
//   const List & dimnames = cpt.attr("dimnames");
//   const CharacterVector & fam = dimnames.attr("names"); 
//   CharacterVector feature_fam = wrap(ordersetdiff(fam, class_var)); 
//   IntegerVector feature_fam_inds = match(feature_fam, columns_db);
//   if (is_true(any(feature_fam_inds == 0)))  stop("All features must be in the dataset.");
//   feature_fam_inds = feature_fam_inds - 1; 
//   // TODO:: only temporarily commented:
//   // if (safediff(feature_fam_inds.size(), this->dim_prod.size() - 1)) stop("Wrong cpt size.");
//   return feature_fam_inds;
// } 