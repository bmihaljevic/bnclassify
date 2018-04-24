#include <data.h>

using namespace Rcpp;    
 
// [[Rcpp::export]]
bool hasna_features(const DataFrame & newdata, const CharacterVector & features)
{  
  const DataFrame & data = trim_dataset_cpp(newdata, features);  
  return hasna(data);
}

// [[Rcpp::export]]
bool hasna(const DataFrame & newdata) 
{  
  for (int i = 0; i < newdata.size(); i++) { 
   const IntegerVector & vec = newdata.at(i);
   if (is_true(any(is_na(vec)))) return true;  
  }  
  return false;
}  

// [[Rcpp::export]]
DataFrame trim_dataset_cpp(const DataFrame & dataset, const CharacterVector & features)
{
     const Rcpp::CharacterVector & columns = dataset.names();
     if (!is_true(all(in(features, columns )))) {
       Rcpp::stop("Some features missing from data set.");
     }
     // Rcpp intersect may alter order of columns, but irrelevant here
     Rcpp::CharacterVector keep = Rcpp::intersect(columns, features);
     DataFrame data = dataset[keep]; 
     return data;
 }