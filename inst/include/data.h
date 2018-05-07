#ifndef bnclassify_data_H
#define bnclassify_data_H

#include <Rcpp.h> 

bool hasna(const Rcpp::DataFrame & newdata);
bool hasna_features(const Rcpp::DataFrame & newdata, const SEXP & features);
// TODO: This should be called at instance level, not data frame! This way, if the data set is complete, it goes through it a couple of times.
Rcpp::DataFrame trim_dataset_cpp(const Rcpp::DataFrame & dataset, const Rcpp::CharacterVector & features);

#endif
