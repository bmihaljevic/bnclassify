#ifndef bnclassify_basicmisc_H
#define bnclassify_basicmisc_H

#include <Rcpp.h>

bool are_disjoint(Rcpp::Nullable<Rcpp::CharacterVector> x, Rcpp::Nullable<Rcpp::CharacterVector> y);  
// TODO: This should be called at instance level, not data frame! This way, if the data set is complete, it goes through it a couple of times.
bool hasna(const Rcpp::DataFrame & newdata); 
/**
 * A comparison that does not raise a compiler warning.
 */
bool safediff(unsigned int x, int y);   
/**
 * A set diff which preserves the order in the first vector. This is because rcpp setdiff does not preserve it.
 */
std::vector<std::string> ordersetdiff(Rcpp::CharacterVector vector, Rcpp::CharacterVector remove);   
/**
 * 0-based match. (1 less than what Rcpp returns)
 * rcpp match was returning -2147483648 when not finding the value, and the any() test was failing, thus implemented without rcpp.
 */
std::vector<int> match_zero_based(const Rcpp::CharacterVector & subset, const Rcpp::CharacterVector & superset, const std::string error_message);

#endif