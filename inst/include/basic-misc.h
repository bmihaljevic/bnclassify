#ifndef bnclassify_basicmisc_H
#define bnclassify_basicmisc_H

#include <Rcpp.h>

bool are_disjoint(Rcpp::Nullable<Rcpp::CharacterVector> x, Rcpp::Nullable<Rcpp::CharacterVector> y); 
bool hasna(const Rcpp::DataFrame & newdata); 
/**
 * A comparison that does not raise a compiler warning.
 */
bool safediff(unsigned int x, int y);   
/**
 * A set diff which preserves the order in the first vector
 */
std::vector<std::string> ordersetdiff(Rcpp::CharacterVector vector, Rcpp::CharacterVector remove);  
/**
 * Maps the features to columns in the data frame.
 * The indices are 0-based.
 * TODO: probably should receive evidence or data frame as input, not columns_db character vector. Or it should be a vector of strings.
 * TODO: make this comments generic. do not refer to columns.
 */
std::vector<int> match_zero_based(const std::vector<std::string> & features, const Rcpp::CharacterVector & columns_db); 

#endif