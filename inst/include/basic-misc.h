#ifndef bnclassify_basicmisc_H
#define bnclassify_basicmisc_H

#include <Rcpp.h>

bool are_disjoint(Rcpp::Nullable<Rcpp::CharacterVector> x, Rcpp::Nullable<Rcpp::CharacterVector> y); 
bool hasna(const Rcpp::DataFrame & newdata); 
/**
 * A comparison that does not raise a compiler warning.
 */
bool safediff(unsigned int x, int y);  

#endif