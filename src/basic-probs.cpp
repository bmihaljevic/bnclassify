#include <Rcpp.h>
using namespace Rcpp;
using namespace std::placeholders;

// [[Rcpp::export]]
NumericVector smooth_sideeffect(NumericVector ctgt, double smooth) { 
  transform(ctgt.begin(), ctgt.end(), ctgt.begin(),
            bind(std::plus<double>(), _1, smooth));     
  return ctgt;
} 

// [[Rcpp::export]]
NumericVector exp_sideeffect(NumericVector p) { 
    double (*dexp)(double) = &std::exp;
    std::transform(p.begin(), p.end(), p.begin(), dexp);   
    return p;
}  

/*** R
a <- c(0.2, 2, -1, 0)
b <- c(0.2, 2, -1, 0)
exp_sideeffect(a)
stopifnot(all.equal(log(a), b))

a <- 1:5
smooth_sideeffect(a, 10)
smooth_sideeffect(a, -1)
smooth_sideeffect(a, 1:5)
*/
