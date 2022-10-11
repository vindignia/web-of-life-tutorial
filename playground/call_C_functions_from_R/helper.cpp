#include <Rcpp.h>
#include <cmath>
using namespace Rcpp; 

// [[Rcpp::export]]
int fibCpp(int n) {
  if ((n == 0) | (n == 1)) 
    return 1;
  else
    return fibCpp(n-1) + fibCpp(n-2);
}

// [[Rcpp::export]]
int factCpp(int n) {
  if ((n == 0) | (n == 1)) 
    return 1;
  else
    return n*factCpp(n-1);
}

// [[Rcpp::export]]
NumericVector timesTwoCpp(NumericVector x) {
  return x * 2;
}
