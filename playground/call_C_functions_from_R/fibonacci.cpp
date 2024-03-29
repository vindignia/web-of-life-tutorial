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