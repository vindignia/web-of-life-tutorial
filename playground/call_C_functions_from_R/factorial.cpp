#include <Rcpp.h>
#include <cmath>
using namespace Rcpp; 

// [[Rcpp::export]]
int factCpp(int n) {
  if ((n == 0) | (n == 1)) 
    return 1;
  else
    return n*factCpp(n-1);
}
