#include <Rcpp.h>
#include <cmath>

// suncc needs help to disambiguate between sqrt( float ) and sqrt(double) 
inline static double sqrt_double(double x) { return ::sqrt(x); }

using namespace Rcpp; 

// [[Rcpp::export]]
int fibCpp(int n) {
  if ((n == 0) | (n == 1)) 
    return 1;
  else
    return fibCpp(n-1) + fibCpp(n-2);
}
