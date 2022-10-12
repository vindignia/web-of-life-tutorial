#include <Rcpp.h>
#include <cmath>
#include <numeric>
#include <vector>
#include <iostream>
using namespace std;
using namespace Rcpp; 

/* this code computes the nestedness of a given incident matrix M 
# according to the definition given in 
# Fortuna, M.A., et al.: Coevolutionary dynamics shape the structure of bacteria‐phage infection networks. Evolution 1001-1011 (2019). 
# DOI 10.1111/evo.13731
 */

int arraySum(IntegerVector & v){
  int sum = 0;
  for(int i = 0; i < v.size(); i++){
    sum += v[i];
  }
  return sum;
}

int scalarProd(IntegerVector & v, IntegerVector & w){
  int sum = 0;
  for(int i = 0; i < v.size(); i++){
    sum += v[i]*w[i];
  }
  return sum;
}

// [[Rcpp::export]]
double nestednessCpp(const NumericMatrix & M) {
  IntegerMatrix B(M.nrow(), M.ncol());	
  
  int i, j, nr, nc;
  nr = M.nrow(); 
  nc = M.ncol(); 
  
  //binarize the matrix 
  for (int i = 0; i < nc*nr; i++) {
    if(M[i]>0){
      B[i]=1;
    } else {
      B[i]=0;
    }
  }
  
  // nestedness of rows
  double nestedness_rows = 0;
  for(int i = 0; i < (nr-1); i++) {
    j = i + 1;
    while (j < nr) {
      
      IntegerVector v_i = B( i , _ );
      IntegerVector v_j = B( j , _ );
      // IMPLEMENT 
      // k_i = sum(B[i,:])
      // k_j = sum(B[j,:])
      
      // more elegant alternative NOT WORKING 
      //int arr[] = { 2, 5, 7, 8, 2, 6, 9 };
      //int k_i = 0;
      //k_i = accumulate(arr, arr + nc, 0, std::plus<int>());
      
      int k_i = arraySum(v_i);
      int k_j = arraySum(v_j);
      
      // IMPLEMENT 
      // shared=sum(B[i,:].*B[j,:]) // sum of common interactions
      int shared = scalarProd(v_i,v_j); 
      
      
      // handle disconnected nodes 
      if (!((k_i == 0) || (k_j==0))){
        int min_shared = min(k_i,k_j); // min of the degrees
        nestedness_rows = nestedness_rows + (1.0*shared/min_shared);
      }
      j = j + 1;
    } // end while iterator
  } //   end for loop 
  
  // nestedness of columns
  double nestedness_cols = 0;
  for(int i = 0; i < (nc-1); i++) {
    j = i + 1;
    while (j < nc) {
      
      IntegerVector v_i = B( _ , i );
      IntegerVector v_j = B( _ , j );
      // IMPLEMENT 
      // k_i <- sum(B[,i])    
      // k_j <- sum(B[,j])  
      
      int k_i = arraySum(v_i);
      int k_j = arraySum(v_j);
      
      // IMPLEMENT 
      //  c_ij <- sum(B[,i] * B[,j])// sum of common interactions
      int shared = scalarProd(v_i,v_j); 
      
      // handle disconnected nodes 
      if (!((k_i == 0) || (k_j==0))){
        int min_shared = min(k_i,k_j); // min of the degrees
        nestedness_cols = nestedness_cols + (1.0*shared/min_shared);
      }
      j = j + 1;
    } // end while iterator
  } //   end for loop 
  
  //  Compute nestedness of the network
  double nestedness_val = (nestedness_rows + nestedness_cols) / ((nr * (nr - 1) / 2) + (nc * (nc - 1) / 2));
  
  return nestedness_val; 
}
