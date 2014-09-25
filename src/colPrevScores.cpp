#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector colPrevScores(NumericMatrix x) {
  int nrow = x.nrow(), ncol = x.ncol();
  NumericVector out(ncol);
  
  for (int j = 0; j < ncol; j++) {
    int num_non_zero = 0;
    
    for (int i = 0; i < nrow; i++) {
      num_non_zero += x(i,j) > 0.0 ? 1 : 0;
    }
    
    double prev = (double) num_non_zero/nrow;
    
    out(j) = std::min(prev, 1.0-prev);
  }
  
  return out;
}
