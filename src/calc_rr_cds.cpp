#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]
NumericVector calc_rr_cds(NumericVector outcome, NumericMatrix covars) {
  int nrow = covars.nrow(), ncol = covars.ncol();
  if (outcome.length() != nrow) {
    stop("length of outcome should be the same as the number of rows in covars");
  }
  
  NumericVector out(ncol);
  out.attr("names") = colnames(covars);
  
  for (int j = 0; j < ncol; j++) {
    double outcomes1 = 0;
    double outcomes0 = 0;
    double n1 = 0;
    double n0 = 0;
        
    for (int i = 0; i < nrow; i++) {
      double covar = covars(i,j);
      if (covar == 0.0) {
        n0 += 1;
        outcomes0 += outcome(i);
      } else {
        n1 += 1;
        outcomes1 += outcome(i);
      }
    }
    
    double prev1 = outcomes1/n1;
    double prev0 = outcomes0/n0;
    
    double rr = prev1/prev0;
    out(j) = rr > 1? rr : 1/rr;
  }
  return out;
}