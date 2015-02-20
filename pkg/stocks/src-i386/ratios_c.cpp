#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector ratios_c(NumericVector x) {
  int n = x.size();
  NumericVector y(n-1);
  double current;
  double previous = x(0);
  for (int a = 1; a < n; ++a) {
    current = x(a);
    y(a-1) = current / previous;
    previous = current;
  }
  return(y);
}