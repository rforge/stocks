#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector pdiffs_c(NumericVector x, int lag, double multiplier) {
  int n = x.size();
  double current;
  double previous;
  NumericVector y(n-lag);
  if (lag == 1) {
    previous = x(0);
    if (multiplier == 1) {
      for (int a = 1; a < n; ++a) {
        current = x(a);
        y(a-1) = 2 * (current - previous) / (current + previous);
        previous = current;
      }
    }
    else {
      for (int a = 1; a < n; ++a) {
        current = x(a);
        y(a-1) = 2 * (current - previous) / (current + previous) * multiplier;
        previous = current;
      }
    }
  }
  else {
    if (multiplier == 1) {
      for (int a = lag; a < n; ++a) {
        current = x(a);
        previous = x(a-lag);
        y(a-lag) = 2 * (current - previous) / (current + previous);
      }
    }
    else {
      for (int a = lag; a < n; ++a) {
        current = x(a);
        previous = x(a-lag);
        y(a-lag) = 2 * (current - previous) / (current + previous) * multiplier;
      }
    }
  }
  return(y);
}