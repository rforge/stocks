#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector pchanges_c(NumericVector x, int lag, double multiplier) {
  int n = x.size();
  NumericVector y(n-lag);
  if (lag == 1) {
    double current;
    double previous = x(0);
    if (multiplier == 1) {
      for (int a = 1; a < n; ++a) {
        current = x(a);
        y(a-1) = current / previous - 1;
        previous = current;
      }
    }
    else {
      for (int a = 1; a < n; ++a) {
        current = x(a);
        y(a-1) = (current / previous - 1) * multiplier;
        previous = current;
      }
    }
  }
  else {
    if (multiplier == 1) for (int a = lag; a < n; ++a) y(a-lag) = x(a) / x(a-lag) - 1;
    else for (int a = lag; a < n; ++a) y(a-lag) = (x(a) / x(a-lag) - 1) * multiplier;
  }
  return(y);
}