#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector diffs_c(NumericVector x, int lag) {
  int n = x.size();
  NumericVector y(n-lag);
  if (lag == 1) {
    double current;
    double previous = x(0);
    for (int a = 1; a < n; ++a) {
      current = x(a);
      y(a-1) = current - previous;
      previous = current;
    }
  }
  else for (int a = lag; a < n; ++a) y(a-lag) = x(a) - x(a-lag);
  return(y);
}