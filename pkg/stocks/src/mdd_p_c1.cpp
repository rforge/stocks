#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double mdd_p_c1(NumericVector x) {
  int n = x.size();
  double maximum = x(0);
  double mdd = 1;
  double dd = 0;
  double current;
  for (int a = 1; a < n; ++a) {
    current = x(a);
    dd = current / maximum;
    if (dd < mdd) mdd = dd;
    if (current > maximum) maximum = current;
  }
  mdd = 1 - mdd;
  return(mdd);
}