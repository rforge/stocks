#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector mdd_p_c2(NumericVector x) {
  int n = x.size();
  int pos1 = 0;
  int pos2 = 0;
  int whichmax = 0;
  double maximum = x(0);
  double mdd = 1;
  double dd = 0;
  double current;
  for (int a = 1; a < n; ++a) {
    current = x(a);
    dd = current / maximum;
    if (dd < mdd) {
      mdd = dd;
      pos1 = whichmax;
      pos2 = a;
    }
    if (current > maximum) {
      whichmax = a;
      maximum = current;
    }
  }
  mdd = 1 - mdd;
  NumericVector out(3);
  out(0) = mdd;
  out(1) = pos1 + 1;
  out(2) = pos2 + 1;
  return(out);
}