#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector mdd_hl_c2(NumericVector highs, NumericVector lows) {
  int n = highs.size();
  int pos1;
  int pos2;
  int whichmax = 0;
  double maximum = highs(0);
  double mdd = 1;
  double dd;
  double currenthigh;
  double currentlow;
  for (int a = 1; a < n; ++a) {
    currenthigh = highs(a);
    currentlow = lows(a);
    dd = currentlow / maximum;
    if (dd < mdd) {
      mdd = dd;
      pos1 = whichmax;
      pos2 = a;
    }
    if (currenthigh > maximum) {
      whichmax = a;
      maximum = currenthigh;
    }
  }
  mdd = 1 - mdd;
  NumericVector out(3);
  out(0) = mdd;
  out(1) = pos1 + 1;
  out(2) = pos2 + 1;
  return(out);
}