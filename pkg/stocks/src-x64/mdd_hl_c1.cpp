#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double mdd_hl_c1(NumericVector highs, NumericVector lows) {
  int n = highs.size();
  double maximum = highs(0);
  double mdd = 1;
  double dd;
  double currenthigh;
  double currentlow;
  for (int a = 1; a < n; ++a) {
    currenthigh = highs(a);
    currentlow = lows(a);
    dd = currentlow / maximum;
    if (dd < mdd) mdd = dd;
    if (currenthigh > maximum) maximum = currenthigh;
  }
  mdd = 1 - mdd;
  return(mdd);
}