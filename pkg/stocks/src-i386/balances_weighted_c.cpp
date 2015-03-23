#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix balances_weighted_c(NumericMatrix ratios, double initial, NumericVector weights, int rebalance_interval) {
  int rows = ratios.nrow();
  int num_funds = ratios.ncol();
  NumericMatrix out(rows + 1, num_funds + 1);
  double fund_sum = 0;
  double new_val = 0;
  for (int b = 0; b < num_funds; ++ b) {
    new_val = initial * weights(b);
    out(0, b) = new_val;
    fund_sum += new_val;
  }
  out(0, num_funds) = fund_sum;
  int counter = 0;
  for (int a = 0; a < rows; ++a) {
    counter += 1;
    fund_sum = 0;
    for (int b = 0; b < num_funds; ++b) {
      new_val = out(a, b) * ratios(a, b);
      out(a+1, b) = new_val;     
      fund_sum += new_val;
    }
    out(a+1, num_funds) = fund_sum;
    if (counter == rebalance_interval) {
      for (int b = 0; b < num_funds; ++b) out(a+1, b) = fund_sum * weights(b);
      counter = 0;
    }
  }
  return(out);
}