#include <Rcpp.h>

using namespace Rcpp;

// diffs_c
NumericVector diffs_c(NumericVector x, int lag);
RcppExport SEXP stocks_diffs_c(SEXP xSEXP, SEXP lagSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        NumericVector x = Rcpp::as<NumericVector >(xSEXP);
        int lag = Rcpp::as<int >(lagSEXP);
        NumericVector __result = diffs_c(x, lag);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}