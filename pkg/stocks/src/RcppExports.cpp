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
// pdiffs_c
NumericVector pdiffs_c(NumericVector x, int lag, double multiplier);
RcppExport SEXP stocks_pdiffs_c(SEXP xSEXP, SEXP lagSEXP, SEXP multiplierSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        NumericVector x = Rcpp::as<NumericVector >(xSEXP);
        int lag = Rcpp::as<int >(lagSEXP);
        double multiplier = Rcpp::as<double >(multiplierSEXP);
        NumericVector __result = pdiffs_c(x, lag, multiplier);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// pchanges_c
NumericVector pchanges_c(NumericVector x, int lag, double multiplier);
RcppExport SEXP stocks_pchanges_c(SEXP xSEXP, SEXP lagSEXP, SEXP multiplierSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        NumericVector x = Rcpp::as<NumericVector >(xSEXP);
        int lag = Rcpp::as<int >(lagSEXP);
        double multiplier = Rcpp::as<double >(multiplierSEXP);
        NumericVector __result = pchanges_c(x, lag, multiplier);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}