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
// ratios_c
NumericVector ratios_c(NumericVector x);
RcppExport SEXP stocks_ratios_c(SEXP xSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        NumericVector x = Rcpp::as<NumericVector >(xSEXP);
        NumericVector __result = ratios_c(x);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// mdd_p_c1
double mdd_p_c1(NumericVector x);
RcppExport SEXP stocks_mdd_p_c1(SEXP xSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        NumericVector x = Rcpp::as<NumericVector >(xSEXP);
        double __result = mdd_p_c1(x);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// mdd_p_c2
NumericVector mdd_p_c2(NumericVector x);
RcppExport SEXP stocks_mdd_p_c2(SEXP xSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        NumericVector x = Rcpp::as<NumericVector >(xSEXP);
        NumericVector __result = mdd_p_c2(x);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// mdd_hl_c1
double mdd_hl_c1(NumericVector highs, NumericVector lows);
RcppExport SEXP stocks_mdd_hl_c1(SEXP highsSEXP, SEXP lowsSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        NumericVector highs = Rcpp::as<NumericVector >(highsSEXP);
        NumericVector lows = Rcpp::as<NumericVector >(lowsSEXP);
        double __result = mdd_hl_c1(highs, lows);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}
// mdd_hl_c2
NumericVector mdd_hl_c2(NumericVector highs, NumericVector lows);
RcppExport SEXP stocks_mdd_hl_c2(SEXP highsSEXP, SEXP lowsSEXP) {
BEGIN_RCPP
    SEXP __sexp_result;
    {
        Rcpp::RNGScope __rngScope;
        NumericVector highs = Rcpp::as<NumericVector >(highsSEXP);
        NumericVector lows = Rcpp::as<NumericVector >(lowsSEXP);
        NumericVector __result = mdd_hl_c2(highs, lows);
        PROTECT(__sexp_result = Rcpp::wrap(__result));
    }
    UNPROTECT(1);
    return __sexp_result;
END_RCPP
}