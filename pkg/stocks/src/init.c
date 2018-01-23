#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
  Check these declarations against the C/Fortran source code.
*/

  /* .Call calls */
  extern SEXP stocks_diffs_c(SEXP, SEXP);
extern SEXP stocks_mdd_hl_c1(SEXP, SEXP);
extern SEXP stocks_mdd_hl_c2(SEXP, SEXP);
extern SEXP stocks_mdd_p_c1(SEXP);
extern SEXP stocks_mdd_p_c2(SEXP);
extern SEXP stocks_pchanges_c(SEXP, SEXP, SEXP);
extern SEXP stocks_pdiffs_c(SEXP, SEXP, SEXP);
extern SEXP stocks_ratios_c(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"stocks_diffs_c",    (DL_FUNC) &stocks_diffs_c,    2},
  {"stocks_mdd_hl_c1",  (DL_FUNC) &stocks_mdd_hl_c1,  2},
  {"stocks_mdd_hl_c2",  (DL_FUNC) &stocks_mdd_hl_c2,  2},
  {"stocks_mdd_p_c1",   (DL_FUNC) &stocks_mdd_p_c1,   1},
  {"stocks_mdd_p_c2",   (DL_FUNC) &stocks_mdd_p_c2,   1},
  {"stocks_pchanges_c", (DL_FUNC) &stocks_pchanges_c, 3},
  {"stocks_pdiffs_c",   (DL_FUNC) &stocks_pdiffs_c,   3},
  {"stocks_ratios_c",   (DL_FUNC) &stocks_ratios_c,   1},
  {NULL, NULL, 0}
};

void R_init_stocks(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
