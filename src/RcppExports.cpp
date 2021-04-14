// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// trustUpdate
NumericMatrix trustUpdate(NumericMatrix trust, NumericVector advisorId, NumericVector advisorAgrees, double updateRate);
RcppExport SEXP _adviseR_trustUpdate(SEXP trustSEXP, SEXP advisorIdSEXP, SEXP advisorAgreesSEXP, SEXP updateRateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type trust(trustSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type advisorId(advisorIdSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type advisorAgrees(advisorAgreesSEXP);
    Rcpp::traits::input_parameter< double >::type updateRate(updateRateSEXP);
    rcpp_result_gen = Rcpp::wrap(trustUpdate(trust, advisorId, advisorAgrees, updateRate));
    return rcpp_result_gen;
END_RCPP
}
// advisorChoiceError
NumericMatrix advisorChoiceError(NumericMatrix trust, IntegerVector advisorIndex, IntegerVector choice0, IntegerVector choice1, double slope, int nBack);
RcppExport SEXP _adviseR_advisorChoiceError(SEXP trustSEXP, SEXP advisorIndexSEXP, SEXP choice0SEXP, SEXP choice1SEXP, SEXP slopeSEXP, SEXP nBackSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type trust(trustSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type advisorIndex(advisorIndexSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type choice0(choice0SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type choice1(choice1SEXP);
    Rcpp::traits::input_parameter< double >::type slope(slopeSEXP);
    Rcpp::traits::input_parameter< int >::type nBack(nBackSEXP);
    rcpp_result_gen = Rcpp::wrap(advisorChoiceError(trust, advisorIndex, choice0, choice1, slope, nBack));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_adviseR_trustUpdate", (DL_FUNC) &_adviseR_trustUpdate, 4},
    {"_adviseR_advisorChoiceError", (DL_FUNC) &_adviseR_advisorChoiceError, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_adviseR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}