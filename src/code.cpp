#include <Rcpp.h>
using namespace Rcpp;

//' Calculates the evolving trust scores based on agreement
//'
//' @param trust Matrix of trust values - dims = trials x advisors
//' @param advisorId ids of the advisor giving advice on each trial
//' @param advisorAgrees whether advisor agrees on each trial
//' @param updateRate amount to down/upweight trust given dis/agreement
//' @return New trust values after iterating through and up/downweighting trust
// [[Rcpp::export]]
NumericMatrix trustUpdate(
    NumericMatrix trust,
    NumericVector advisorId,
    NumericVector advisorAgrees,
    double updateRate
) {
  const int nRow = trust.nrow();
  const int nCol = trust.ncol();
  const LogicalVector missingAdvice = is_na(advisorAgrees);
  NumericMatrix out = trust; // avoid in-place modification

  for(int r = 0; r < nRow - 1; r++) {
    for(int c = 0; c < nCol; c++) {
      out(r + 1, c) = out(r, c);
      if(advisorId[r] == c + 1 && !missingAdvice[r])
        out(r + 1, c) += updateRate * (advisorAgrees[r] - .5);
    }
  }
  return out;
}
