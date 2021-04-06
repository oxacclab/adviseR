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
        /* This is the general integration equation used for e.g. advice
         * integration in the models.
         *
         * T_t+1 = T_t * (1 - L) + AL
         *
         * Where T is the trust value at a given time, L is the learning rate,
         * and A is 0 if the advisor disagrees and 1 if the advisor agrees.
         * The function thus steps towards 0 or 1 by weighted average with the
         * current value (weighted by the learning rate)
         */
        out(r + 1, c) = out(r, c) * (1 - updateRate) + updateRate * advisorAgrees[r];
    }
  }
  return out;
}
