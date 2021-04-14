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

//' Calculate choice error averaged over the last 5 trials
//' @param trust Matrix of trust values - dims = trials x advisors
//' @param advisorIndex indices of chosen advisors
//' @param choice0 indices of the advisor in the first choice slot
//' @param choice1 indices of the advisor in the second choice slot
//' @param slope slope of the sigmoid function
//' @param nBack number of trials to look back. Looks at previous trials where
//'   the advisorIndex advisor is in one of the choice slots.
//'
//' @return Matrix of mean pick rate for the advisorIndex advisor (column 1) and
//'   mean predicted pick rate for that advisor (column 2)
// [[Rcpp::export]]
NumericMatrix advisorChoiceError(
  NumericMatrix trust,
  IntegerVector advisorIndex,
  IntegerVector choice0,
  IntegerVector choice1,
  double slope,
  int nBack = 5
) {
  const int nRow = trust.nrow();

  NumericMatrix out(nRow, 2);

  for(int t = 0; t < nRow; t++) {
    // Register NA if there is no choice
    bool okay = true;
    if(IntegerVector::is_na(advisorIndex[t]) ||
       IntegerVector::is_na(choice0[t]) ||
       IntegerVector::is_na(choice1[t]))
      okay = false;
    else if(choice0[t] != advisorIndex[t] && choice1[t] != advisorIndex[t])
      okay = false;
    if(!okay) {
      out(t, _) = NumericVector {NA_REAL, NA_REAL};
      continue;
    }
    // Collect appropriate previous trials
    NumericVector pick_probs;
    NumericVector predictions;
    for(int i = t; i >= 0 && i > t - nBack; i--) {
      if(IntegerVector::is_na(advisorIndex[i]) ||
         IntegerVector::is_na(choice0[i]) ||
         IntegerVector::is_na(choice1[i]))
        continue;
      if(choice0[i] == advisorIndex[t] || choice1[i] == advisorIndex[t]) {
        // Note whether our advisor was actually picked
        pick_probs.push_front((double) (advisorIndex[i] == advisorIndex[t]));

        // Calculate prediction based on sigmoid of trust differences
        const double trustPicked = trust(i, advisorIndex[t] - 1);
        const int otherIndex = choice0[i] == advisorIndex[t]? choice1[i] : choice0[i];
        const double trustOther = trust(i, otherIndex - 1);
        const double difference = trustPicked - trustOther;
        const double pPickPicked = 1/(1 + exp(-difference * slope));
        predictions.push_front(pPickPicked);
      }
    }

    out(t, _) = NumericVector {mean(pick_probs), mean(predictions)};
  }

  return out;
}
