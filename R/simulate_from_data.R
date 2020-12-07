#' Simulate interactions based on specified data and parameters
#' @param d tbl of data with columns 'advisorIndex', 'choice0', 'choice1',
#'   'advisorAgrees', 'confidenceShift'
#' @param params 1x2 tbl of parameters 'confidence slope' and 'trust update
#'   rate'
#' @param detailed_output whether to return an augmented \code{d} tbl (\code{T})
#'   or a named numeric vector of Mean Squared Errors
#'
#' @return Mean Squared Error of simulation with parameters compared to
#'   empirical data values or augmented \code{d} argument, depending on whether
#'   \code{detailed_output} is requested
#'
#' @export
simulateFromData <- function(d, params, detailed_output = F) {
  colnames(params) <- c('weightedSelection', 'trustUpdateRate')

  # Starting trust picked to be about what we expect from judge-advisor system
  # literature. Could change this as a parameter later?
  starting_trust <- .3

  # Set up initial trust matrix
  trust <- matrix(
    starting_trust,
    nrow = nrow(d),
    ncol = length(unique(d$advisorIndex))
  )

  # Mean agreement rate is used to balance agreeing/disagreeing advice. An
  # alternative approach would be to have different update rates following
  # dis/agreement
  mean_agreement <- mean(d$advisorAgrees, na.rm = T)

  # Simulate the trust updating
  for (i in 1:nrow(d)) {
    # Update trust matrix for selected advisor and agreement
    if (i != nrow(d)) {
      trust[i + 1,] <- trust[i,]
      trust[i + 1,][d$advisorIndex[i]] <-
        trust[i,][d$advisorIndex[i]] +
        params[["trustUpdateRate"]] * (d$advisorAgrees[i] - .5)
    }
  }

  # Calculate errors
  # What's the probability the chosen advisor was chosen?
  d$advisor_choice_error <- if_else(
    is.na(d$choice0) | is.na(d$choice1),
    NA_real_,
    {
      picked <- if_else(d$choice0 == d$advisorIndex, d$choice0, d$choice1)
      unpicked <- if_else(d$choice0 == d$advisorIndex, d$choice1, d$choice0)
      1 - advisor_pick_probability(
        picked, unpicked, trust, params$weightedSelection
      )
    }
  )

  # What's the advice error?
  d$advice_taking_error <- if_else(
    is.na(d$advisorAgrees),
    NA_real_,
    {
      predicted_update <-
        (d$advisorAgrees - mean_agreement) * diag(trust[,d$advisorIndex])
      d$confidenceShift.z - predicted_update
    }
  )

  if (detailed_output) return(d)

  MSE <- c(
    mean(d$advisor_choice_error ^ 2, na.rm = T),
    mean(d$advice_taking_error ^ 2, na.rm = T)
  )
  names(MSE) <- c(
    "Advisor choice mean squared error",
    "Advice-taking mean squared error"
  )
  MSE
}

#' Return the probability that the picked advisor is picked over the unpicked
#' advisor for given trust values
#' @param picked the index in \code{trust_vector} of the picked advisor
#' @param unpicked the index in \code{trust_vector} of the unpicked advisor
#' @param trust_vector vector of subjective trust in advisors
#' @param weight weighting parameter used as an exponent for trust values
#' @return probability picked advisor is picked over unpicked advisor
advisor_pick_probability <- function(picked, unpicked, trust_vector, weight) {
  trusts <- data.frame(
    p = diag(trust_vector[,picked]) ^ weight,
    u = diag(trust_vector[,unpicked]) ^ weight
  )
  trusts[,1] / apply(trusts, 1, sum)
}