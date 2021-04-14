.onUnload <- function(libpath) {
  library.dynam.unload("adviseR", libpath)
}

#' Simulate interactions based on specified data and parameters
#' @param d tbl of data with columns 'advisorIndex', 'choice0', 'choice1',
#'   'advisorAgrees', 'initialConfidence', 'finalConfidence'
#' @param params 1x2 tbl of parameters 'weighted selection' and 'trust update
#'   rate'
#' @param detailed_output whether to return an augmented \code{d} tbl (\code{T})
#'   or a named numeric vector of Mean Squared Errors
#' @param scale_width range of the confidence scale in either direction around 0
#'
#' @return Mean Squared Error of simulation with parameters compared to
#'   empirical data values or augmented \code{d} argument, depending on whether
#'   \code{detailed_output} is requested
#'
#' @details Given an appropriately-formatted tbl, advisor choice and influence
#'   values can be fitted against a model of advisor trust. The trust is updated
#'   using \code{adviseR::trustUpdate} for all trials, and then the choice and
#'   influence errors are calculated based on these underlying trust values.
#'
#'   The advisor choice prediction is based on a sigmoid function where the two
#'   parameters are a 'weighted selection' provided by the user which governs
#'   the slope of the sigmoid, and the current difference in trust values
#'   between the advisors.
#'
#'   The influence values are based on a Bayesian update rule where the
#'   confidence scale is considered to give the probability of correctness.
#'   Initial confidence and advisor reliability (trust) are used to calculate
#'   the final confidence.
#'
#' @importFrom dplyr bind_cols %>% mutate
#' @importFrom rlang .data
#'
#' @export
simulateFromData <- function(
  d,
  params,
  detailed_output = F,
  scale_width = 55
) {
  colnames(params) <- c('weightedSelection', 'trustUpdateRate')

  # Starting trust picked to be about what we expect from judge-advisor system
  # literature. Could change this as a parameter later?
  starting_trust <- .65

  # Set up initial trust matrix
  trust <- matrix(
    starting_trust,
    nrow = nrow(d),
    ncol = length(unique(d$advisorIndex))
  )

  # Simulate the trust updating
  trust <- trustUpdate(
    trust,
    as.integer(d[["advisorIndex"]]),
    as.numeric(d[["advisorAgrees"]]),
    params[["trustUpdateRate"]]
  )

  # Calculate errors
  # What's the probability the chosen advisor was chosen?
  d <- bind_cols(
    d,
    advisor_choice_error(d, trust, params$weightedSelection)
  ) %>%
    mutate(advisor_choice_error = .data$mean_pick - .data$mean_pick_predicted)

  # What's the advice error?
  d <- d %>% mutate(
    c1_scaled = abs(.data$initialConfidence / scale_width / 2) + .5,
    c2_scaled = abs(.data$finalConfidence / scale_width / 2) + .5,
    trust_compressed = pmin(.95, pmax(.05, diag(trust[, .data$advisorIndex]))),
    reliability = if_else(d$advisorAgrees, .data$trust_compressed, 1 - .data$trust_compressed),
    c2_hat_scaled = (.data$c1_scaled * .data$reliability) /
      (.data$c1_scaled * .data$reliability + (1 - .data$c1_scaled) * (1 - .data$reliability)),
    c2_hat = .data$c2_hat_scaled * scale_width * 2 - scale_width,
    advice_taking_error = .data$c2_scaled - .data$c2_hat_scaled
  ) %>%
    mutate(across(
      .cols = c(
        .data$c1_scaled,
        .data$c2_scaled,
        .data$trust_compressed,
        .data$reliability,
        .data$c2_hat_scaled,
        .data$c2_hat,
        .data$advice_taking_error
      ),
      ~ if_else(is.na(.data$advisorAgrees), NA_real_, .))
    )


  if (detailed_output) {
    return(d)
  }

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
#' advisor for given trust values. Assumes an adviseR::sigmoid picking behaviour
#' with slope 3 applied to weighted picked vs unpicked trusts.
#' @param picked the index in \code{trust_vector} of the picked advisor
#' @param unpicked the index in \code{trust_vector} of the unpicked advisor
#' @param trust_vector vector of subjective trust in advisors
#' @param weight sigmoid slope governing steepness and direction of relationship
#'   between trust difference and pick probability
#'
#' @return probability picked advisor is picked over unpicked advisor
advisor_pick_probability <- function(picked, unpicked, trust_vector, weight) {
  if (is.null(nrow(trust_vector))) {
    trusts <- data.frame(
      p = diag(trust_vector[picked], nrow = 1, ncol = 1),
      u = diag(trust_vector[unpicked], nrow = 1, ncol = 1)
    )
  } else {
    trusts <- data.frame(
      p = diag(trust_vector[,picked]),
      u = diag(trust_vector[,unpicked])
    )
  }

  sigmoid((trusts$p - trusts$u), slope = weight)
}

#' Wrapper for the C++ function advisorChoiceError
#' @param d tbl to calculate advisor choice error for
#' @param trust matrix of trust values for each trial
#' @param weight slope of the advisor choice sigmoid function
#' @param nBack number of trials to use for the running average
advisor_choice_error <- function(d, trust, weight, nBack = 5) {
  out <- advisorChoiceError(
    trust, d[["advisorIndex"]], d[["choice0"]], d[["choice1"]], weight, nBack)
  data.frame(
    mean_pick = out[,1],
    mean_pick_predicted = out[,2]
  )
}
