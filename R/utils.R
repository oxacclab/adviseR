# Utility functions

#' Sigmoid function
#' @param x value
#' @param slope slope steepness parameter
#' @param nudge adjustments applied if p is very close to 0 or 1
#' @return probability of selecting positive over negative direction given x
sigmoid <- function(x, slope = 1, nudge = 1e-8) {
  y <- 1/(1 + exp(-x * slope))
  y[y == round(y)] <- ifelse(y == 0, y + nudge, y - nudge)
  y
}

#' Inverse sigmoid function
#' @param p probability
#' @param slope slope steepness parameter
sigmoid.inv <- function(p, slope = 1) {
  log(p / (1 - p)) / slope
}
