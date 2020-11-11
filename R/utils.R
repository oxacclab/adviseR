# Utility functions

#' Sigmoid function
#' @param x value
#' @param slope slope steepness parameter
#' @return probability of selecting positive over negative direction given x
sigmoid <- function(x, slope = 1) {
  1/(1 + exp(-x * slope))
}
