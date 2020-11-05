# Utility functions

#' Sigmoid function
#' @param x value
#' @param slope slope steepness parameter
sigmoid <- function(x, slope = 1) {
  1/(1 + exp(-x * slope))
}

#' Inverse sigmoid function
#' @param p probability
#' @param slope slope steepness parameter
sigmoid.inv <- function(p, slope = 1) {
  log(p / (1 - p)) / slope
}
