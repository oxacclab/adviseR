# Utility functions

#' Sigmoid function
#' @param x value
#' @param slope slope steepness parameter
#' @return probability of selecting positive over negative direction given x
sigmoid <- function(x, slope = 1) {
  1/(1 + exp(-x * slope))
}


#' Recycle x to length n
#' @param x vector to resize
#' @param n length of output
recycle <- function(x, n) {
  mod <- n %% length(x)
  if (mod != 0)
    warning(paste0(
      'Recycling vector of length ', length(x), ' into length ', n,
      ' cannot be done exactly.'
    ))
  times <- floor(n / length(x))
  out <- rep(x, times)
  if (mod != 0)
    c(out, x[1:mod])
  else
    out
}
