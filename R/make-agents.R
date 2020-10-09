#' Agents are constructed with a bias on their binary decision (constant over
#' time) and a set of weights governing how seriously the advice of other agents
#' is taken (modified over time).
#' @param n_agents number of agents to create
#' @param n_decisions number of decisions
#' @param biasMean the mean for the agents' bias distribution (agents' biases
#'   are drawn from normal distributions with mean +/- biasMean)
#' @param biasSD standard deviation for the bias distribution
#' @param sensitivitySD standard deviation for distribution of agents'
#'   sensitivity (mean is 1)
#'
#' @return \code{list(
#'   agents = tibble of agents' decisions, advice, etc. at each time point
#'   graphs = list of agents' trust matrix for each time point
#' )}
#'
#' @importFrom tibble tibble
#' @importFrom stats runif rnorm
makeAgents <- function(
  n_agents = n_agents,
  n_decisions = n_decisions,
  biasMean = 1,
  biasSD = 1,
  sensitivitySD = 1
) {
  bias <- ifelse(runif(n_agents) > .5,
                 rnorm(n_agents, biasMean, biasSD),
                 rnorm(n_agents, -biasMean, biasSD))

  agents <- tibble(id = rep(1:n_agents, n_decisions),
                   decision = rep(1:n_decisions, each = n_agents),
                   sensitivity = pmax(
                     abs(rep(rnorm(n_agents, 1, sensitivitySD), n_decisions)),
                     .00001
                   ),
                   bias = rep(bias[order(bias)], n_decisions),
                   truth = NA_real_,
                   initial = NA_real_,
                   advisor = NA_integer_,
                   advice = NA_real_,
                   weight = NA_real_,
                   final = NA_real_)

  # Set up initial trust weights as .25, .5, .75 at random
  graphs <- list(matrix(as.numeric(cut(runif(n_agents ^ 2), 3))/4, n_agents, n_agents))
  diag(graphs[[1]]) <- 0

  list(agents = agents, graphs = graphs)
}
