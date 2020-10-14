#' Agents are constructed with a bias on their binary decision (constant over
#' time) and a set of weights governing how seriously the advice of other agents
#' is taken (modified over time).
#' @param n_agents number of agents to create
#' @param n_decisions number of decisions
#' @param bias_mean the mean for the agents' bias distribution (agents' biases
#'   are drawn from normal distributions with mean +/- biasMean)
#' @param bias_sd standard deviation for the bias distribution
#' @param sensitivity_sd standard deviation for distribution of agents'
#'   sensitivity (mean is 1)
#' @param trust_volatility_mean the mean volatility of agents' trust
#' @param trust_volatility_sd standard deviation
#' @param bias_volatility_mean the mean volatility of agents' biases
#' @param bias_volatility_sd standard deviation
#' @param starting_graph single number, vector, or n_agents-by-n_agents matrix
#'   of starting trust weights between agents. Coerced to numeric
#'
#' @details the \code{agents} tibble is an n_agents*n_decisions by 12 table with
#' \itemize{
#'  \item{"id"}{The agent's identifier}
#'  \item{"decision"}{The decision number}
#'  \item{"sensitivity"}{The agent's ability to do the task}
#'  \item{"trust_volatility"}{How quickly the agent's trust updates}
#'  \item{"bias_volatility"}{How quickly the agent's bias updates}
#'  \item{"bias"}{The agent's (initial) bias}
#'  \item{"truth"}{The true state of the world (same for a given decision for all agents)}
#'  \item{"initial"}{The agent's initial estimate of the truth}
#'  \item{"advisor"}{ID of the agent whose advice is received}
#'  \item{"weight"}{The weight assigned to the advice by the agent}
#'  \item{"final"}{The final decision of the agent}
#' }
#'
#' @return \code{list(
#'   agents = tibble of agents' decisions, advice, etc. at each time point
#'   graphs = list of agents' trust matrix for each time point
#' )}
#'
#' @importFrom tibble tibble
#' @importFrom dplyr mutate across ends_with
#' @importFrom stats runif rnorm
makeAgents <- function(
  n_agents = n_agents,
  n_decisions = n_decisions,
  bias_mean = 1,
  bias_sd = 1,
  sensitivity_sd = 1,
  trust_volatility_mean = .05,
  trust_volatility_sd = .01,
  bias_volatility_mean = .05,
  bias_volatility_sd = .01,
  starting_graph = NULL
) {
  bias <- ifelse(runif(n_agents) > .5,
                 rnorm(n_agents, bias_mean, bias_sd),
                 rnorm(n_agents, -bias_mean, bias_sd))

  agents <- tibble(
    id = rep(1:n_agents, n_decisions),
    decision = rep(1:n_decisions, each = n_agents),
    sensitivity = pmax(
      abs(rep(rnorm(n_agents, 1, sensitivity_sd), n_decisions)),
      .00001
    ),
    trust_volatility = rep(
      rnorm(n_agents, trust_volatility_mean, trust_volatility_sd),
      n_decisions
    ),
    bias_volatility = rep(
      rnorm(n_agents, bias_volatility_mean, bias_volatility_sd),
      n_decisions
    ),
    bias = rep(bias[order(bias)], n_decisions),
    truth = NA_real_,
    initial = NA_real_,
    advisor = NA_integer_,
    advice = NA_real_,
    weight = NA_real_,
    final = NA_real_
  )
  agents <- mutate(
    agents,
    across(ends_with('volatility'), ~ abs(.))
  )

  if (!is.null(starting_graph)) {
    if (length(starting_graph) == 1) {
      graph <- matrix(as.numeric(starting_graph), n_agents, n_agents)
    } else {
      graph <- matrix(as.numeric(starting_graph), n_agents, n_agents)
    }
  } else {
    # Set up initial trust weights as .25, .5, .75 at random
    graph <- matrix(as.numeric(cut(runif(n_agents ^ 2), 3))/4, n_agents, n_agents)
  }

  diag(graph) <- 0

  list(agents = agents, graphs = list(graph))
}
