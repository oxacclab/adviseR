#' Agents are constructed with a bias on their binary decision (constant over
#' time) and a set of weights governing how seriously the advice of other agents
#' is taken (modified over time).
#' @param n_agents number of agents to create
#' @param n_decisions number of decisions
#' @param bias_mean the mean for the agents' bias distribution (agents' biases
#'   are drawn from normal distributions with mean +/- biasMean). Translated via
#'   sigmoid function to between 0 and 1, and represents the prior probability
#'   that the answer is 1.
#' @param bias_sd standard deviation for the bias distribution
#' @param sensitivity_sd standard deviation for distribution of agents'
#'   sensitivity (mean is 1)
#' @param trust_volatility_mean the mean volatility of agents' trust
#' @param trust_volatility_sd standard deviation
#' @param bias_volatility_mean the mean volatility of agents' biases (move this
#'   proportion towards the final decision value from current bias at each step)
#' @param bias_volatility_sd standard deviation
#' @param confidence_slope_mean the mean of the distribution from which agents
#'   take their slopes for the sigmoid function mapping continuous evidence to
#'   a probability of a categorical decision.
#' @param confidence_slope_sd standard deviation
#' @param weighted_sampling_mean a non-zero value means agents choose who to
#'   seek advice from according to how likely they are to trust the advice. The
#'   weights are raised to the power of this value (so values > 1 make source
#'   selection more pronounced than advice weighting, and values < 1 make source
#'   selection less pronounced than advice weighting). Negative values will make
#'   agents actively seek out those they do not trust for advice.
#' @param weighted_sampling_sd standard deviation
#' @param starting_graph single number, vector, or n_agents-by-n_agents matrix
#'   of starting trust weights between agents. Coerced to numeric. Can also be
#'   a function taking the first generation of the agents tbl as an input and
#'   returning an n-by-n matrix of trust values between 0 and 1, where n is the
#'   number of agents, 0 represents completely untrustworthy, .5 random, and 1
#'   completely trustworthy.
#'
#'
#' @details the \code{agents} tibble is an n_agents*n_decisions by 12 table with
#' \itemize{
#'  \item{"id"}{The agent's identifier}
#'  \item{"decision"}{The decision number}
#'  \item{"sensitivity"}{The agent's ability to do the task}
#'  \item{"trust_volatility"}{How quickly the agent's trust updates}
#'  \item{"bias_volatility"}{How quickly the agent's bias updates}
#'  \item{"weighted_sampling"}{How heavily trust governs advice sampling behaviour}
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
  bias_mean = 0,
  bias_sd = 1,
  sensitivity_sd = 1,
  trust_volatility_mean = .05,
  trust_volatility_sd = .01,
  bias_volatility_mean = .05,
  bias_volatility_sd = .01,
  confidence_slope_mean = 1,
  confidence_slope_sd = 0,
  weighted_sampling_mean = 0,
  weighted_sampling_sd = 0,
  starting_graph = NULL
) {
  bias <- sigmoid(rnorm(n_agents, sign(runif(n_agents) - .5) * bias_mean, bias_sd))

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
    weighted_sampling = rep(
      rnorm(n_agents, weighted_sampling_mean, weighted_sampling_sd),
      n_decisions
    ),
    bias = rep(bias[order(bias)], n_decisions),
    truth = NA_real_,
    percept = NA_real_,
    initial = NA_real_,
    advisor = NA_integer_,
    advice = NA_real_,
    weight = NA_real_,
    final = NA_real_,
    confidence_slope = rep(
      rnorm(n_agents, confidence_slope_mean, confidence_slope_sd),
      n_decisions
    )
  )
  agents <- mutate(
    agents,
    across(c(ends_with('volatility'), "confidence_slope"), ~ abs(.)),
  )

  if (!is.null(starting_graph)) {
    if (length(starting_graph) == 1) {
      if ('function' %in% class(starting_graph)) {
        graph <- starting_graph(agents[agents$decision == 1, ])
      } else {
        graph <- matrix(as.numeric(starting_graph), n_agents, n_agents)
      }
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
