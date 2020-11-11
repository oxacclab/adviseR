#' Run the simulation
#' @param n_agents number of nodes in the network
#' @param n_decisions number of decisions to simulate
#' @param bias_mean the mean for the agents' bias distribution (agents' biases
#'   are drawn from normal distributions with mean +/- biasMean). Capped to
#'   between 0 and 1, and represents the prior probability that the answer is 1.
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
#'   a probability of a categorical decision. \code{\link{abs}} is used to
#'   avoid negative slopes.
#' @param confidence_slope_sd standard deviation
#' @param starting_graph single number, vector, or n_agents-by-n_agents matrix
#'   of starting trust weights between agents. Coerced to numeric
#' @param randomSeed the random seed to start the simulation with
#' @param truth_fun function taking the simulation and decision number as
#'   arguments and returning the true state of the world as a single number
#' @param truth_sd standard deviation of the truth function that the agents use
#'   to calculate the probability of values given their biases.
#' @param weighted_sampling a non-zero/NA value means agents choose who to seek
#'   advice from according to how likely they are to trust the advice. The
#'   weights are multiplied by this value (so values > 1 make source selection
#'   more pronounced than advice weighting, and values < 1 make source selection
#'   less pronounced than advice weighting). Negative values will make agents
#'   actively seek out those they do not trust for advice.
#'
#' @return a list with \itemize{
#'  \item{"times"}{Timestamps associated with simulation stages.}
#'  \item{"parameters"}{Parameters input into the model}
#'  \item{"model"}{A list of model content \itemize{
#'    \item{"agents"}{A tbl with 1 row for each decision made by an agent.}
#'    \item{"graphs"}{Annotated igraphs of trust strength between agents at each
#'    generation.}
#'  }}
#' }
#'
#' @importFrom stats runif
#' @importFrom withr with_seed
#'
#' @export
runSimulation <- function(
  n_agents = 6,
  n_decisions = 200,
  bias_mean = 0,
  bias_sd = 1,
  sensitivity_sd = 1,
  trust_volatility_mean = .05,
  trust_volatility_sd = .01,
  bias_volatility_mean = .05,
  bias_volatility_sd = .01,
  confidence_slope_mean = 1,
  confidence_slope_sd = 0,
  starting_graph = NULL,
  randomSeed = NA,
  truth_fun = function(model, d) stats::rnorm(1, 0, model$parameters$truth_sd),
  truth_sd = .5,
  weighted_sampling = NA
) {

  if (is.na(randomSeed))
    randomSeed = round(runif(1, 1e6, 1e8))  # random random seed

  with_seed(
    as.integer(randomSeed),
    {
      out <- list(
        times = list(
          start = Sys.time()
        ),
        parameters = list(
          n_agents = n_agents,
          n_decisions = n_decisions,
          bias_mean = bias_mean,
          bias_sd = bias_sd,
          sensitivity_sd = sensitivity_sd,
          trust_volatility_mean = trust_volatility_mean,
          trust_volatility_sd = trust_volatility_sd,
          bias_volatility_mean = bias_volatility_mean,
          bias_volatility_sd = bias_volatility_sd,
          confidence_slope_mean = confidence_slope_mean,
          confidence_slope_sd = confidence_slope_sd,
          starting_graph_type = class(starting_graph)[1],
          starting_graph = starting_graph,
          randomSeed = .Random.seed[length(.Random.seed)],
          truth_fun = truth_fun,
          truth_sd = truth_sd,
          weighted_sampling = weighted_sampling
        )
      )

      # Construct the agents
      out$model <- makeAgents(
        n_agents = n_agents,
        n_decisions = n_decisions,
        bias_mean = bias_mean,
        bias_sd = bias_sd,
        sensitivity_sd = sensitivity_sd,
        trust_volatility_mean = trust_volatility_mean,
        trust_volatility_sd = trust_volatility_sd,
        bias_volatility_mean = bias_volatility_mean,
        bias_volatility_sd = bias_volatility_sd,
        confidence_slope_mean = confidence_slope_mean,
        confidence_slope_sd = confidence_slope_sd,
        starting_graph = starting_graph
      )

      out$times$agentsCreated <- Sys.time()

      # Run the model
      for (d in 1:n_decisions)
        out <- simulationStep(out, d)
    }
  )

  out$times$end <- Sys.time()

  detailGraphs(out)
}

#' Run a suite of simulations defined by params
#' @param params dataframe of parameters for simulations
#'   (see \code{\link{runSimulation}} for details). Can also be a list of lists
#'   to support custom function arguments
#' @param cores number of cores to use in the cluster
#' @inheritDotParams parallel::makeCluster
#'
#' @importFrom parallel makeCluster stopCluster parLapply
#'
#' @export
runSimulations <- function(params, cores = parallel::detectCores(), ...) {
  # Unpack arguments and call runSimulation
  f <- function(p) {
    library(adviseR)
    do.call(runSimulation, as.list(p))
  }

  cl <- parallel::makeCluster(cores, ...)
  if ('data.frame' %in% class(params)) {
    out <- parallel::parApply(cl, params, 1, f)
  } else {
    out <- parallel::parLapply(cl, params, f)
  }
  parallel::stopCluster(cl)
  out
}

#' Each timestep agents are asked for a binary decision about whether a variable
#' is > 0. They form a noisy initial decision based on the true value plus their
#' noise based on their sensitivity. This decision is coded as 0 (v < 0) or 1 (
#' v > 0).
#' They then weight this evidence by their bias (prior probability of 0 vs 1).
#' They then give their opinion to another agent as a binary endorsement (0 or
#' 1), and receive another agent's opinion as advice.
#' The advice is integrated according to the current weight placed on the other
#' agent's trustworthiness. This weight is then updated according to the
#' agent's trust in their advisor.
#' The bias is updated depending on the final decision.
#' The weight in the advisor is updated depending upon the plausibility of the
#' advice given the initial estimate.
#' @param model to simulate the step for
#' @param d decision to simulate
#'
#' @importFrom stats rnorm
#' @importFrom utils hasName
#'
#' @return model updated to include values for decision d
simulationStep <- function(model, d) {
  # identify the agent tibble rows corresponding to decision d
  rows <- (
    ((d - 1) * model$parameters$n_agents):(d * model$parameters$n_agents - 1)
    ) + 1

  agents <- model$model$agents[rows, ]

  # Truth
  # single true value for all agents
  agents$truth <- model$parameters$truth_fun(model, d)[[1]]

  # Initial decisions
  agents$percept <- getPercept(agents)

  # Initial confidence
  agents$initial <- getConfidence(agents, model$parameters$truth_sd)

  # Select advisor
  agents$advisor <-
    selectAdvisor(model$model$graphs[[d]], model$parameters$weighted_sampling)

  agents$weight <- diag(model$model$graphs[[d]][, agents$advisor])

  # Advice
  agents$advice <- round(agents$initial[agents$advisor]) # advice is 0 or 1

  # Final decision
  agents$final <- weighted(agents$advice, agents$initial, agents$weight)

  # Write output to the model
  model$model$agents[rows, ] <- agents

  # Updating bias for next time
  if (max(rows) != nrow(model$model$agents)) {
    # Nudge bias towards observed (i.e. based on final decision) truth
    model$model$agents[rows + model$parameters$n_agents, "bias"] <-
      weighted(agents$final, agents$bias, agents$bias_volatility)
  }

  # Updating weights
  model$model$graphs[[d + 1]] <-
    newWeights(agents, model$model$graphs[[d]], model$parameters$truth_sd)

  model
}

#' Return the percept of the \code{agents} given some truth
#' @param agents tbl with snapshot of agents at a given decision time
#' @details \code{agents} should have the fields \code{$truth} and
#'   \code{$sensitivity}
#'
#' @importFrom stats rnorm
#'
#' @return agents' percepts
getPercept <- function(agents) {
  agents$truth +    # true value
    # normally distributed noise with sd = 1/sensitivity
    rnorm(nrow(agents), 0, 1/agents$sensitivity)
}

#' Return the confidence of the \code{agents} given a percept, confidence
#' steepness, and bias.
#' @param agents tbl with a snapshot of agents at a given decision time
#' @param truth_sd agents' beliefs about the world's variability
#' @details \code{agents} should have the fields \code{$percept},
#'   \code{$confidence_slope}, and \code{$bias}
#'
#' @importFrom stats dnorm
#'
#' @return agents' confidence the answer is 1 vs 0 as a proportion.
getConfidence <- function(agents, truth_sd) {
  percept <- sigmoid(agents$percept, agents$confidence_slope)
  # initial confidence is a bayesian combination of
  # P(R|data) = (P(R) * P(data|R)) / (P(L) * P(data|L))
  pRight <- agents$bias * dnorm(percept, 1, truth_sd)
  pLeft <- (1 - agents$bias) * dnorm(percept, 0, truth_sd)
  # return initial decision with confidence
  pRight / (pRight + pLeft)
}

#' Return the advisor selections based on \code{graph}
#' @param graph weighted trust matrix for agents
#' @param exponent power to which trust weights are raised for probabilistic
#'   selection; NA or 0 means selection is equally weighted
#' @return vector of the advisor id selected by each agent
selectAdvisor <- function(graph, exponent = 1) {
  # pick an advisor
  if (is.na(exponent) || exponent == 0) {
    probabilities <- matrix(1, nrow = nrow(graph), ncol = ncol(graph))
  } else {
    probabilities <- graph ^ exponent
  }
  # never ask yourself - set diag to just below minimum value
  # this approach supports negative values of exponent without self-seeking
  diag(probabilities) <- apply(probabilities, 1, min) - .0001
  sapply(1:nrow(graph), function(i)
    sample(
      col(probabilities)[i, ], # ids are column numbers
      size = 1,
      prob = probabilities[i, ] - min(probabilities[i, ]) # self prob to zero
    )
  )
}

#' Weighted average where \code{a} uses \code{weight_a} and \code{b} uses
#' \code{1 - weight_a}
#' @param a numeric
#' @param b numeric
#' @param weight_a weight given to a (1-\code{weight_a}) is used for \code{b}
weighted <- function(a, b, weight_a) {
  a * weight_a + b * (1 - weight_a)
}

#' Return the new trust matrix for agents
#' @param agents tbl with a snapshot of agents at a given time
#' @param graph connectivity matrix of trust
#' @param truth_sd agents' beliefs about the variability of the world
#'
#' @details Agents work out how expected advice was given their initial
#' estimate, and in/decrease their trust according to that likelihood, scaled
#' by each agent's trust_volatility.
#'
#' @importFrom stats pnorm
#'
#' @return Connectivity matrix of trust after accounting for agents' decisions
newWeights <- function(agents, graph, truth_sd) {
  n_agents <- nrow(graph)
  # how expected was the advice | initial estimate?
  pAdvice <- pnorm(.5, agents$initial, truth_sd)
  # shift so -ve adjustment possible
  pAdvice <- ifelse(agents$advice, pAdvice, 1 - pAdvice) - .5

  W <- as.vector(graph)
  W[(agents$id - 1) * n_agents + agents$advisor] <-
    W[(agents$id - 1) * n_agents + agents$advisor] +
    pAdvice * agents$trust_volatility

  W <- pmax(0.0001, pmin(1, W)) # cap weights
  W <- matrix(W, n_agents, n_agents)
  diag(W) <- 0
  W
}
