#' Run the simulation
#' @param n_agents number of nodes in the network
#' @param n_decisions number of decisions to simulate
#' @param conf whether or not agents use confidence to update trust
#' @param bias_mean the mean for the agents' bias distribution (agents' biases
#'   are drawn from normal distributions with mean +/- biasMean)
#' @param bias_sd standard deviation for the bias distribution
#' @param sensitivity_sd standard deviation for distribution of agents'
#'   sensitivity (mean is 1)
#' @param trust_volatility_mean the mean volatility of agents' trust
#' @param trust_volatility_sd standard deviation
#' @param bias_volatility_mean the mean volatility of agents' biases (move this
#'   proportion towards the final decision value from current bias at each step)
#' @param bias_volatility_sd standard deviation
#' @param starting_graph single number, vector, or n_agents-by-n_agents matrix
#'   of starting trust weights between agents. Coerced to numeric
#' @param randomSeed the random seed to start the simulation with
#' @param truth_fun function taking the simulation and decision number as
#'   arguments and returning the true state of the world as a single number
#' @param weighted_sampling a non-zero/NA value means agents choose who to seek
#'   advice from according to how likely they are to trust the advice. The
#'   weights are multiplied by this value (so values > 1 make source selection
#'   more pronounced than advice weighting, and values < 1 make source selection
#'   less pronounced than advice weighting). Negative values will make agents
#'   actively seek out those they do not trust for advice.
#' @param asymptotic_confidence whether to constrain confidence in decision to
#'   be between 0 (certain left) and 1 (certain right), as determined by a
#'   sigmoid function. If this is a length 2 vector it contains the mean and SD
#'   of the distribution from which agent sigmoid slopes are drawn. If this is
#'   a function, it is called with the agents' details and should return the
#'   slopes as a vector. \code{\link{abs}} is used to avoid negative slopes.
#'   If this is FALSE, decision confidence is simply the agent's estimate of the
#'   state of the world.
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
  conf = T,
  bias_mean = 1,
  bias_sd = 1,
  sensitivity_sd = 1,
  trust_volatility_mean = .05,
  trust_volatility_sd = .01,
  bias_volatility_mean = .05,
  bias_volatility_sd = .01,
  starting_graph = NULL,
  randomSeed = NA,
  truth_fun = function(model, d) stats::rnorm(1),
  weighted_sampling = NA,
  asymptotic_confidence = c(0,1)
) {

  # Compensate for parallel:: coercing logical to numeric
  if (length(asymptotic_confidence) == 1 &
      typeof(asymptotic_confidence) != 'closure')
    asymptotic_confidence <- as.logical(asymptotic_confidence)

  # print(paste0(
  #   "Running simulation: ",
  #   "; AgentCount = ", n_agents,
  #   "; DecisionCount = ", n_decisions,
  #   "; BiasMean = ", bias_mean,
  #   " (SD = ", bias_sd, ")",
  #   "; sensitivitySD = ", sensitivity_sd,
  #   "; TrustVolatility = ", trust_volatility_mean,
  #   " (SD = ", trust_volatility_sd, ")",
  #   "; BiasVolatility = ", bias_volatility_mean,
  #   " (SD = ", bias_volatility_sd, ")",
  # ))

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
          conf = as.logical(conf),
          bias_mean = bias_mean,
          bias_sd = bias_sd,
          sensitivity_sd = sensitivity_sd,
          trust_volatility_mean = trust_volatility_mean,
          trust_volatility_sd = trust_volatility_sd,
          bias_volatility_mean = bias_volatility_mean,
          bias_volatility_sd = bias_volatility_sd,
          starting_graph_type = class(starting_graph)[1],
          starting_graph = starting_graph,
          randomSeed = .Random.seed[length(.Random.seed)],
          truth_fun = truth_fun,
          weighted_sampling = weighted_sampling,
          asymptotic_confidence = asymptotic_confidence
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
        starting_graph = starting_graph,
        asymptotic_confidence = asymptotic_confidence
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
#' bias plus noise. They then give their opinion to another agent, and receive
#' another agent's opinion as advice. The advice is integrated according to the
#' current weight placed on the other agent's trustworthiness. This weight is
#' then updated according to whether the agents agree, weighted by the
#' confidence of the deciding agent.
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
  agents$initial <-
    agents$truth +    # true value
    agents$bias +     # bias
    # normally distributed noise with sd = 1/sensitivity
    rnorm(model$parameters$n_agents, 0, 1/agents$sensitivity)

  if (hasName(agents, 'initialConfidence'))
    agents$initialConfidence <- sigmoid(agents$initial, agents$confSlope)

  # Advice

  # pick an advisor
  w <- model$parameters$weighted_sampling
  if (is.na(w) || w == 0) {
    probabilities <- matrix(
      1,
      nrow = model$parameters$n_agents,
      ncol = model$parameters$n_agents
    )
  } else {
    probabilities <- model$model$graphs[[d]] ^ w
  }
  # never ask yourself - set diag to just below minimum value
  # this approach supports negative values of w without self-seeking
  diag(probabilities) <- apply(probabilities, 1, min) - .0001
  agents$advisor <- sapply(agents$id, function(i)
    base::sample(
      col(probabilities)[i, ], # ids are column numbers
      size = 1,
      prob = probabilities[i, ] - min(probabilities[i, ]) # self prob to zero
    )
  )

  agents$weight <- diag(model$model$graphs[[d]][agents$advisor, ])

  if (hasName(agents, 'initialConfidence')) {
    agents$advice <- agents$initialConfidence[agents$advisor]
    agents$finalConfidence <- (agents$initialConfidence * (1 - agents$weight)) +
      (agents$advice * agents$weight)
    agents$final <- sigmoid.inv(agents$finalConfidence)
  } else {
    agents$advice <- agents$initial[agents$advisor]
    agents$final <-
      (agents$initial * (1 - agents$weight)) +
      (agents$advice * agents$weight)
  }

  # Write output to the model
  model$model$agents[rows, ] <- agents

  # Updating bias for next time
  if (max(rows) != nrow(model$model$agents)) {
    # Nudge bias towards observed (i.e. based on final decision) truth
    model$model$agents[rows + model$parameters$n_agents, "bias"] <-
      agents$bias * (1 - agents$bias_volatility) +
      agents$final * agents$bias_volatility
  }

  # Updating weights
  if (hasName(agents, 'initialConfidence')) {
    initial <- agents$initialConfidence
    agree <- (initial > .5) == (agents$advice > .5)
  } else {
    initial <- agents$initial
    agree <- (initial > 0) == (agents$advice > 0)
  }
  newWeights <- as.vector(model$model$graphs[[d]])
  if (model$parameters$conf) {
    newWeights[(agents$id - 1) * model$parameters$n_agents + agents$advisor] <-
      newWeights[(agents$id - 1) * model$parameters$n_agents + agents$advisor] +
      ifelse(
        agree,
        agents$trust_volatility * abs(initial), # agree
        -agents$trust_volatility * abs(initial) # disagree
      )
  } else {
    newWeights[(agents$id - 1) * model$parameters$n_agents + agents$advisor] <-
      newWeights[(agents$id - 1) * model$parameters$n_agents + agents$advisor] +
      ifelse(
        agree,
        agents$trust_volatility,
        -agents$trust_volatility
      )
  }
  newWeights <- pmax(0.0001, pmin(1, newWeights))
  newWeights <- matrix(
    newWeights,
    model$parameters$n_agents,
    model$parameters$n_agents
  )
  diag(newWeights) <- 0
  model$model$graphs[[d + 1]] <- newWeights

  model
}
