#' Run the simulation
#' @param n_agents number of nodes in the network
#' @param n_decisions number of decisions to simulate
#' @param conf whether or not agents use confidence to update trust
#' @param biasMean the mean for the agents' bias distribution (agents' biases
#'   are drawn from normal distributions with mean +/- biasMean)
#' @param biasSD standard deviation for the bias distribution
#' @param sensitivitySD standard deviation for distribution of agents'
#'   sensitivity (mean is 1)
#' @param learningRate size of the trust update at each time step
#' @param randomSeed the random seed to start the simulation with
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
  biasMean = 1,
  biasSD = 1,
  sensitivitySD = 1,
  learningRate = .1,
  randomSeed = NA
) {

  # print(paste0(
  #   "Running simulation: ",
  #   "; AgentCount = ", n_agents,
  #   "; DecisionCount = ", n_decisions,
  #   "; BiasMean = ", biasMean,
  #   " (SD = ", biasSD, ")",
  #   "; sensitivitySD = ", sensitivitySD,
  #   "; learningRate = ", learningRate
  # ))

  if (is.na(randomSeed))
    randomSeed = runif(1, 1e6, 1e12)  # random random seed

  with_seed(
    randomSeed,
    {
      out <- list(
        times = list(
          start = Sys.time()
        ),
        parameters = list(
          n_agents = n_agents,
          n_decisions = n_decisions,
          conf = as.logical(conf),
          biasMean = biasMean,
          biasSD = biasSD,
          sensitivitySD = sensitivitySD,
          learningRate = learningRate,
          randomSeed = .Random.seed[length(.Random.seed)]
        )
      )

      # Construct the agents
      out$model <- makeAgents(
        n_agents = n_agents,
        n_decisions = n_decisions,
        biasMean = biasMean,
        biasSD = biasSD,
        sensitivitySD = sensitivitySD
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
#' @param params dataframe of parameters for simulations (see \code{runSimulation()} for details)
#' @param cores number of cores to use in the cluster
#' @inheritDotParams parallel::makeCluster
#'
#' @importFrom parallel makeCluster stopCluster parLapply
#'
#' @export
runSimulations <- function(params, cores = parallel::detectCores(), ...) {
  cl <- parallel::makeCluster(cores, ...)
  out <- parallel::parApply(cl, params, 1, function(p) {
    library(adviseR)
    do.call(runSimulation, as.list(p))
  })
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
#'
#' @return model updated to include values for decision d
simulationStep <- function(model, d) {
  # identify the agent tibble rows corresponding to decision d
  rows <- (
    ((d - 1) * model$parameters$n_agents):(d * model$parameters$n_agents - 1)
    ) + 1

  agents <- model$model$agents[rows, ]

  # Truth
  agents$truth <- rnorm(1)  # single true value for all agents

  # Initial decisions
  agents$initial <-
    agents$truth +    # true value
    agents$bias +     # bias
    rnorm(model$parameters$n_agents, 0, 1/agents$sensitivity) # normally distributed noise with sd = 1/sensitivity

  # Advice
  agents$advisor <- sapply(agents$id, function(i)
    base::sample((1:model$parameters$n_agents)[-i], # never ask yourself!
                 1))

  agents$weight <- diag(model$model$graphs[[d]][agents$advisor, ])

  agents$advice <-
    agents$initial[agents$advisor]

  agents$final <-
    (agents$initial * (1 - agents$weight)) +
    (agents$advice * agents$weight)

  # Write output to the model
  model$model$agents[rows, ] <- agents

  # Updating weights
  newWeights <- as.vector(model$model$graphs[[d]])
  if (model$parameters$conf) {
    newWeights[(agents$id - 1) * model$parameters$n_agents + agents$advisor] <-
      newWeights[(agents$id - 1) * model$parameters$n_agents + agents$advisor] +
      ifelse((agents$initial > 0) == (agents$advice > 0),
             model$parameters$learningRate * abs(agents$initial), # agree
             -model$parameters$learningRate * abs(agents$initial)) # disagree
  } else {
    newWeights[(agents$id - 1) * model$parameters$n_agents + agents$advisor] <-
      newWeights[(agents$id - 1) * model$parameters$n_agents + agents$advisor] +
      ifelse((agents$initial > 0) == (agents$advice > 0),
             model$parameters$learningRate, -model$parameters$learningRate)
  }
  newWeights <- pmax(0.0001, pmin(1, newWeights))
  newWeights <- matrix(newWeights, model$parameters$n_agents, model$parameters$n_agents)
  diag(newWeights) <- 0
  model$model$graphs[[d + 1]] <- newWeights

  model
}
