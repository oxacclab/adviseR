#' Run the simulation
#' @param n_agents number of nodes in the network
#' @param n_decisions number of decisions to simulate
#' @param decision_flags numeric vector recycled to \code{length(n_decisions)}
#'   containing boolean flags for which components of the simulation to run at
#'   each decision point.
#'   This vector can be used to e.g. run several decisions without updating
#'   biases before allowing biases to update on the resulting trust network.
#'   Flag values are 1 = trust update; 2 = bias update. Defaults to all.
#' @param bias_mean the mean for the agents' bias distribution (agents' biases
#'   are drawn from normal distributions with mean +/- bias_mean). Fed into a
#'   sigmoid function and the capped to between 0 and 1. Represents the prior
#'   probability that the answer is 1.
#' @param bias_sd standard deviation for the bias distribution
#' @param sensitivity_mean mean for agents' sensitivity
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
#' @param weighted_sampling_mean a non-zero value means agents choose who to
#'   seek advice from according to how likely they are to trust the advice. The
#'   weights are raised to the power of this value (so values > 1 make source
#'   selection more pronounced than advice weighting, and values < 1 make source
#'   selection less pronounced than advice weighting). Negative values will make
#'   agents actively seek out those they do not trust for advice.
#' @param weighted_sampling_sd standard deviation
#' @param bias_update_slope value used (for all agents) as the slope of the bias
#'   update function
#' @param feedback_probability the proportion of trials on which feedback
#'   occurs. Feedback is a broadcast signal revealing the true world value on a
#'   given trial (perhaps with some noise). All agents implicitly trust the
#'   feedback.
#' @param feedback_proportion the proportion of agents to which feedback is
#'   given when it occurs
#' @param feedback_sd the standard deviation of the noise around the feedback.
#'   If this value is non-zero, the same approximate value is given to all
#'   agents receiving feedback that round.
#' @param starting_graph single number, vector, or n_agents-by-n_agents matrix
#'   of starting trust weights between agents. Coerced to numeric. Can also be
#'   a function taking the first generation of the agents tbl as an input and
#'   returning an n-by-n matrix of trust values between 0 and 1, where n is the
#'   number of agents, 0 represents completely untrustworthy, .5 random, and 1
#'   completely trustworthy.
#' @param random_seed the random seed to start the simulation with. If set, this
#'   is used to generate the random seeds for agent construction and simulation
#'   (unless those seeds are explicitly specified). This means output is
#'   reproducible even if only this seed is set.
#' @param .random_seed_agents random seed for agent construction
#' @param .random_seed_simulation random seed for simulation
#' @param truth_fun function taking the simulation and decision number as
#'   arguments and returning the true state of the world as a single number
#' @param truth_sd standard deviation of the truth function that the agents use
#'   to calculate the probability of values given their biases.
#' @param confidence_weighted whether agents use their own confidence in initial
#'   decisions to weigh their updating of trust in other agents. If F, agents
#'   simply examine whether other agents agree (although this produces a cliff
#'   whereby slight agreement and slight disagreement produce results as
#'   different as extreme agreement and extreme disagreement).
#' @param model if present as a data frame output similar to that produced by
#'   \code{\link{makeAgents}}, this model is used instead of being generated
#'   automatically. If this is specified, the other model parameters giving
#'   means and SDs for agent properties are for reference only, and the
#'   properties which describe the model (e.g. n_agents) should match the
#'   values extracted from this data frame.
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
  decision_flags = 3,
  bias_mean = 0,
  bias_sd = 1,
  sensitivity_mean = 1,
  sensitivity_sd = 1,
  trust_volatility_mean = .1,
  trust_volatility_sd = .025,
  bias_volatility_mean = .025,
  bias_volatility_sd = .01,
  confidence_slope_mean = 1,
  confidence_slope_sd = 0,
  weighted_sampling_mean = 0,
  weighted_sampling_sd = 0,
  bias_update_slope = 1,
  feedback_probability = .05,
  feedback_proportion = 1.0,
  feedback_sd = 0.0,
  starting_graph = NULL,
  random_seed = NA,
  .random_seed_agents = NA,
  .random_seed_simulation = NA,
  truth_fun = function(model, d) stats::rnorm(1, 0, model$parameters$truth_sd),
  truth_sd = .5,
  confidence_weighted = T,
  model = NA
) {
  # Set random seeds
  if (is.na(random_seed))
    random_seed <- round(runif(1, 1e6, 1e8))

  with_seed(
    as.integer(random_seed),
    {
      if (is.na(.random_seed_agents))
        .random_seed_agents <- round(runif(1, 1e6, 1e8))  # random random seed
      if (is.na(.random_seed_simulation))
        .random_seed_simulation <- round(runif(1, 1e6, 1e8))
    }
  )

  # Prepare output
  out <- list(
    times = list(
      start = Sys.time()
    ),
    parameters = list(
      n_agents = n_agents,
      n_decisions = n_decisions,
      decision_flags = recycle(decision_flags, n_decisions),
      bias_mean = bias_mean,
      bias_sd = bias_sd,
      sensitivity_mean = sensitivity_mean,
      sensitivity_sd = sensitivity_sd,
      trust_volatility_mean = trust_volatility_mean,
      trust_volatility_sd = trust_volatility_sd,
      bias_volatility_mean = bias_volatility_mean,
      bias_volatility_sd = bias_volatility_sd,
      confidence_slope_mean = confidence_slope_mean,
      confidence_slope_sd = confidence_slope_sd,
      weighted_sampling_mean = weighted_sampling_mean,
      weighted_sampling_sd = weighted_sampling_sd,
      bias_update_slope = bias_update_slope,
      feedback_probability = feedback_probability,
      feedback_proportion = feedback_proportion,
      feedback_sd = feedback_sd,
      starting_graph_type = class(starting_graph)[1],
      starting_graph = starting_graph,
      random_seed = random_seed,
      .random_seed_agents = .random_seed_agents,
      .random_seed_simulation = .random_seed_simulation,
      truth_fun = truth_fun,
      truth_sd = truth_sd,
      confidence_weighted = as.logical(confidence_weighted)
    )
  )

  with_seed(
    as.integer(.random_seed_agents),
    {
      # Construct the agents
      if (all(is.na(model))) {
        out$model <- makeAgents(
          n_agents = n_agents,
          n_decisions = n_decisions,
          bias_mean = bias_mean,
          bias_sd = bias_sd,
          sensitivity_mean = sensitivity_mean,
          sensitivity_sd = sensitivity_sd,
          trust_volatility_mean = trust_volatility_mean,
          trust_volatility_sd = trust_volatility_sd,
          bias_volatility_mean = bias_volatility_mean,
          bias_volatility_sd = bias_volatility_sd,
          confidence_slope_mean = confidence_slope_mean,
          confidence_slope_sd = confidence_slope_sd,
          weighted_sampling_mean = weighted_sampling_mean,
          weighted_sampling_sd = weighted_sampling_sd,
          starting_graph = starting_graph
        )
      } else {
        out$model <- model
      }
    }
  )

  out$times$agentsCreated <- Sys.time()

  with_seed(
    as.integer(.random_seed_simulation),
    {
      # Run the model
      for (d in 1:n_decisions)
        out <- simulationStep(out, d)
    }
  )

  out$times$end <- Sys.time()

  detailGraphs(out)
}

#' Run a suite of simulations defined by params
#' @param params dataframe of parameters for simulations (see
#'   \code{\link{runSimulation}} for details). Can also be a list of lists to
#'   support custom function arguments
#' @param cores number of cores to use in the cluster
#' @param summaryFun used to summarise each model after running. Takes the
#'   completed model as its argument. Parallelised, so must load necessary
#'   libraries etc. explicitly.
#' @inheritDotParams parallel::makeCluster
#'
#' @importFrom parallel makeCluster stopCluster parApply parLapply clusterExport
#'
#' @export
runSimulations <- function(
  params,
  cores = parallel::detectCores(),
  summaryFun = function(model) model,
  ...
  ) {
  # Unpack arguments and call runSimulation
  f <- function(p) {
    library(adviseR)
    summaryFun(do.call(runSimulation, as.list(p)))
  }

  # Single core case, primarily for debugging
  if (cores < 2) {
    out <- list()
    for (i in 1:nrow(params)) {
      out[[i]] <- f(params[i, ])
    }
    return(out)
  }

  cl <- makeCluster(cores, ...)
  on.exit(stopCluster(cl), add = TRUE)
  clusterExport(cl, 'summaryFun', envir = environment())

  if ('data.frame' %in% class(params)) {
    out <- parApply(cl, params, 1, f)
  } else {
    out <- parLapply(cl, params, f)
  }

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
    selectAdvisorSimple(model$model$graphs[[d]], agents$weighted_sampling)

  agents$weight <- diag(model$model$graphs[[d]][, agents$advisor])

  # Advice
  agents$advice <- round(agents$initial[agents$advisor]) # advice is 0 or 1

  # Final decision
  # agents$final <- weighted(agents$advice, agents$initial, agents$weight)
  agents$final <- bayes(agents$initial, agents$advice, agents$weight)

  if (runif(1) < model$parameters$feedback_probability) {
    n_get_feedback <- round(nrow(agents) / model$parameters$feedback_proportion)
    get_feedback <- sample(agents$id, n_get_feedback)
    feedback <- rnorm(1, sd = model$parameters$feedback_sd)
    agents$feedback[get_feedback] <- feedback
  }

  # Write output to the model
  model$model$agents[rows, ] <- agents

  # Updating bias for next time
  if (max(rows) != nrow(model$model$agents)) {
    if (bitwAnd(model$parameters$decision_flags[d], 2) == 2) {
      # Nudge bias towards observed (i.e. based on final decision) truth
      model$model$agents[rows + model$parameters$n_agents, "bias"] <-
        getUpdatedBias(agents, model$parameters$bias_update_slope)
    }

    # Updating weights
    if (bitwAnd(model$parameters$decision_flags[d], 1) == 1) {
      model$model$graphs[[d + 1]] <-
        newWeightsByDrift(
          agents,
          model$model$graphs[[d]],
          model$parameters$confidence_weighted
        )
    } else {
      model$model$graphs[[d + 1]] <- model$model$graphs[[d]]
    }
  }

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
#'   selection; 0 means selection is equally weighted
#' @return vector of the advisor id selected by each agent
selectAdvisor <- function(graph, exponent = 0) {
  # Weight trust matrix by exponent
  probabilities <- graph ^ exponent
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

#' Return the advisor selections based on \code{graph}
#' @param graph weighted trust matrix for agents
#' @param weightedSelection scaling for probabilistic selection; 0 means
#' selection is equally weighted
#' @details Weights the probability of selection of each advisor by using a
#' half sigmoid with slope = weightedSelection on the trust in that advisor
#' where the most preferred advisor has probability .5
#' @return vector of the advisor id selected by each agent
selectAdvisorSimple <- function(graph, weightedSelection = 0) {
  # Shift trust to centre around the mean (ignoring self)
  weight_max <- apply(graph, 1, max)
  # Weight trust matrix by exponent
  probabilities <- 2 * sigmoid(graph - weight_max, weightedSelection)
  # never ask yourself
  diag(probabilities) <- 0
  sapply(1:nrow(graph), function(i)
    sample(
      col(probabilities)[i, ], # ids are column numbers
      size = 1,
      prob = probabilities[i, ]
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

#' Bayesian integration where the reliability of the advisor is the probability
#' the advisor agrees given we were correct.
#' @param initial vector of initial decisions
#' @param advice vector of advisory estimates
#' @param weight trust rating for the advisor
#' @param compression whether to limit extreme values to c(x,1-x)
#' @details Uses a Bayesian integration formula where
#' \deqn{c_2 = \frac{c_1 * t}{c_1 * t + (1-c_1)(1-t)}}{c2 = (c1*t)/(c1*t + (1-c1)(1-t))}
#' \eqn{c_2}{c2} is the final confidence (returned as a vector), and \eqn{c_1}{c1} the initial
#' confidence.
#' \eqn{t} is the probability of the advisor's advice given the initial decision
#' was correct. Where the advisor agrees, this is simply the trust we have in
#' the advisor (an advisor we trusted 100% would always be expected to give the
#' same answer we did). Where the advisor disagrees, this is the opposite (we
#' consider it very unlikely a highly trusted advisor disagrees with us if we
#' are right).
bayes <- function(initial, advice, weight, compression = .05) {
  if (compression) {
    weight <- pmin(1 - compression, pmax(weight, compression))
  }
  weight <- ifelse(adviceAgrees(initial, advice), weight, 1 - weight)
  (initial * weight) / (initial * weight + (1 - initial) * (1 - weight))
}

#' Return a measure of how compatible advice is with an initial decision
#' @param initial vector of initial decisions
#' @param advice vector of advisory estimates
#' @return 1 - error score
adviceCompatibility <- function(initial, advice) {
  # Turn around left estimates so everything starts off initial > .5, advice in
  # same direction
  left <- initial < .5
  initial[left] <- 1 - initial[left]
  advice[left] <- 1 - advice[left]
  # Calculate error for advice|initial
  residual <- abs(advice - initial)
  # bigger values should indicate more compatibility
  1 - residual
}

#' Return whether the advice agrees with the initial decision
#' @param initial vector of initial decisions
#' @param advice vector of advisory estimates
adviceAgrees <- function(initial, advice) {
  (initial < .5) == (advice < .5)
}

#' Return vector of updated biases for agents
#' @param agents tbl with a snapshot of agents at a given time
#' @param slope slope of the sigmoid function run on influence
#' @param limit clamp resulting bias to between limit and 1-limit
#'
#' @details Bias is calculated by feeding influence (difference between final
#' and initial judgement) into a sigmoid function. This gives an amount of bias
#' influence between -.5 and .5, for which we take the absolute value and
#' subtract .5, giving a value between 0 and .5 which is around 0 for low levels
#' of influence and rises rapidly to around .5 for higher influence levels.
#' This bias influence is then scaled to between 0 and the agent's
#' bias_volatility parameter (when bias influence is .5), and this is used as
#' the amount the bias updates towards the final decision direction.
#' The final value of the bias is clamped to avoid extreme values.
getUpdatedBias <- function(agents, slope, limit = .05) {
  newBias <- weighted(
    round(ifelse(is.na(agents$feedback), agents$final, agents$feedback)),
    agents$bias,
    agents$bias_volatility
  )

  # Niccolo's approach
  # influence = agents$final - agents$initial
  # newBias <- agents$bias +
  #   sign(agents$final - .5) *
  #   abs(sigmoid(influence, slope) - .5) *
  #   agents$bias_volatility

  pmin(1 - limit, pmax(0 + limit, newBias))
}

#' Return the new trust matrix for agents
#' @param agents tbl with a snapshot of agents at a given time
#' @param graph connectivity matrix of trust
#' @param confidence_weighted whether to use confidence to weight the updating or
#'  instead rely only on dis/agreement
#'
#' @details Agents work out how expected advice was given their initial
#' estimate, and in/decrease their trust according to that likelihood, scaled
#' by each agent's trust_volatility.
#'
#' @return Connectivity matrix of trust after accounting for agents' decisions
newWeights <- function(agents, graph, confidence_weighted = T) {
  n_agents <- nrow(graph)
  # use feedback if available
  agents$initial <- ifelse(
    is.na(agents$feedback),
    agents$initial, round(agents$feedback)
  )

  if (confidence_weighted)
    adviceAgree <- (adviceCompatibility(agents$initial, agents$advice) - .5) * 2
  else
    adviceAgree <- (adviceAgrees(agents$initial, agents$advice) - .5) * 2

  W <- as.vector(graph)
  m <- (agents$advisor - 1) * n_agents + agents$id
  W[m] <- W[m] + adviceAgree * agents$trust_volatility

  err <- 1e-4

  W <- pmax(err, pmin(1 - err, W)) # cap weights
  W <- matrix(W, n_agents, n_agents)
  diag(W) <- 0
  W
}

#' Return the new trust matrix for agents using weighted drift towards 0/1
#' @param agents tbl with a snapshot of agents at a given time
#' @param graph connectivity matrix of trust
#' @param confidence_weighted whether to use confidence to weight the updating or
#'  instead rely only on dis/agreement
#'
#' @details Agents drift towards 0 or 1 trust according to whether or not the
#' advisor agreed with them. This can be weighted by confidence, too, where the
#' agreement matters more the more confident the agent was in their initial
#' decision. This latter is accomplished by a second drift from the original
#' trust rating to the prospective value.
#'
#' @return Connectivity matrix of trust after accounting for agents' decisions
newWeightsByDrift <- function(agents, graph, confidence_weighted = T) {
  n_agents <- nrow(graph)
  # use feedback if available
  agents$initial <- ifelse(
    is.na(agents$feedback),
    agents$initial, round(agents$feedback)
  )
  adviceAgree <- ifelse(adviceAgrees(agents$initial, agents$advice), 1, 0)

  W <- as.vector(graph)
  m <- (agents$advisor - 1) * n_agents + agents$id
  x <- W[m] * (1 - agents$trust_volatility) + adviceAgree * agents$trust_volatility

  if (confidence_weighted) {
    conf <- abs(agents$initial - .5) * 2
    W[m] <- x * conf + (W[m] * (1 - conf))
  } else {
    W[m] <- x
  }

  err <- 1e-4

  W <- pmax(err, pmin(1 - err, W)) # cap weights
  W <- matrix(W, n_agents, n_agents)
  diag(W) <- 0
  W
}

#' Return new trust matrix updating using Bayes rule to combine the current
#' weight with the data from agreement. The agents' trust_volatility is used as
#' the probability for the hypothesis (that the weight assigned to the advisor
#' is correct).
#' @param agents tbl with a snapshot of agents at a given time
#' @param graph connectivity matrix of trust
#' @param confidence_weighted wehther to use confidence to weight the updating
#' or instead rely only on dis/agreement
#' @details The Bayesian integration uses 1 minus each agent's
#' trust_volatility as the precision of their advisor trust. This is not
#' hugely realistic, because it's very plausible that we may be more certain
#' about how un/trustworthy one advisor is than another, and we could imagine
#' that the precision of trustworthiness estimates would increase with greater
#' experience with that advisor.
#'
#' The 1-trust_volatility is taken as P(H), and the P(D|H) is the probability
#' of the received advice given that the initial answer was correct (which is
#' assumed for the sake of simplicity). Where the advisor agrees, this is the
#' weight placed on that advisor, and where the advisor disagrees this is 1
#' minus that weight.
#'
#' Bayesian integration then gives P(H|D) = P(H)P(D|H)/(P(D|H) + P(D|¬H))
#' which yields the posterior plausibility of current trust weight.
#' The posterior plausibility of the current trust weight is then used as to
#' weight the update from the current trust towards 1 or 0 depending upon
#' whether the advisor agreed.
#' This means that an advisor who offers quite surprising advice, and thus gets
#' a low posterior trust weight plausibility will be updated quite dramatically
#' in the direction of their agreement, while one whose advice is anticipated
#' will get only a little change.
#'
#' In the confidence_weighted case, the final adjustment is weighted by the
#' confidence in the initial answer.
newWeightsBayes <- function(agents, graph, confidence_weighted) {
  n_agents <- nrow(graph)
  # use feedback if available
  agents$initial <- ifelse(
    is.na(agents$feedback),
    agents$initial, round(agents$feedback)
  )

  W <- as.vector(graph)
  m <- (agents$advisor - 1) * n_agents + agents$id

  pAdvice <- 1 - agents$trust_volatility
  curW <- ifelse(
    adviceAgrees(agents$initial, agents$advice),
    agents$weight,
    1 - agents$weight
  )
  curW <- agents$weight
  post <- (curW * pAdvice) / (curW * pAdvice + (1 - curW) * (1 - pAdvice))

  newTrust <- post * agents$weight + (1 - post) *
    as.integer(adviceAgrees(agents$initial, agents$advice))

  if (confidence_weighted) {
    conf <- abs(agents$initial - .5) * 2
    newTrust <- newTrust * conf + (W[m] * (1 - conf))
  }

  # cap update by trust volatility
  W[m] <- ifelse(
    abs(W[m] - newTrust) > agents$trust_volatility,
    W[m] + agents$trust_volatility * sign(newTrust - W[m]),
    newTrust
  )

  err <- 1e-4

  W <- pmax(err, pmin(1 - err, W)) # cap weights
  W <- matrix(W, n_agents, n_agents)
  diag(W) <- 0
  W
}
