context('Custom functionality')
library(adviseR)
library(igraph)

test_that('Custom truth_fun', {
  load('data/truth_fun-model.rda')
  model <- runSimulation(
    random_seed = floor(pi * 1e8),
    truth_fun = function(m, d) d %% 5 - 2,
    truth_sd = 1
  )
  expect_equal(model$parameters, truth_fun.model$parameters)
  expect_equal(model$model$agents, truth_fun.model$model$agents)
})

test_that('Weighted sampling', {
  model <- runSimulation(
    trust_volatility_mean = 0,
    trust_volatility_sd = 0, # no trust weight updating
    weighted_sampling_mean = 5,
    weighted_sampling_sd = .3,
    random_seed = floor(pi * 1e8)
  )

  # Check counts of selection by weight
  counts <- sapply(
    c(.25, .5, .75),
    function(x) sum(model$model$agents$weight == x)
  )
  expect_lt(counts[1], counts[2])
  expect_lt(counts[2], counts[3])

  # Check that higher values of weighted_sampling mean the agents are more picky
  # and not that the agents are more picked!
  agents <- data.frame(
    id = 1:3,
    decision = rep(1, each = 3),
    sensitivity = .3,
    trust_volatility = 0,
    bias_volatility = 0,
    weighted_sampling = c(1, 5, 25),
    bias = 0,
    truth = NA_real_,
    percept = NA_real_,
    initial = NA_real_,
    advisor = NA_integer_,
    advice = NA_real_,
    weight = NA_real_,
    final = NA_real_,
    confidence_slope = 1
  )
  trust <- matrix(c(
    0, .9, .1,
    .9, 0, .1,
    .9, .1, 0
  ), 3, 3, byrow = T)
  m <- list(
    model = list(agents = agents, graphs = list(trust)),
    parameters = model$parameters
  )
  m$parameters$n_agents <- 3
  # Do multiple runs so we can check pickiness and picked-ness
  s <- NULL
  set.seed(floor(pi * 1e8))
  for (i in 1:1000)
    s <- rbind(s, simulationStep(m, 1)$model$agents)
  s <- aggregate(advisor ~ id, mean, data = s)
  s <- round(s, 1)
  expect_equal(s, data.frame(id = 1:3, advisor = c(2.4, 1.3, 1.0)))
})

test_that('Custom starting_graph works', {
  m_dbl <- runSimulation(
    n_agents = 6,
    n_decisions = 2,
    random_seed = floor(pi * 1e8),
    starting_graph = 0.9
  )
  mat <- matrix(0.9, nrow = 6, ncol = 6)
  diag(mat) <- 0
  expect_equal(all(as.matrix(m_dbl$model$graphs[[1]][attr = 'weight']) == mat), T)

  mat[upper.tri(mat)] <- .6
  m_mat <- runSimulation(
    n_agents = 6,
    n_decisions = 2,
    random_seed = floor(pi * 1e8),
    starting_graph = mat
  )
  expect_equal(all(as.matrix(m_mat$model$graphs[[1]][attr = 'weight']) == mat), T)

  m_fun <- runSimulation(
    n_agents = 6,
    n_decisions = 20,
    weighted_sampling_mean = 50,
    random_seed = floor(pi * 1e8),
    starting_graph = function(a) {
      # bias difference
      bias <- matrix(a$bias, nrow = nrow(a), ncol = nrow(a))
      x <- abs(bias - t(bias))
      1 - (x / 2)
    }
  )
  expect_equal(as.numeric(.biasCorrelation(m_fun)$r[1]), 1)
})

test_that('Example thesis simulation', {
  load('data/thesis-model.rda')

  m <- runSimulation(
    n_agents = 20,
    n_decisions = 500,
    bias_mean = 0,
    bias_sd = 2,
    sensitivity_sd = 1,
    trust_volatility_mean = 5,
    trust_volatility_sd = .3,
    bias_volatility_mean = 0,
    bias_volatility_sd = .0,
    starting_graph = .1,
    random_seed = 20201014
  )

  # Can't do a simple identical check because $timings will be different,
  # and $graphs have different ids (presumably to avoid conflicts)
  expect_equal(m$model$agents, thesis.model$model$agents)
  expect_equal(length(inspectModel(m)), 4)
  expect_lt(groupRatio(m$model$graphs[[1]]), groupRatio(m$model$graphs[[500]]))
})

test_that('Bias update skipped with mask', {
  m <- runSimulation(decision_flags = rep(c(1, 3), each = 100))
  expect_equal(
    m$model$agents$bias[m$model$agents$decision == 1],
    m$model$agents$bias[m$model$agents$decision == 100]
  )
  expect_false(
    all(
      m$model$agents$bias[m$model$agents$decision == 1] ==
        m$model$agents$bias[m$model$agents$decision == 200]
    )
  )
})

test_that('Trust update skipped with mask', {
  m <- runSimulation(decision_flags = rep(c(2, 3), each = 100))
  expect_equal(
    edge_attr(m$model$graphs[[1]], 'weight'),
    edge_attr(m$model$graphs[[100]], 'weight')
  )
  expect_error(
    expect_equal(
      edge_attr(m$model$graphs[[1]], 'weight'),
      edge_attr(m$model$graphs[[200]], 'weight')
    )
  )
})

if (F) {
  truth_fun.model <- model
  save(truth_fun.model, file = 'tests/testthat/data/truth_fun-model.rda')
  thesis.model <- m
  save(thesis.model, file = 'tests/testthat/data/thesis-model.rda')

  # Visualise the thesis model to check nothing whacky is up
  for (x in inspectModel(m))
    print(x)

}
