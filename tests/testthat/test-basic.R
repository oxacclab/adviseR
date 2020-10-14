context('Basic functionality')
library(adviseR)

test_that('Simple simulation', {
  load('data/basic-model.rda')
  model <- runSimulation(
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
    randomSeed = floor(pi * 1e6)
  )
  # Can't do a simple identical check because $timings will be different,
  # and $graphs have different ids (presumably to avoid conflicts)
  expect_identical(model$parameters, basic.model$parameters)
  expect_identical(model$model$agents, basic.model$model$agents)
})

test_that('Simulation network graphs', {
  load('data/basic-model.rda')
  expect_invisible(networkGraph(basic.model))
})

test_that('Bias graph', {
  load('data/basic-model.rda')
  expect_equal('ggplot' %in% class(biasGraph(basic.model)), T)
})

test_that('Sensitivity graph', {
  load('data/basic-model.rda')
  expect_equal('ggplot' %in% class(sensitivityGraph(basic.model)), T)
})
