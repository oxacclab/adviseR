context('Custom functionality')
library(adviseR)

test_that('Custom truth_fun', {
  load('data/truth_fun-model.rda')
  model <- runSimulation(
    randomSeed = floor(pi * 1e6),
    truth_fun = function(m, d) d %% 5 - 2,
    truth_sd = 1
  )
  expect_equal(model$parameters, truth_fun.model$parameters)
  expect_identical(model$model$agents, truth_fun.model$model$agents)
})

test_that('Weighted sampling', {
  model <- runSimulation(
    trust_volatility_mean = 0,
    trust_volatility_sd = 0, # no trust weight updating
    weighted_sampling = 2,
    randomSeed = floor(pi * 1e6)
  )

  # Check counts of selection by weight
  counts <- sapply(
    c(.25, .5, .75),
    function(x) sum(model$model$agents$weight == x)
  )
  expect_lt(counts[1], counts[2])
  expect_lt(counts[2], counts[3])
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
    randomSeed = 20201014
  )

  # Can't do a simple identical check because $timings will be different,
  # and $graphs have different ids (presumably to avoid conflicts)
  expect_equal(m$model$agents, thesis.model$model$agents)
  expect_equal(length(inspectModel(m)), 4)
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
