context('Custom functionality')
library(adviseR)

test_that('Custom truth_fun', {
  load('data/truth_fun-model.rda')
  model <- runSimulation(
    n_agents = 6,
    n_decisions = 200,
    conf = T,
    bias_mean = 1,
    bias_sd = 1,
    sensitivity_sd = 1,
    trust_volatility_mean = .05,
    trust_volatility_sd = .01,
    bias_volatility_mean = 0,
    bias_volatility_sd = 0,
    randomSeed = floor(pi * 1e6),
    truth_fun = function(m, d) d %% 5 - 2,
    asymptotic_confidence = F
  )
  expect_equal(model$parameters, truth_fun.model$parameters)
  expect_identical(model$model$agents, truth_fun.model$model$agents)
})

test_that('Weighted sampling', {
  model <- runSimulation(
    n_agents = 6,
    n_decisions = 200,
    conf = T,
    bias_mean = 1,
    bias_sd = 1,
    sensitivity_sd = 1,
    trust_volatility_mean = 0,
    trust_volatility_sd = 0, # no trust weight updating
    bias_volatility_mean = 0,
    bias_volatility_sd = 0,
    randomSeed = floor(pi * 1e6),
    weighted_sampling = 1,
    asymptotic_confidence = F
  )

  # Check counts of selection by weight
  counts <- sapply(
    c(.25, .5, .75),
    function(x) sum(model$model$agents$weight == x)
  )
  expect_lt(counts[1], counts[2])
  expect_lt(counts[2], counts[3])
})

test_that('Simple parallel simulation', {
  load('data/asymp-model.rda')

  params <- list(
    list(
      n_agents = 6,
      n_decisions = 200,
      conf = T,
      bias_mean = 1,
      bias_sd = 1,
      sensitivity_sd = 1,
      trust_volatility_mean = .05,
      trust_volatility_sd = .01,
      bias_volatility_mean = 0,
      bias_volatility_sd = .01,
      randomSeed = floor(pi * 1e6),
      asymptotic_confidence = c(0,1)
    ),
    list(
      n_agents = 6,
      n_decisions = 200,
      conf = T,
      bias_mean = 1,
      bias_sd = 1,
      sensitivity_sd = 1,
      trust_volatility_mean = .05,
      trust_volatility_sd = .01,
      bias_volatility_mean = 0,
      bias_volatility_sd = .01,
      randomSeed = floor(pi * 1e6),
      asymptotic_confidence = function(x) rnorm(length(unique(x$id)))
    )
  )
  models <- runSimulations(params, cores = 2)

  # Can't do a simple identical check because $timings will be different,
  # and $graphs have different ids (presumably to avoid conflicts)
  expect_identical(models[[1]]$model$agents, asymp.model$model$agents)
  expect_identical(models[[2]]$model$agents, asymp.model$model$agents)
})

if (F) {
  truth_fun.model <- model
  save(truth_fun.model, file = 'tests/testthat/data/truth_fun-model.rda')
  asymp.model <- models[[1]]
  save(asymp.model, file = 'tests/testthat/data/asymp-model.rda')
}
