context('Basic functionality')
library(adviseR)

test_that('Simple parallel simulation', {
  load('data/basic-model.rda')
  load('data/basic-noconf-model.rda')

  params <- data.frame(
    n_agents = 6,
    n_decisions = 200,
    conf = c(T, F),
    bias_mean = 1,
    bias_sd = 1,
    sensitivity_sd = 1,
    trust_volatility_mean = .05,
    trust_volatility_sd = .01,
    bias_volatility_mean = .05,
    bias_volatility_sd = .01,
    randomSeed = floor(pi * 1e6)
  )
  models <- runSimulations(params, cores = nrow(params))

  # Can't do a simple identical check because $timings will be different,
  # and $graphs have different ids (presumably to avoid conflicts)
  expect_identical(models[[1]]$parameters, basic.model$parameters)
  expect_identical(models[[1]]$model$agents, basic.model$model$agents)
  expect_identical(models[[2]]$parameters, basic.noconf.model$parameters)
  expect_identical(models[[2]]$model$agents, basic.noconf.model$model$agents)
})

if (F) {
  basic.noconf.model <- models[[2]]
  save(basic.noconf.model, file = 'tests/testthat/data/basic-noconf-model.rda')
}
