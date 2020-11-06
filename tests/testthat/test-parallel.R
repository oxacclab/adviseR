context('Parallel functionality')
library(adviseR)

test_that('Simple parallel simulation', {
  load('data/basic-model.rda')
  load('data/basic-noconf-model.rda')
  load('data/bias-model.rda')
  load('data/bias-noconf-model.rda')

  params <- data.frame(
    n_agents = 6,
    n_decisions = 200,
    conf = c(T, F, T, F),
    bias_mean = 1,
    bias_sd = 1,
    sensitivity_sd = 1,
    trust_volatility_mean = .05,
    trust_volatility_sd = .01,
    bias_volatility_mean = 0,
    bias_volatility_sd = c(0, 0, .01, .01),
    randomSeed = floor(pi * 1e6),
    asymptotic_confidence = F
  )
  models <- runSimulations(params, cores = 2, outfile = paste0(tempfile(), '.log'))

  # Can't do a simple identical check because $timings will be different,
  # and $graphs have different ids (presumably to avoid conflicts)
  expect_equal(models[[1]]$parameters, basic.model$parameters)
  expect_equal(models[[1]]$model$agents, basic.model$model$agents)
  expect_equal(models[[2]]$parameters, basic.noconf.model$parameters)
  expect_equal(models[[2]]$model$agents, basic.noconf.model$model$agents)
  expect_equal(models[[3]]$parameters, bias.model$parameters)
  expect_equal(models[[3]]$model$agents, bias.model$model$agents)
  expect_equal(models[[4]]$parameters, bias.noconf.model$parameters)
  expect_equal(models[[4]]$model$agents, bias.noconf.model$model$agents)
})

if (F) {
  # Do this individually because parallel processing can't be trusted not to
  # use cached versions of the package
  basic.model <- do.call(runSimulation, as.list(params[1, ]))
  basic.noconf.model <- do.call(runSimulation, as.list(params[2, ]))
  bias.model <- do.call(runSimulation, as.list(params[3, ]))
  bias.noconf.model <- do.call(runSimulation, as.list(params[4, ]))
  save(basic.model, file = 'tests/testthat/data/basic-model.rda')
  save(basic.noconf.model, file = 'tests/testthat/data/basic-noconf-model.rda')
  save(bias.model, file = 'tests/testthat/data/bias-model.rda')
  save(bias.noconf.model, file = 'tests/testthat/data/bias-noconf-model.rda')
}
