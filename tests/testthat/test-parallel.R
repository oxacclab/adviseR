context('Parallel functionality')
library(adviseR)

test_that('Simple parallel simulation', {
  load('data/basic-model.rda')
  load('data/bias-model.rda')

  params <- data.frame(
    bias_volatility_mean = c(0, .05, 0),
    bias_volatility_sd = c(0, .01, 0),
    random_seed = floor(pi * 1e8),
    confidence_weighted = c(T, T, F)
  )
  models <- runSimulations(params, cores = 2, outfile = paste0(tempfile(), '.log'))

  # Can't do a simple identical check because $timings will be different,
  # and $graphs have different ids (presumably to avoid conflicts)
  expect_equal(models[[1]]$parameters, basic.model$parameters)
  expect_equal(models[[1]]$model$agents, basic.model$model$agents)
  expect_equal(models[[2]]$parameters, bias.model$parameters)
  expect_equal(models[[2]]$model$agents, bias.model$model$agents)

  # Confidence weighting should make a difference!
  expect_error(
    expect_equal(models[[3]]$model$agents, basic.model$model$agents)
  )
})

test_that('Parallel with custom summary', {
  params <- data.frame(
    n_agents = 24,
    n_decisions = 100,
    random_seed = floor(pi * 1e8),
    bias_volatility_mean = c(0, .05),
    bias_volatility_sd = c(0, .01)
  )
  gr <- runSimulations(
    params,
    cores = 2,
    summaryFun = function(model)
      groupRatio(model$model$graphs[[length(model$model$graphs)]])
  )
  expect_gt(gr[1], gr[2])
})

if (F) {
  # Do this individually because parallel processing can't be trusted not to
  # use cached versions of the package
  basic.model <- do.call(runSimulation, as.list(params[1, ]))
  bias.model <- do.call(runSimulation, as.list(params[2, ]))
  save(basic.model, file = 'tests/testthat/data/basic-model.rda')
  save(bias.model, file = 'tests/testthat/data/bias-model.rda')
}
