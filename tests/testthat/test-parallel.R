context('Parallel functionality')
library(adviseR)

test_that('Simple parallel simulation', {
  load('data/basic-model.rda')
  load('data/bias-model.rda')

  params <- data.frame(
    bias_volatility_mean = c(0, .05),
    bias_volatility_sd = c(0, .01),
    randomSeed = floor(pi * 1e6)
  )
  models <- runSimulations(params, cores = 2, outfile = paste0(tempfile(), '.log'))

  # Can't do a simple identical check because $timings will be different,
  # and $graphs have different ids (presumably to avoid conflicts)
  expect_equal(models[[1]]$parameters, basic.model$parameters)
  expect_identical(models[[1]]$model$agents, basic.model$model$agents)
  expect_equal(models[[2]]$parameters, bias.model$parameters)
  expect_identical(models[[2]]$model$agents, bias.model$model$agents)
})

if (F) {
  # Do this individually because parallel processing can't be trusted not to
  # use cached versions of the package
  basic.model <- do.call(runSimulation, as.list(params[1, ]))
  bias.model <- do.call(runSimulation, as.list(params[2, ]))
  save(basic.model, file = 'tests/testthat/data/basic-model.rda')
  save(bias.model, file = 'tests/testthat/data/bias-model.rda')
}
