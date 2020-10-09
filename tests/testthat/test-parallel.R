context('Basic functionality')
library(adviseR)

test_that('Simple parallel simulation', {
  load('data/basic-model.rda')
  load('data/basic-noconf-model.rda')

  params <- data.frame(
    n_agents = 6,
    n_decisions = 200,
    conf = c(T, F),
    biasMean = 1,
    biasSD = 1,
    sensitivitySD = 1,
    learningRate = .1,
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
