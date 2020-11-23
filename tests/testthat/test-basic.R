context('Basic functionality')
library(adviseR)

test_that('Simple simulation', {
  load('data/bias-model.rda')
  model <- runSimulation(randomSeed = floor(pi * 1e6))
  # Can't do a simple identical check because $timings will be different,
  # and $graphs have different ids (presumably to avoid conflicts)
  expect_equal(model$parameters, bias.model$parameters)
  expect_equal(model$model$agents, bias.model$model$agents)
})

test_that('Simulation network graphs', {
  load('data/basic-model.rda')
  expect_invisible(networkGraph(basic.model))
})

test_that('Bias graph', {
  load('data/basic-model.rda')
  gg <- biasGraph(basic.model)
  expect_equal('ggplot' %in% class(gg), T)
  expect_error(
    expect_equal(gg, biasGraph(basic.model, use_starting_bias = T))
  )
})

test_that('Sensitivity graph', {
  load('data/basic-model.rda')
  expect_equal('ggplot' %in% class(sensitivityGraph(basic.model)), T)
})
