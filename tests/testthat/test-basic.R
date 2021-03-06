context('Basic functionality')
library(adviseR)
library(igraph)

test_that('Simple simulation', {
  load('data/bias-model.rda')
  model <- runSimulation(random_seed = floor(pi * 1e6))
  # Can't do a simple identical check because $timings will be different,
  # and $graphs have different ids (presumably to avoid conflicts)
  expect_equal(model$parameters, bias.model$parameters)
  expect_equal(model$model$agents, bias.model$model$agents)
})

test_that('Custom model specification', {
  load('data/bias-model.rda')
  model <- runSimulation(
    model = list(
      agents = bias.model$model$agents,
      graphs = list(
        t(as_adjacency_matrix(
          bias.model$model$graphs[[1]],
          attr = 'weight',
          sparse = F
        ))
      )
    ),
    .random_seed_simulation = bias.model$parameters$.random_seed_simulation,
    random_seed = bias.model$parameters$random_seed,
    .random_seed_agents = bias.model$parameters$.random_seed_agents
  )
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

test_that('Simulate from data', {
  load('data/empirical_data.rda')
  m <- simulateFromData(empirical_data, data.frame(a = 1, b = 1), T)
  MSE <- c(
    "Advisor choice mean squared error" =
      mean(m$advisor_choice_error ^ 2, na.rm = T),
    "Advice-taking mean squared error" =
      mean(m$advice_taking_error ^ 2, na.rm = T)
  )
  expect_equal(
    simulateFromData(empirical_data, data.frame(a = 1, b = 1)),
    MSE
  )
})
