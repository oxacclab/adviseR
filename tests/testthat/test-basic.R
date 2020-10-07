context('Basic functionality')
library(adviseR)

test_that('Simple simulations', {
  load('data/basic-model.rda')
  model <- runSimulation(
    n = list(p = 10, d = 200),
    conf = T,
    biasMean = 1,
    biasSD = 1,
    sensitivitySD = 1,
    learningRate = .1,
    randomSeed = floor(pi * 1e6)
  )
  expect_identical(model$parameters, basic.model$parameters)
  expect_identical(model$model, basic.model$model)
})
