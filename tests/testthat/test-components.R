context('Component tests')
library(adviseR)

test_that('recycle recycles', {
  expect_equal(recycle(1:3, 9), rep(1:3, 3))
  expect_equal(recycle(1:3, 8), rep(1:3, 3)[1:8])
  expect_warning(recycle(1:3, 8))
})

test_that('getPercept works', {
  # Right length of values and Infinite sensitivity returns truth
  x <- data.frame(sensitivity = rep(Inf, 25), truth = rep(1, 25))
  expect_equal(getPercept(x), rep(1, 25))
  # Greater sensitivity produces less residual
  y <- data.frame(
    sensitivity = exp(seq(-2, 5, length.out = 25)),
    truth = rep(0, 25)
  )
  percept <- apply(y, 1, function(x) {
    z <- data.frame(
      sensitivity = rep(x[1], 5000),
      truth = rep(x[2], 5000)
    )
    mean(abs(getPercept(z)))
  })
  expect_equal(25:1, order(percept))
  # Any sensitivity averages to roughly zero (truth) deviation
  m <- apply(y, 1, function(x) {
    z <- data.frame(
      sensitivity = rep(x[1], 1e4),
      truth = rep(x[2], 1e4)
    )
    mean(getPercept(z))
  })
  expect_equal(round(m), rep(0, 25))
})

test_that('sigmoid works', {
  x <- seq(-25, 25, length.out = 250)
  y <- sigmoid(x)
  expect_equal(round(y[1], 5), 0)
  expect_equal(round(y[250], 5), 1)
  expect_equal(sigmoid(0), .5)
})

test_that('getConfidence works', {
  x <- data.frame(
    percept = seq(-10, 10, length.out = 250),
    confidence_slope = rep(1, 250),
    bias = rep(.5, 250)
  )
  conf <- getConfidence(x, .5)
  # Symmetry and length
  expect_equal(conf[1], 1 - conf[250])
  # Bias
  x$bias <- .1
  expect_lt(max(getConfidence(x, .5)), .5)
  x$bias <- .75
  expect_gt(min(getConfidence(x, .5)), .25)
})

test_that('selectAdvisor works', {
  g <- matrix(
    c(
      0, 1, 0,
      0, 0, 1,
      1, 0, 0
    ),
    nrow = 3,
    ncol = 3,
    byrow = T
  )
  expect_equal(selectAdvisor(g, exponent = 1), c(2, 3, 1))
  # Probabilistic
  g <- matrix(
    c(
      0, .25, .5,
      .25, 0, .25,
      .5, 1, 0
    ),
    nrow = 3,
    ncol = 3,
    byrow = T
  )
  x <- sapply(1:10000, function(i) selectAdvisor(g, exponent = 1))
  expect_equal(
    round(rowMeans(x), 1),
    round(
      c(
        (2*g[1,2]/sum(g[1,]) + 3*g[1,3]/sum(g[1,])),
        (g[2,1]/sum(g[2,]) + 3*g[2,3]/sum(g[2,])),
        (g[3,1]/sum(g[3,]) + 2*g[3,2]/sum(g[3,]))
      ),
      1
    )
  )
  # Probablistic with weighted selection
  y <- sapply(1:5000, function(i) selectAdvisor(g, exponent = 5))
  expect_equal(round(rowMeans(y), 1), c(3.0, 2.0, 2.0))
})

test_that('selectAdvisorSimple works', {
  # Probabilistic
  g <- matrix(
    c(
      0, .25, .5,
      .25, 0, .25,
      .5, 1, 0
    ),
    nrow = 3,
    ncol = 3,
    byrow = T
  )
  x <- sapply(1:5000, function(i) selectAdvisorSimple(g, weightedSelection = 0))
  expect_equal(round(rowMeans(x), 1), c(2.5, 2.0, 1.5))
  # Probabilistic with weighted selection
  y <- sapply(1:5000, function(i) selectAdvisorSimple(g, weightedSelection = 5))
  expect_equal(round(rowMeans(y), 1), c(2.7, 2.0, 1.9))
})

test_that('weighted works', {
  x <- data.frame(
    a = c(0, 0, .5, .5, 1, 1),
    b = rep(1, 6),
    weights = c(.5, 1, .5, 1, .5, 1)
  )
  out <- c(.5, 0, .75, .5, 1, 1)
  expect_equal(weighted(x$a, x$b, x$weights), out)
})

test_that('bayes() updating works', {
  x <- data.frame(
    i = c(.1, .1, .5, .5, .9, .9),
    a = c(0, 1, 0, 1, 0, 1),
    w = c(0, 1, 1, 1, 1, 0)
  )
  b <- bayes(x$i, x$a, x$w)
  # Symmetry between agree|trust and disagree|distrust
  expect_equal(b[1], b[2])
  expect_equal(b[5], b[6])
  # Symmetry over disagree|trust and 1-(agree|trust)
  expect_equal(b[3], 1 - b[4])
})

test_that('adviceCompatibility works', {
  x <- data.frame(
    initial = c(0, .25, .5, .75, 1, 1, .75, .25),
    advice = c(0, 0, 0, 0, 0, 1, 1, 1)
  )
  expect_equal(
    adviceCompatibility(x$initial, x$advice),
    c(1, .75, .5, .25, 0, 1, .75, .25)
  )
})

test_that('getUpdatedBias works', {
  x <- data.frame(
    initial = c(0, .25, .5, .75, 1),
    final = rep(.5, 5),
    feedback = NA,
    bias = rep(.5, 5),
    bias_volatility = rep(.1, 5)
  )
  expect_equal(getUpdatedBias(x, slope = 1), rep(.45, 5))

  # Check changes of mind are updated in the right direction
  x <- data.frame(
    initial = c(.25, .75),
    final = c(.6, .4),
    feedback = NA,
    bias = c(.5, .5),
    bias_volatility = c(.1, .1)
  )
  expect_equal(getUpdatedBias(x, slope = 1), c(.55, .45))
})

test_that('newWeights works', {
  a <- data.frame(
    id = 1:3,
    initial = rep(.5, 3),
    advice = c(1, 0, 1),
    advisor = c(2, 3, 1),
    feedback = NA,
    trust_volatility = .1
  )
  g <- matrix(
    c(
      0, .75, 0,
      0, 0, .75,
      .75, 0, 0
    ),
    nrow = 3,
    ncol = 3,
    byrow = T
  )
  expect_equal(round(newWeights(a, g), 2), g)
  # Update in the right direction
  a <- data.frame(
    id = 1:3,
    initial = rep(.75, 3),
    advice = c(0, 1, 1),
    advisor = c(2, 3, 1),
    feedback = NA,
    trust_volatility = .5
  )
  w <- newWeights(a, g)
  expect_gt(g[1, a$advisor[1]], w[1, a$advisor[1]])
  expect_lt(g[2, a$advisor[2]], w[2, a$advisor[2]])
  expect_lt(g[3, a$advisor[3]], w[3, a$advisor[3]])
})

test_that('newWeightsByDrift works', {
  a <- data.frame(
    id = 1:3,
    initial = rep(.5, 3),
    advice = c(1, 0, 1),
    feedback = NA,
    advisor = c(2, 3, 1),
    trust_volatility = .2
  )
  g <- t(matrix(
    c(
      0, .75, 0,
      0, 0, .75,
      .75, 0, 0
    ),
    nrow = 3,
    ncol = 3
  ))
  expect_equal(round(newWeights(a, g), 2), g)
  # Update in the right direction
  a <- data.frame(
    id = 1:3,
    initial = rep(.75, 3),
    advice = c(0, 1, 1),
    feedback = NA,
    advisor = c(2, 3, 1),
    trust_volatility = .2
  )
  w <- newWeights(a, g, confidence_weighted = F)
  expect_gt(g[1, a$advisor[1]], w[1, a$advisor[1]])
  expect_lt(g[2, a$advisor[2]], w[2, a$advisor[2]])
  expect_lt(g[3, a$advisor[3]], w[3, a$advisor[3]])
  # Confidence weighted updates in the right direction but not as much
  wc <- newWeights(a, g)
  expect_gt(wc[1, a$advisor[1]], w[1, a$advisor[1]])
  expect_lt(wc[2, a$advisor[2]], w[2, a$advisor[2]])
  expect_lt(wc[3, a$advisor[3]], w[3, a$advisor[3]])
})

test_that('trustUpdate does something', {
  expect_error(
    expect_equal(
      trustUpdate(matrix(.3, nrow = 4, ncol = 1), rep(1, 4), rep(1, 4), 1),
      matrix(.3, nrow = 4, ncol = 1)
    )
  )
})

test_that('advisor_pick_probability works', {
  expect_equal(
    round(advisor_pick_probability(
      c(1, 2, 1, 3),
      c(2, 1, 3, 1),
      matrix(c(.5, 1, -.5), nrow = 4, ncol = 3, byrow = T),
      1
    ), 2),
    c(.38, .62, .73, .27)
  )
  expect_equal(
    round(advisor_pick_probability(
      c(1, 2, 1, 3),
      c(2, 1, 3, 1),
      matrix(c(5, 10, -5), nrow = 4, ncol = 3, byrow = T),
      5
    ), 2),
    c(0, 1, 1, 0)
  )
})

test_that('advisor_pick_probability is symmetrical and supports -ve weights', {
  expect_equal(
    round(advisor_pick_probability(
      c(1, 2, 1, 3),
      c(2, 1, 3, 1),
      matrix(c(.5, 1, -.5), nrow = 4, ncol = 3, byrow = T),
      1
    ), 2),
    round(advisor_pick_probability(
      c(2, 1, 3, 1),
      c(1, 2, 1, 3),
      matrix(c(.5, 1, -.5), nrow = 4, ncol = 3, byrow = T),
      -1
    ), 2)
  )
})

test_that('advisorChoiceError works', {
  trust <- matrix(
    c(.3, .3, .4, .5, .5, .5, .5, .6, .6, .5, .5,
      .3, .3, .3, .3, .4, .3, .2, .2, .3, .3, .2),
    nrow = 11,
    ncol = 2
  )
  x <- data.frame(
    trialId = 1:11,
    advisorIndex = c(rep(1, 4), rep(2, 3), rep(c(1, 2), 2)),
    choice0 = c(NA_integer_, rep(c(1, 2), 5)),
    choice1 = c(NA_integer_, rep(c(2, 1), 5))
  )
  slope = 3
  e <- advisorChoiceError(trust, x$advisorIndex, x$choice0, x$choice1, slope)
  expect_equal(
    round(e, 2),
    round(matrix(
      c(NA_real_, 1, 1, 1, .25, .40, .60, .40, .80, .40, .60,
        NA_real_, .5, .54, .57, .43, .41, .37, .67, .32, .70, .29),
      nrow = 11,
      ncol = 2
    ), 2)
  )
})

test_that('advisor_choice_error works', {
  # This is a wrapper so it should give the same answer whether wrapped or unwrapped
  trust <- matrix(
    c(.3, .3, .4, .5, .5, .5, .5, .6, .6, .5, .5,
      .3, .3, .3, .3, .4, .3, .2, .2, .3, .3, .2),
    nrow = 11,
    ncol = 2
  )
  x <- data.frame(
    trialId = 1:11,
    advisorIndex = c(rep(1, 4), rep(2, 3), rep(c(1, 2), 2)),
    choice0 = c(NA_integer_, rep(c(1, 2), 5)),
    choice1 = c(NA_integer_, rep(c(2, 1), 5))
  )
  slope = 3
  e <- advisorChoiceError(trust, x$advisorIndex, x$choice0, x$choice1, slope)
  w <- advisor_choice_error(x, trust, slope)
  expect_equal(e[,1], w[,1])
  expect_equal(e[,2], w[,2])
})

test_that('simulateFromData handles edge cases', {
  x <- data.frame(
    trialId = 1:10,
    initialConfidence = c(4.634205, 17.412019, 5.862632, 22.983083, 4.005643, 37.059090, 52.996425, 40.619095, 46.110484, 8.600329),
    finalConfidence = c(20.61316, 44.08938, 21.66968, 46.18767, 46.58179, -18.72473, 54.89270, 54.13611, 54.49334, 50.93247),
    advisorIndex = c(2, 2, 1, 1, 1, 2, 1, 1, 1, 1),
    choice0 = 1,
    choice1 = 2,
    advisorAgrees = c(T, T, T, T, T, F, T, T, T, T)
  )
  m <- simulateFromData(x, params = data.frame(a = 3, b = .5), detailed_output = T)
  expect_equal(all(m$advice_taking_error < 1e-6), T)
})

test_that('.biasCorrelation works', {
  load('data/basic-model.rda')
  load('data/bias-model.rda')
  bc <- .biasCorrelation(basic.model)
  expect_lt(max(bc$r), .9)
  expect_equal(bc, .biasCorrelation(basic.model, T))
  expect_error(
    expect_equal(
      .biasCorrelation(bias.model),
      .biasCorrelation(bias.model, T)
    )
  )
})
