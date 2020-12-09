# adviseR

<!-- badges: start -->
![R-CMD-check](https://github.com/oxacclab/adviseR/workflows/R-CMD-check/badge.svg)
[![codecov](https://codecov.io/gh/oxacclab/adviseR/branch/master/graph/badge.svg)](https://codecov.io/gh/oxacclab/adviseR)
<!-- badges: end -->

The goal of adviseR is to model the interaction of simple interconnected agents who have beliefs and exchange information.
The agents can vary in their simplicity and their homogeneity.

## Installation

You can install the released version of adviseR from GitHub with:

``` r
install.packages('remotes')
remotes::install_github("oxacclab/adviseR")
```

## Example

Here we set up a small basic network:

``` r
library(adviseR)

model <- runSimulation(
  n_agents = 6,
  n_decisions = 200,
  bias_sd = 1,
  sensitivity_sd = 1,
  trust_volatility_mean = .05,
  trust_volatility_sd = .01,
  bias_volatility_mean = .05,
  bias_volatility_sd = .01
)

```

We can then look at the network graph at the beginning and end:

``` r
networkGraph(model)
```

It is also possible to run several models in parallel (using the `parallel::` package).
Note that individual models _are not_ run in parallel because each step depends upon the step before.
Instead, this approach runs separate simulations in parallel.
We can use this for, among other things, examining the robustness of a simulation by running multiple copies of it.

``` r
params <- data.frame(
  n_agents = 6,
  n_decisions = 200,
  bias_sd = c(0, 1),
  sensitivity_sd = 1,
  trust_volatility_mean = .05,
  trust_volatility_sd = .01,
  bias_volatility_mean = .05,
  bias_volatility_sd = .01
)
models <- runSimulations(params, cores = nrow(params))
```
