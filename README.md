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
  n = list(p = 10, d = 200),
  conf = T,
  biasMean = 1,
  biasSD = 1,
  sensitivitySD = 1,
  learningRate = .1
)

```

We can then look at the network graph at the beginning and end:

``` r
networkGraph(model)
```


