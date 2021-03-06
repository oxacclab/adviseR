% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/results.R
\name{biasEvolution}
\alias{biasEvolution}
\title{Return a graph of the biases of the model agents and their change over time.}
\usage{
biasEvolution(model, summaryFun = stats::median)
}
\arguments{
\item{model}{model to inspect}

\item{summaryFun}{function to use to summarise each group's bias}
}
\description{
Return a graph of the biases of the model agents and their change over time.
}
