% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation.R
\name{selectAdvisor}
\alias{selectAdvisor}
\title{Return the advisor selections based on \code{graph}}
\usage{
selectAdvisor(graph, exponent = 0)
}
\arguments{
\item{graph}{weighted trust matrix for agents}

\item{exponent}{power to which trust weights are raised for probabilistic
selection; 0 means selection is equally weighted}
}
\value{
vector of the advisor id selected by each agent
}
\description{
Return the advisor selections based on \code{graph}
}
