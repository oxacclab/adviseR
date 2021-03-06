% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_from_data.R
\name{advisor_pick_probability}
\alias{advisor_pick_probability}
\title{Return the probability that the picked advisor is picked over the unpicked
advisor for given trust values. Assumes a sigmoid picking behaviour with
slope 1 applied to weighted picked vs unpicked trusts.}
\usage{
advisor_pick_probability(picked, unpicked, trust_vector, weight)
}
\arguments{
\item{picked}{the index in \code{trust_vector} of the picked advisor}

\item{unpicked}{the index in \code{trust_vector} of the unpicked advisor}

\item{trust_vector}{vector of subjective trust in advisors}

\item{weight}{weighting parameter used as an exponent for trust values}
}
\value{
probability picked advisor is picked over unpicked advisor
}
\description{
Return the probability that the picked advisor is picked over the unpicked
advisor for given trust values. Assumes a sigmoid picking behaviour with
slope 1 applied to weighted picked vs unpicked trusts.
}
