% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation.R
\name{newWeights}
\alias{newWeights}
\title{Return the new trust matrix for agents}
\usage{
newWeights(agents, graph, confidence_weighted = T)
}
\arguments{
\item{agents}{tbl with a snapshot of agents at a given time}

\item{graph}{connectivity matrix of trust}

\item{confidence_weighted}{whether to use confidence to weight the updating or
instead rely only on dis/agreement}
}
\value{
Connectivity matrix of trust after accounting for agents' decisions
}
\description{
Return the new trust matrix for agents
}
\details{
Agents work out how expected advice was given their initial
estimate, and in/decrease their trust according to that likelihood, scaled
by each agent's trust_volatility.
}
