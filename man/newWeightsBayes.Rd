% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation.R
\name{newWeightsBayes}
\alias{newWeightsBayes}
\title{Return new trust matrix updating using Bayes rule to combine the current
weight with the data from agreement. The agents' trust_volatility is used as
the probability for the hypothesis (that the weight assigned to the advisor
is correct).}
\usage{
newWeightsBayes(agents, graph, confidence_weighted)
}
\arguments{
\item{agents}{tbl with a snapshot of agents at a given time}

\item{graph}{connectivity matrix of trust}

\item{confidence_weighted}{wehther to use confidence to weight the updating
or instead rely only on dis/agreement}
}
\description{
Return new trust matrix updating using Bayes rule to combine the current
weight with the data from agreement. The agents' trust_volatility is used as
the probability for the hypothesis (that the weight assigned to the advisor
is correct).
}
\details{
The Bayesian integration uses 1 minus each agent's
trust_volatility as the precision of their advisor trust. This is not
hugely realistic, because it's very plausible that we may be more certain
about how un/trustworthy one advisor is than another, and we could imagine
that the precision of trustworthiness estimates would increase with greater
experience with that advisor.

The 1-trust_volatility is taken as P(H), and the P(D|H) is the probability
of the received advice given that the initial answer was correct (which is
assumed for the sake of simplicity). Where the advisor agrees, this is the
weight placed on that advisor, and where the advisor disagrees this is 1
minus that weight.

Bayesian integration then gives P(H|D) = P(H)P(D|H)/(P(D|H) + P(D|¬H))
which yields the posterior plausibility of current trust weight.
The posterior plausibility of the current trust weight is then used as to
weight the update from the current trust towards 1 or 0 depending upon
whether the advisor agreed.
This means that an advisor who offers quite surprising advice, and thus gets
a low posterior trust weight plausibility will be updated quite dramatically
in the direction of their agreement, while one whose advice is anticipated
will get only a little change.

In the confidence_weighted case, the final adjustment is weighted by the
confidence in the initial answer.
}
