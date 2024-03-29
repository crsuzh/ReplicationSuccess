% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampleSizeSignificance.R
\name{sampleSizeSignificance}
\alias{sampleSizeSignificance}
\title{Computes the required relative sample size to achieve significance
based on power}
\usage{
sampleSizeSignificance(
  zo,
  power = NA,
  level = 0.025,
  alternative = c("one.sided", "two.sided"),
  designPrior = c("conditional", "predictive", "EB"),
  h = 0,
  shrinkage = 0
)
}
\arguments{
\item{zo}{A vector of z-values from original studies.}

\item{power}{The power to achieve replication success.}

\item{level}{Significance level. Default is 0.025.}

\item{alternative}{Either "one.sided" (default) or "two.sided".
Specifies if the significance level is one-sided or two-sided.
If the significance level is one-sided, then sample size calculations are based on a
one-sided assessment of significance in the direction of the
original effect estimate.}

\item{designPrior}{Is only taken into account when \code{power} is specified.
Either "conditional" (default), "predictive", or "EB". If "EB", the power
is computed under a predictive distribution where the contribution of the
original study is shrunken towards zero based on the evidence in the
original study (with an empirical Bayes shrinkage estimator).}

\item{h}{Is only taken into account when \code{power} is specified and
\code{designPrior} is "predictive" or "EB". The relative between-study
heterogeneity, i.e., the ratio of the heterogeneity variance to the
variance of the original effect estimate. Default is 0 (no
heterogeneity).}

\item{shrinkage}{Is only taken into account when \code{power} is specified. A
number in [0,1) with default 0. Specifies the shrinkage of the original effect
towards zero (e.g., \code{shrinkage = 0.25} implies shrinkage by a
factor of 25\%). Is only taken into account when \code{designPrior} is
"conditional" or "predictive".}
}
\value{
The relative sample size to achieve significance in the specified
    direction. If impossible to achieve the desired power for specified
    inputs \code{NaN} is returned.
}
\description{
The relative sample size to achieve significance of the replication study is
computed based on the z-value of the original study, the significance level
and the power.
}
\details{
\code{sampleSizeSignificance} is the vectorized version of
    \code{.sampleSizeSignificance_}. \code{\link[base]{Vectorize}} is used to
    vectorize the function.
}
\examples{
sampleSizeSignificance(zo = p2z(0.005), power = 0.8)
sampleSizeSignificance(zo = p2z(0.005, alternative = "two.sided"), power = 0.8)
sampleSizeSignificance(zo = p2z(0.005), power = 0.8, designPrior = "predictive")

sampleSizeSignificance(zo = 3, power = 0.8, designPrior = "predictive",
                       shrinkage = 0.5, h = 0.25)
sampleSizeSignificance(zo = 3, power = 0.8, designPrior = "EB",  h = 0.5)

# sample size to achieve  0.8 power as function of original p-value
zo <- p2z(seq(0.0001, 0.05, 0.0001))
oldPar <- par(mfrow = c(1,2))
plot(z2p(zo), sampleSizeSignificance(zo = zo, designPrior = "conditional", power = 0.8),
     type = "l", ylim = c(0.5, 10), log = "y", lwd = 1.5, ylab = "Relative sample size",
     xlab = expression(italic(p)[o]), las = 1)
lines(z2p(zo), sampleSizeSignificance(zo = zo, designPrior = "predictive", power = 0.8),
      lwd = 2, lty = 2)
lines(z2p(zo), sampleSizeSignificance(zo = zo, designPrior = "EB", power = 0.8),
      lwd = 1.5, lty = 3)
legend("topleft", legend = c("conditional", "predictive", "EB"),
       title = "Design prior", lty = c(1, 2, 3), lwd = 1.5, bty = "n")

par(oldPar)
}
\references{
Held, L. (2020). A new standard for the analysis and design of replication
studies (with discussion). \emph{Journal of the Royal Statistical Society:
Series A (Statistics in Society)}, \bold{183}, 431-448.
\doi{10.1111/rssa.12493}

Pawel, S., Held, L. (2020). Probabilistic forecasting of replication studies.
\emph{PLoS ONE}. \bold{15}, e0231416. \doi{10.1371/journal.pone.0231416}

Held, L., Micheloud, C., Pawel, S. (2022). The assessment of replication
    success based on relative effect size. The Annals of Applied Statistics.
    16:706-720. \doi{10.1214/21-AOAS1502}

Micheloud, C., Held, L. (2022). Power Calculations for Replication Studies.
 Statistical Science. 37:369-379. \doi{10.1214/21-STS828}
}
\seealso{
\code{\link{powerSignificance}}
}
\author{
Leonhard Held, Samuel Pawel, Charlotte Micheloud, Florian Gerber
}
