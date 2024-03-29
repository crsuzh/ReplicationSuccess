% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pBox.R
\name{pBox}
\alias{pBox}
\alias{zBox}
\title{Computes Box's tail probability}
\usage{
pBox(zo, zr, c, level = 0.05, alternative = c("two.sided", "one.sided"))

zBox(zo, zr, c, level = 0.05, alternative = c("two.sided", "one.sided"))
}
\arguments{
\item{zo}{Numeric vector of z-values from the original studies.}

\item{zr}{Numeric vector of z-values from replication studies.}

\item{c}{Numeric vector of variance ratios of the original and replication
effect estimates. This is usually the ratio of the sample
size of the replication study to the sample size of the
original study.}

\item{level}{Numeric vector of significance levels. Default is 0.05.}

\item{alternative}{Either "two.sided" (default) or "one.sided".
Specifies whether two-sided or one-sided Box's tail
probabilities are computed.}
}
\value{
\code{pBox} returns Box's tail probabilities.

\code{zBox} returns the z-values used in \code{pBox}.
}
\description{
\code{pBox} computes Box's tail probabilities based on the z-values of the
original and the replication study, the corresponding variance ratio,
and the significance level.
}
\details{
\code{pBox} quantifies the conflict between the sceptical prior
that would render the original study non-significant and the result
from the replication study. If the original study was not significant
at level \code{level}, the sceptical prior does not exist and \code{pBox}
cannot be calculated.
}
\examples{
pBox(zo = p2z(0.01), zr = p2z(0.02), c = 2)
pBox(zo = p2z(0.02), zr = p2z(0.01), c = 1/2)
pBox(zo = p2z(0.02, alternative = "one.sided"),
     zr = p2z(0.01, alternative = "one.sided"),
     c = 1/2, alternative = "one.sided")
}
\references{
Box, G.E.P. (1980). Sampling and Bayes' inference in scientific
modelling and robustness (with discussion). \emph{Journal of the Royal
Statistical Society, Series A}, \bold{143}, 383-430.

Held, L. (2020). A new standard for the analysis and design of replication
studies (with discussion). \emph{Journal of the Royal Statistical Society:
Series A (Statistics in Society)}, \bold{183}, 431-448.
\doi{10.1111/rssa.12493}
}
\author{
Leonhard Held
}
