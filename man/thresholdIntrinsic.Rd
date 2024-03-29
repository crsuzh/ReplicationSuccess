% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/thresholdIntrinsic.R
\name{thresholdIntrinsic}
\alias{thresholdIntrinsic}
\title{Computes the p-value threshold for intrinsic credibility}
\usage{
thresholdIntrinsic(
  alpha,
  alternative = c("two.sided", "one.sided"),
  type = c("Held", "Matthews")
)
}
\arguments{
\item{alpha}{Numeric vector of intrinsic credibility levels.}

\item{alternative}{Either "two.sided" (default) or "one.sided".
Specifies if the threshold is for one-sided or two-sided p-values.}

\item{type}{Either "Held" (default) or "Matthews".
Type of intrinsic p-value threshold, see Held (2019) and Matthews (2018)
for more information.}
}
\value{
The threshold for intrinsic credibility.
}
\description{
Computes the p-value threshold for intrinsic credibility
}
\examples{
thresholdIntrinsic(alpha = c(0.005, 0.01, 0.05))
thresholdIntrinsic(alpha = c(0.005, 0.01, 0.05), alternative = "one.sided")
}
\references{
Matthews, R. A. J. (2018). Beyond 'significance': principles and
practice of the analysis of credibility. \emph{Royal Society Open
Science}, \bold{5}, 171047. \doi{10.1098/rsos.171047}

Held, L. (2019). The assessment of intrinsic credibility and a new argument
for \emph{p < 0.005}. \emph{Royal Society Open Science}, \bold{6}, 181534.
\doi{10.1098/rsos.181534}
}
\author{
Leonhard Held
}
