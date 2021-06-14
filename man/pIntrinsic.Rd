\name{pIntrinsic}
\alias{pIntrinsic}
\title{Computes the p-value for intrinsic credibility}
\description{
The p-value for intrinsic credibility is computed. 
}
\usage{
pIntrinsic(p = z2p(z, alternative = alternative), z = NULL, alternative = "two.sided", 
           type = "Held")
}
\arguments{
  \item{p}{
  A vector of p-values, is calculated from \code{z}, if necessary.
  }
  \item{z}{
  A vector of z-values, default is \code{NULL}
  }
  \item{alternative}{
  Either \code{"one.sided"} or \code{"two.sided"}. Defaults to \code{"two.sided"}. Specifies if the p-value is one-sided or
two-sided. If the p-value is one-sided, then a one-sided p-value for
intrinsic credibility is computed.
  }
  \item{type}{
  Type of intrinsic p-value. Default is \code{"Held"} as in
  Held (2019). The other option is \code{"Matthews"} as in Matthews
  (2018).
  }
}
\value{
The p-value for intrinsic credibility. 
}
\references{
Matthews, R. A. J. (2018).  Beyond 'significance': principles and
practice of the analysis of credibility.
\emph{Royal Society Open Science}, 5:171047. \url{https://doi.org/10.1098/rsos.171047}

Held, L. (2019). The assessment of intrinsic
credibility and a new argument for p < 0.005. \emph{Royal Society Open
Science}, 6:181534. \url{https://dx.doi.org/10.1098/rsos.181534}
}
\author{
Leonhard Held
}
\examples{
p <- c(0.005, 0.01, 0.05)
z <- c(2, 3, 4)
pIntrinsic(p = p)
pIntrinsic(p = p, type = "Matthews")
pIntrinsic(p = p, alternative = "one.sided")
pIntrinsic(p = p, alternative = "one.sided", type = "Matthews")
}
