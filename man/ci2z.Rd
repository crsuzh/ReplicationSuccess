\name{ci2z}
\alias{ci2z}
\title{
    Transforms confidence intervals to z-values. 
%%  ~~function to do ... ~~
}
\description{
    Transforms confidence intervals to the corresponding z-values.
}
\usage{
ci2z(lower, upper, conf.level = 0.95, ratio = FALSE)
}
\arguments{
  \item{lower}{
   a vector of lower confidence interval bounds
}
  \item{upper}{
   a vector of upper confidence interval bounds
}
  \item{conf.level}{
    The confidence level of the confidence intervals. Default is 0.95
  }
 \item{ratio}{
   Indicates whether the confidence interval is for a ratio, e.g. an
   odds ratio, relative risk or hazard ratio. If \code{TRUE} the
   z-value of the log ratio is computed. 
   Defaults to \code{FALSE}.
}
}
\value{A numeric vector of z-values.
}
\seealso{
  \code{\link{ci2p}}, \code{\link{ci2se}}, \code{\link{ci2estimate}}, \code{\link{p2z}}, \code{\link{z2p}}
}
\examples{
ci2z(lower = 1, upper = 3)
ci2z(lower = 1, upper = 3, ratio = TRUE)
ci2z(lower = 1, upper = 3, conf.level = 0.9)
}

