\name{ci2se}
\alias{ci2se}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Transforms confidence intervals to standard errors.
%%  ~~function to do ... ~~
}
\description{
    Transforms confidence intervals to the corresponding standard errors. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ci2se(lower, upper, conf.level = 0.95, ratio = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lower}{
  a vector of lower confidence interval bounds
}
  \item{upper}{
  a vector of upper confidence interval bounds
}
  \item{conf.level}{
  The confidence level of the confidence intervals. Default is 0.95.
}
 \item{ratio}{
  Indicates whether the confidence interval is for a ratio, e.g. an
  odds ratio, relative risk or hazard ratio.
  If \code{TRUE} the standard error of the log ratio is computed. 
  Defaults to \code{FALSE}.
}
}
\value{A numeric vector of standard errors. 
}
\seealso{
  \code{\link{ci2p}}, \code{\link{ci2z}}, \code{\link{ci2estimate}}, \code{\link{p2z}}, \code{\link{z2p}}
}
\examples{
ci2se(lower = 1, upper = 3)
ci2se(lower = 1, upper = 3, ratio = TRUE)
ci2se(lower = 1, upper = 3, conf.level = 0.9)
}

