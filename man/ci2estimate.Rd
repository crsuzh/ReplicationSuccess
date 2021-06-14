\name{ci2estimate}
\alias{ci2estimate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Transforms confidence intervals to parameter estimates
%%  ~~function to do ... ~~
}
\description{
    Transforms confidence intervals to the corresponding parameter estimates.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ci2estimate(lower, upper, ratio = FALSE, antilog = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lower}{
   a vector of lower confidence interval bounds
}
  \item{upper}{
   a vector of upper confidence interval bounds
}
   \item{ratio}{
     Indicates whether the confidence interval is for a ratio, e.g. an
     odds ratio, relative risk or hazard ratio, or not. 
    Defaults to \code{FALSE}.
  }
   \item{antilog}{
     Indicates whether the estimate is reported on the ratio scale. Only
     applies if \code{ratio == TRUE}. Defaults to \code{FALSE}.
  }
}
\value{A numeric vector of parameter estimates.}
\seealso{
  \code{\link{ci2p}}, \code{\link{ci2se}}, \code{\link{ci2z}}, \code{\link{p2z}}, \code{\link{z2p}}
}
\examples{
ci2estimate(lower = 1, upper = 3)
ci2estimate(lower = 1, upper = 3, ratio = TRUE)
ci2estimate(lower = 1, upper = 3, ratio = TRUE, antilog = TRUE)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
