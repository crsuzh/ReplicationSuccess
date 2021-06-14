\name{hMeanChiSqMu}
\alias{hMeanChiSqMu}
\title{Computes the p-value from the harmonic mean chi-squared test}
\description{
The p-value from the harmonic mean chi-squared test is computed based on study-specific estimates and standard errors. 
}
\usage{
hMeanChiSqMu(thetahat, se, w=rep(1, length(thetahat)), mu=0, 
             alternative="greater", bound=TRUE)
}
\arguments{
  \item{thetahat}{
    A vector of parameter estimates. 
  }
  \item{se}{
    A vector of standard errors. 
  }
  \item{w}{
    A vector of weights. 
  }
  \item{mu}{
    The null hypothesis value. Defaults to 0. 
  }
  \item{alternative}{
    Either \code{"greater"}, \code{"less"}, \code{"two.sided"} or \code{"none"}.
    Defaults to \code{"greater"}. Specifies the alternative to
    be considered in the computation of the p-value. 
    If alternative is two-sided, then a one-sided p-value for
    intrinsic credibility is computed.
  }
  \item{bound}{Determines whether p-values that cannot be computed are
    reported as "> bound" (\code{"bound=TRUE"}) or as NA
    (\code{"bound=FALSE"})
  }
}
\value{
    The p-value from the harmonic mean chi-squared test
}
\references{

Held, L. (2020). The harmonic mean chi-squared test to substantiate scientific findings. 
  \emph{Journal of the Royal Statistical Society: Series C (Applied Statistics)}, \bold{69}, 697-708.  
\url{https://doi.org/10.1111/rssc.12410}
}
\author{
Leonhard Held
}
\examples{
    
## Example from Fisher (1999) as discussed in Held (2020)
## but now based HR estimates
    
lower <- c(0.04, 0.21, 0.12, 0.07, 0.41)
upper <- c(1.14, 1.54, 0.60, 3.75, 1.27)
se <- ci2se(lower, upper, ratio=TRUE)
estimate <- ci2estimate(lower, upper, ratio=TRUE)

hMeanChiSqMu(thetahat=estimate, se=se, alternative="two.sided")
hMeanChiSqMu(thetahat=estimate, se=se, w=1/se^2, alternative="two.sided")
hMeanChiSqMu(thetahat=estimate, se=se, alternative="two.sided", mu=-0.1)

}
