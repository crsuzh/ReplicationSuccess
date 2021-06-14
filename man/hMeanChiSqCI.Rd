\name{hMeanChiSqCI}
\alias{hMeanChiSqCI}
\title{Computes a confidence interval by inverting the harmonic mean chi-squared test}
\description{
  A confidence interval by inverting the harmonic mean chi-squared test based on study-specific estimates and standard errors. 
}
\usage{
hMeanChiSqCI(thetahat, se, w=rep(1, length(thetahat)),
                         alternative="two.sided", level=0.95)
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
  \item{alternative}{
    Either \code{"greater"}, \code{"less"}, \code{"two.sided"} or \code{"none"}.
    Defaults to \code{"two.sided"}. Specifies the alternative to
    be considered in the computation of the confidence interval. 
  }
  \item{level}{
    The level of the confidence interval. Defaults to 0.95.
  }
}
\value{
  The confidence interval(s) from inverting the harmonic mean chi-squared test
}
\details{
  If \code{alternative} is \code{"none"}, then the function may return a
  set of (non-overlapping) confidence intervals. The output then is a vector
  of length 2n where n is the number of confidence intervals. 
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
pvalues <- c(0.0245, 0.1305, 0.00025, 0.2575, 0.128)
lower <- c(0.04, 0.21, 0.12, 0.07, 0.41)
upper <- c(1.14, 1.54, 0.60, 3.75, 1.27)
se <- ci2se(lower, upper, ratio=TRUE)
estimate <- ci2estimate(lower, upper, ratio=TRUE)

## two-sided
CI1 <- hMeanChiSqCI(thetahat=estimate, se=se, w=1/se^2, alternative="two.sided")
CI2 <- hMeanChiSqCI(thetahat=estimate, se=se, w=1/se^2, alternative="two.sided", level=0.99875)
## one-sided
CI1b <- hMeanChiSqCI(thetahat=estimate, se=se, w=1/se^2, alternative="less", level=0.975)
CI2b <- hMeanChiSqCI(thetahat=estimate, se=se, w=1/se^2, alternative="less", level=1-0.025^2)

## confidence intervals on hazard ratio scale
print(round(exp(CI1),2))
print(round(exp(CI2),2))
print(round(exp(CI1b),2))
print(round(exp(CI2b),2))

}
