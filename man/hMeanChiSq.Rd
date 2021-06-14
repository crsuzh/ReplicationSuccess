\name{hMeanChiSq}
\alias{hMeanChiSq}
\title{Computes the p-value from the harmonic mean chi-squared test}
\description{
The p-value from the harmonic mean chi-squared test is computed based on study-specific z-values. 
}
\usage{
hMeanChiSq(z, w=rep(1, length(z)), alternative="greater", bound=TRUE)
}
\arguments{
  \item{z}{
    A vector of z-values. 
  }
  \item{w}{
    A vector of weights. 
  }
  \item{alternative}{
    Either \code{"greater"}, \code{"less"}, \code{"two.sided"} or \code{"none"}.
    Defaults to \code{"greater"}. Specifies the alternative to
    be considered in the computation of the p-value.
  }
  \item{bound}{Determines whether p-value that cannot be computed are
    reported as "> bound" (\code{"bound=TRUE"}) or as NA (\code{"bound=FALSE"})
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
library(ReplicationSuccess)
pvalues <- c(0.0245, 0.1305, 0.00025, 0.2575, 0.128)
lower <- c(0.04, 0.21, 0.12, 0.07, 0.41)
upper <- c(1.14, 1.54, 0.60, 3.75, 1.27)
se <- ci2se(lower, upper, ratio=TRUE)
estimate <- ci2estimate(lower, upper, ratio=TRUE)

hMeanChiSq(p2z(pvalues, alternative="less"), alternative="less")
hMeanChiSq(p2z(pvalues, alternative="less"), alternative="two.sided")
hMeanChiSq(p2z(pvalues, alternative="less"), alternative="none")

hMeanChiSq(p2z(pvalues, alternative="less"),  w=1/se^2, alternative="less")
hMeanChiSq(p2z(pvalues, alternative="less"),  w=1/se^2, alternative="two.sided")
hMeanChiSq(p2z(pvalues, alternative="less"),  w=1/se^2, alternative="none")


}
