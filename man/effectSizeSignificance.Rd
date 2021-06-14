\name{effectSizeSignificance}
\alias{effectSizeSignificance}
\title{
  Computes the minimum relative effect size to achieve significance 
  of the replication study
  }
\description{
The minimum relative effect size (replication to original) to achieve significance
of the replication study is computed based on the result of the original study and 
the corresponding variance ratio.}
\usage{
effectSizeSignificance(zo,
                       c = 1,
                       level = 0.025, 
                       alternative = "one.sided")
       }
\arguments{
  \item{zo}{
    A vector of z-values from original studies. 
  }
  \item{c}{
    A vector of variance ratios of the original and replication
    effect estimates. This is usually the ratio of the sample
    size of the replication study to the sample size of the
    original study.
  }
  \item{level}{
    Significance level. Default is 0.025
  }
  
\item{alternative}{
  Either \code{"one.sided"} or \code{"two.sided"}.  Defaults to 
  \code{"one.sided"}. Specifies if the p-value is one-sided or two-sided. 
  % If \code{"one.sided"}, then power calculations are based on a 
  % one-sided assessment of replication success in the direction of the 
  % original effect estimate.
}
}
\value{
  The minimum relative effect size to achieve significance of the replication 
  study.
}

\references{
Held, L., Micheloud, C. & Pawel, S. (2020). The assessment of replication success 
based on relative effect size. \url{https://arxiv.org/abs/2009.07782}
}

\author{
  Charlotte Micheloud
}
\seealso{
  \code{\link{effectSizeReplicationSuccess}}

}
\examples{
po <- c(0.001, 0.002, 0.01, 0.02, 0.025)
zo <- p2z(po, alternative = "one.sided")

effectSizeSignificance(zo = zo, 
                       c = 1, 
                       level = 0.025, 
                       alternative = "one.sided")
              
effectSizeSignificance(zo = zo, 
                       c = 1, 
                       level = 0.05, 
                       alternative = "two.sided")
                   
effectSizeSignificance(zo = zo, 
                       c = 50, 
                       level = 0.025, 
                       alternative = "one.sided")

}
