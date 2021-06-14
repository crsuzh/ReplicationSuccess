\name{powerReplicationSuccess}
\alias{powerReplicationSuccess}
\title{Computes the power for replication success}
\description{
  The power for replication success is computed based on the result of the 
  original study, the corresponding variance ratio and the design prior. 
}
\usage{
powerReplicationSuccess(zo, 
                        c = 1, 
                        level = 0.025,
                        designPrior = "conditional", 
                        alternative = "one.sided",
                        type = "golden",
                        shrinkage = 0)
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
    Replication success level. The default is 0.025.
    }
  \item{designPrior}{
    Either \code{"conditional"}, \code{"predictive"}, or \code{"EB"}.  
    Defaults to \code{"conditional"}. If \code{"EB"}, the power is computed under
    a predictive distribution where the contribution of the original study is
    shrunken towards zero based on the evidence in the original study (with an
    empirical Bayes shrinkage estimator).
  }
  \item{alternative}{
 either \code{"one.sided"} or \code{"two.sided"}.
Defaults to \code{"one.sided"}.  Specifies if the replication success level
is one-sided or two-sided. If the replication success level level is one-sided, 
then power calculations are based on a 
  one-sided assessment of replication success in the direction of the 
  original effect estimate.
  }
  \item{type}{
  Type of recalibration. Can be either \code{"nominal"} (no recalibration), 
  \code{"liberal"}, \code{"controlled"} or \code{"golden"}.
  The default is \code{"golden"} which ensures that 
  for an original study just significant at the specified \code{level}, 
  replication success is only possible if the replication effect estimate is
  larger than the original one. See \code{?levelSceptical} for details
  about recalibration types.
  }
  \item{shrinkage}{
    A number in [0,1].
    Defaults to \code{0}. 
    Specifies how much the original effect estimate is shrunken towards zero 
    (e.g. the effect is shrunken by a factor of 25\% for \code{shrinkage = 0.25}). 
    Is only taken into account when \code{designPrior = "conditional"} or 
    \code{designPrior = "predictive"}.
    }
  }
\value{
  The power for replication success. 
}
\references{
Held, L. (2020). A new standard for the analysis and design of replication 
studies (with discussion). \emph{Journal of the Royal Statistical Society:
Series A (Statistics in Society)}. 183(2):431 - 448. \url{https://doi.org/10.1111/rssa.12493}

Held, L., Micheloud, C. & Pawel, S. (2020). The assessment of replication success 
based on relative effect size. \url{https://arxiv.org/abs/2009.07782}
}
\author{
  Leonhard Held
}
\seealso{
  \code{\link{sampleSizeReplicationSuccess}}, \code{\link{pSceptical}}, 
  \code{\link{levelSceptical}}
}
\examples{
## larger sample size in replication (c > 1)
powerReplicationSuccess(zo = p2z(0.005), c = 2)
powerReplicationSuccess(zo = p2z(0.005), c = 2, designPrior = "predictive")

## smaller sample size in replication (c < 1)
powerReplicationSuccess(zo = p2z(0.005), c = 1/2)
powerReplicationSuccess(zo = p2z(0.005), c = 1/2, designPrior = "predictive")

}
