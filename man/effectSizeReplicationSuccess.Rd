\name{effectSizeReplicationSuccess}
\alias{effectSizeReplicationSuccess}
\title{
  Computes the minimum relative effect size to achieve replication success
  }
\description{
The minimum relative effect size (replication to original) to achieve replication success is computed based on the result of the original study and the corresponding variance ratio (relative sample size).}
\usage{
effectSizeReplicationSuccess(zo, 
                             c = 1,
                             level = 0.025, 
                             alternative = "one.sided", 
                             type = "golden")
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
    Replication success level. Default is 0.025. 
  }
  
  \item{alternative}{
 either \code{"one.sided"} or \code{"two.sided"}.
Defaults to \code{"one.sided"}.  Specifies if the replication success level
is one-sided or two-sided. If the significance level is one-sided, 
then effect size calculations are based on a one-sided assessment of 
replication success 
	in the direction of the original effect estimate.
}
  \item{type}{
  Type of recalibration. Can be either \code{"nominal"} (no recalibration), 
  \code{"liberal"}, \code{"controlled"} or \code{"golden"}.
  The default is \code{"golden"} which ensures that 
  for an original study just significant at the specified \code{level}, 
  replication success is only possible if the replication effect estimate 
  larger than the original one. See \code{?levelSceptical} for details
  about recalibration types.
}
}
\value{
  The minimum relative effect size to achieve replication success
}

\references{
Held, L., Micheloud, C. & Pawel, S. (2020). The assessment of replication success 
based on relative effect size. \url{https://arxiv.org/abs/2009.07782}
}

\author{
  Leonhard Held, 
  Charlotte Micheloud
}
\seealso{
  \code{\link{sampleSizeReplicationSuccess}}, \code{\link{levelSceptical}}

}
\examples{
                                  
po <- c(0.001, 0.002, 0.01, 0.02, 0.025)
zo <- p2z(po, alternative = "one.sided")

effectSizeReplicationSuccess(zo = zo, 
                             c = 1, 
                             level = 0.025, 
                             alternative = "one.sided", 
                             type = "golden")
              
effectSizeReplicationSuccess(zo = zo, 
                             c = 10, 
                             level = 0.025, 
                             alternative = "one.sided", 
                             type = "golden")
              
              
effectSizeReplicationSuccess(zo = zo, 
                             c = 2, 
                             level = 0.025, 
                             alternative = "one.sided", 
                             type = "nominal")
       
effectSizeReplicationSuccess(zo = zo, 
                             c = 2, 
                             level = 0.05, 
                             alternative = "two.sided", 
                             type = "nominal")
              
              

}
