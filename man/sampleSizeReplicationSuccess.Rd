\name{sampleSizeReplicationSuccess}
\alias{sampleSizeReplicationSuccess}
\title{
Computes the required relative sample size to achieve replication 
success based on power or on the minimum relative effect size.}
\description{
  The relative sample size to achieve replication success is computed based on the 
  z-value of the original study, the replication success level, the type of 
  recalibration and either the power or the minimum relative effect size.
  When the approach based on power is used, the design prior also has to be specified.
}
\usage{
sampleSizeReplicationSuccess(zo, 
                             power, 
                             d,
                             level = 0.025,
                             alternative = "one.sided",
                             type = "golden", 
                             designPrior = "conditional",
                             shrinkage = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{zo}{
  A vector of z-values from original studies. 
  }
  \item{power}{
  The power to achieve replication success.
  }
  \item{d}{
    The minimum relative effect size (ratio of the effect estimate 
    from the replication study to the effect estimate 
    from the original study) to achieve replication success.
  }
  \item{level}{
  Replication success level. Default is 0.025.
  }
	\item{alternative}{
either \code{"one.sided"} or \code{"two.sided"}.
Defaults to \code{"one.sided"}.  Specifies if the replication success level
is one-sided or two-sided. If the replication success level is one-sided, 
then sample size calculations are based on a one-sided assessment of 
replication success 
	in the direction of the original effect estimate.
	 }
	 \item{type}{
  Type of recalibration. Can be either \code{"nominal"} (no recalibration), 
  \code{"liberal"}, \code{"controlled"} or \code{"golden"}.
  The default is \code{"golden"} which ensures that 
  for an original study just significant at the specified \code{level}, 
  replication success is only possible if the replication effect estimate is at
  least as large as the original one. See \code{?levelSceptical} for details
  about recalibration types.
}
\item{designPrior}{
    Is only taken into account when \code{"power"} is specified.
    Either \code{"conditional"}, \code{"predictive"}, or \code{"EB"}.  
    Defaults to \code{"conditional"}. If \code{"EB"}, the power is computed under
    a predictive distribution where the contribution of the original study is 
    shrunken towards zero based on the evidence in the original study 
    (with an empirical Bayes shrinkage estimator).
}
\item{shrinkage}{
   Is only taken into account when \code{"power"} is specified.
   A number in [0,1].
   Defaults to \code{0}. 
   Specifies how much the original effect estimate is shrunken towards zero 
   (e.g. the effect is shrunken by a factor of 25\% for \code{shrinkage = 0.25}).
   Is only taken into account when \code{designPrior = "conditional"} or
   \code{designPrior = "predictive"}.
  }
}
\value{
The relative sample size for replication success. If larger than 1000
\code{Inf} is returned. 
}
\references{
Held, L. (2020). A new standard for the analysis and design of replication 
studies (with discussion). 
\emph{Journal of the Royal Statistical Society: Series A (Statistics in Society)}.
183(2):431 - 448. \url{https://doi.org/10.1111/rssa.12493}
  
Held, L., Micheloud, C. & Pawel, S. (2020). The assessment of replication success 
based on relative effect size. \url{https://arxiv.org/abs/2009.07782}
}
\author{
Leonhard Held,
Charlotte Micheloud
}
\seealso{
  \code{\link{pSceptical}}, \code{\link{powerReplicationSuccess}}, 
  \code{\link{levelSceptical}}
}
\examples{

## based on power
sampleSizeReplicationSuccess(zo = p2z(0.0025), power = 0.8, level = 0.025, 
                             type = "golden")
sampleSizeReplicationSuccess(zo = p2z(0.0025), power = 0.8, level = 0.025, 
                             type = "golden", designPrior = "predictive")
           
## based on minimum relative effect size                  
sampleSizeReplicationSuccess(zo = p2z(0.0025), d = 0.9, level = 0.025, 
                             type = "nominal")
                             
sampleSizeReplicationSuccess(zo = p2z(0.0025), d = 0.9, level = 0.025, 
                             type = "golden")
}
 