\name{pSceptical}
\alias{pSceptical}
\title{Computes the sceptical p-value}

\description{
  The sceptical p-value is computed based on the z-values of the
  original and the replication study and the corresponding variance ratio. 
  If specified, a recalibration is applied.
}

\usage{
pSceptical(zo, zr, c, alternative = "one.sided", type = "golden")
}

\arguments{
  \item{zo}{
    A vector of z-values from original studies.
  }
  \item{zr}{
    A vector of z-values from replication studies.
  }
  \item{c}{
    A vector of variance ratios of the original and replication
    effect estimates. This is usually the ratio of the sample
    size of the replication study to the sample size of the
    original study.
  }
  \item{alternative}{
    % Either \code{"one.sided"}, \code{"two.sided"}, \code{"greater"} or
    % \code{"less"}.  
    Either \code{"one.sided"} or \code{"two.sided"}.
    Defaults to \code{"one.sided"}. If \code{"one.sided"} then the
    sceptical p-value is based on a one-sided assessment of replication
    success in the direction of the original effect estimate. 
    If \code{"two.sided"} then the sceptical p-value is based on a two-sided
    assessment of replication success regardless of the direction of the 
    original and replication effect estimate. 
    % If \code{"greater"} or \code{"less"} then the
    % sceptical p-value is based on an assessment of replication
    % success in the positive respectively negative direction. 
  }
\item{type}{
  Type of recalibration. Can be either \code{"nominal"}, 
  \code{"liberal"}, \code{"controlled"} or \code{"golden"}.
  Defaults to \code{"golden"}.
  Setting  \code{type} to \code{"nominal"} corresponds to no recalibration 
  as in Held et al. (2020). A recalibration is
  applied if \code{type} is \code{"liberal"}, \code{"controlled"} or
  \code{"golden"} and the sceptical p-value can then be interpreted on 
  the same scale as an ordinary p-value (e.g. a one-sided sceptical p-value
  can be thresholded at the conventional 0.025 level). 
  See \code{?levelSceptical} for details about recalibration types. 
}
}

\value{
The sceptical p-value. 
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
  \code{\link{sampleSizeReplicationSuccess}}, \code{\link{powerReplicationSuccess}}, 
  \code{\link{levelSceptical}}
}

\examples{
## no recalibration (type = "nominal") as in Held (2020)
pSceptical(zo = p2z(0.01), zr = p2z(0.02), c = 2, alternative = "one.sided",
           type = "nominal")
           
## recalibration with golden level as in Held, Micheloud, Pawel (2020)
pSceptical(zo = p2z(0.01), zr = p2z(0.02), c = 2, alternative = "one.sided",
           type = "golden")

## two-sided p-values 0.01 and 0.02, relative sample size 2
pSceptical(zo = p2z(0.01), zr = p2z(0.02), c = 2, alternative = "one.sided")
## reverse the studies
pSceptical(zo = p2z(0.02), zr = p2z(0.01), c = 1/2, alternative = "one.sided")
## both p-values 0.01, relative sample size 2
pSceptical(zo = p2z(0.01), zr = p2z(0.01), c = 2, alternative = "two.sided")
}
