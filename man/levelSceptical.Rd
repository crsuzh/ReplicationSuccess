\name{levelSceptical}
\alias{levelSceptical}
\title{Computes the level for the sceptical p-value}

\description{
  The level for the sceptical p-value is computed based on the specified
  alternative and calibration type.
}

\usage{
levelSceptical(level, alternative = "one.sided", type = "golden")
}

\arguments{
\item{level}{
  The required significance level. This can be a vector.
}
\item{alternative}{
    Either \code{"one.sided"} or \code{"two.sided"} or \code{"greater"} or \code{"less"}.  
  Defaults to \code{"one.sided"}. Specifies if the significance level
  is one-sided or two-sided. If the significance level is one-sided, 
  then a one-sided level for the sceptical p-value is computed.
}
\item{type}{
  Type of calibration. Can be either \code{"nominal"}, 
  \code{"liberal"}, \code{"controlled"} or \code{"golden"}.
  Defaults to \code{"golden"}. This type ensures that 
  for an original study just significant at the specified \code{level}, 
  replication success is only possible if the replication effect estimate is
  larger than the original one.
  If \code{"type"} is \code{"controlled"} then the type-I error rate is equal
  to \code{level}^2 (for alternative = "two.sided") or 
  2 \eqn{\times}{*}\code{level}^2 (for alternative = "one.sided") if the 
  variance ratio is equal to 1. 
  The type \code{"nominal"} ensures that the type-I error rate is always smaller 
  than \code{level}^2. Significance of both the original and replication study 
  at \code{level} is then a necessary but not sufficient requirement for 
  replication success. 
  If \code{"type"} is \code{"liberal"} then significance of both studies is a 
  sufficient requirement for replication success if the variance ratio is equal 
  to 1.
}
}

\value{
The level for the sceptical p-value.
}

\references{
Held, L. (2020). A new standard for the analysis and design of replication studies (with discussion). 
  \emph{Journal of the Royal Statistical Society: Series A (Statistics
    in Society)}, \bold{183}, 431-448.  
  \url{https://doi.org/10.1111/rssa.12493}

Held, L. (2020). The harmonic mean chi-squared test to substantiate scientific findings. 
  \emph{Journal of the Royal Statistical Society: Series C (Applied Statistics)}, \bold{69}, 697-708.  
\url{https://doi.org/10.1111/rssc.12410}

Held, L., Micheloud, C. & Pawel, S. (2020). The assessment of replication success 
based on relative effect size. \url{https://arxiv.org/abs/2009.07782}
}

\seealso{
  \code{\link{pSceptical}}, \code{\link{T1EpSceptical}}
}

\author{
Leonhard Held
}

\examples{
levelSceptical(level = 0.025, type = "nominal")
levelSceptical(level = 0.025, type = "liberal")
levelSceptical(level = 0.025, type = "controlled")
levelSceptical(level = 0.025, type = "golden")
}
