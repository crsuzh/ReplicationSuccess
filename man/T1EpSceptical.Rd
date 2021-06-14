\name{T1EpSceptical}
\alias{T1EpSceptical}
\title{Compute type-I error rate of the sceptical p-value}

\description{
  The type-I error rate of the sceptical p-value is computed for a 
  specified level of replication success, the relative variance, 
  and the alternative hypothesis.
}

\usage{
T1EpSceptical(level, c, alternative = "one.sided", type = "golden")
}

\arguments{
  \item{level}{
    A vector of levels of replication success 
  }
  \item{c}{
    A vector of variance ratios of the original and replication
    effect estimates. This is usually the ratio of the sample
    size of the replication study to the sample size of the
    original study.
  }
  \item{alternative}{
    Either \code{"one.sided"} or \code{"two.sided"} or \code{"greater"} or \code{"less"}.  
    Defaults to \code{"one.sided"}. If \code{"one.sided"} then the
    type-I error rate is computed based on a one-sided assessment of
    replication success in the direction of the original effect estimate. 
    If \code{"two.sided"} then the type-I error rate is computed based
    on a two-sided assessment of replication success
    regardless of the direction of the original and replication effect estimate.
    If \code{"greater"} or \code{"less"} then the type-I error rate is
    computed based on a one-sided assessment of replication success
    in the pre-specified direction of the original and replication effect estimate.
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
}

\value{
  The type-I error rate
}

\references{
Held, L. (2020). The harmonic mean chi-squared test to substantiate scientific 
findings.  \emph{Journal of the Royal Statistical Society: Series C 
(Applied Statistics)}, \bold{69}, 697-708. \url{https://doi.org/10.1111/rssc.12410}

Held, L., Micheloud, C. & Pawel, S. (2020). The assessment of replication success 
based on relative effect size. \url{https://arxiv.org/abs/2009.07782}
}

\author{
Samuel Pawel, Leonhard Held
}

\seealso{
  \code{\link{pSceptical}}, \code{\link{levelSceptical}}, \code{\link{PPpSceptical}}
}

\examples{
## compare type-I error rate for different levels of replication success
levels <- c("nominal" = levelSceptical(level = 0.025, type = "nominal"),
            "liberal" = levelSceptical(level = 0.025, type = "liberal"),
            "controlled" = levelSceptical(level = 0.025, type = "controlled"),
            "golden" = levelSceptical(level = 0.025, type = "golden"))
c <- seq(0.2, 5, by = 0.05)
t1 <- sapply(X = levels, FUN = function(l) {
  T1EpSceptical(level = l, c = c, alternative = "one.sided", type = "nominal")
})
matplot(x = c, y = t1*100, type = "l", lty = 1, lwd = 2, las = 1, log = "x",
        xlab = bquote(italic(c)), ylab = "Type-I error (\%)", xlim = c(0.2, 5))
legend("topright", legend = names(levels), lty = 1, lwd = 2, col = seq_along(levels))

## check that one.sided controlled level controls type-I error rate for c = 1 
## at alpha = 0.05*0.025 = 0.00125
T1EpSceptical(level = levelSceptical(level = 0.025, alternative = "one.sided", 
                                     type = "controlled"), 
              c = 1, alternative = "one.sided",  type = "nominal")
}
