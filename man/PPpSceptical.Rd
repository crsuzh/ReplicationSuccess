\name{PPpSceptical}
\alias{PPpSceptical}
\title{Compute project power of the sceptical p-value}

\description{
  The project power of the sceptical p-value is computed for a 
  specified level of replication success, the relative variance, 
  significance level and power for a standard significance test of
  the original study, and the alternative hypothesis.
}

\usage{
PPpSceptical(level, c, alpha, power, alternative = "one.sided", type = "golden")
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
  \item{alpha}{
    Significance level for a standard significance test in the original 
    study.
  }
  \item{power}{
    Power to detect the assumed effect with a standard significance test
    in the original study.
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
  The project power
}

\references{
Held, L. (2020). The harmonic mean chi-squared test to substantiate scientific 
findings.  \emph{Journal of the Royal Statistical Society: Series C 
(Applied Statistics)}, \bold{69}, 697-708. \url{https://doi.org/10.1111/rssc.12410}

Held, L., Micheloud, C. & Pawel, S. (2020). The assessment of replication success 
based on relative effect size. \url{https://arxiv.org/abs/2009.07782}

Maca, J., Gallo, P., Branson, M., and Maurer, W. (2002).  Reconsidering some aspects
of the two-trials paradigm. Journal of Biopharmaceutical Statistics, 12(2):107-119.
\url{https://doi.org/10.1081/bip-120006450}

}

\author{
Samuel Pawel, Leonhard Held
}

\seealso{
  \code{\link{pSceptical}}, \code{\link{levelSceptical}}, \code{\link{T1EpSceptical}}
}

\examples{
## compare project power for different levels of replication success
levels <- c("nominal" = levelSceptical(level = 0.025, type = "nominal"),
            "liberal" = levelSceptical(level = 0.025, type = "liberal"),
            "controlled" = levelSceptical(level = 0.025, type = "controlled"),
            "golden" = levelSceptical(level = 0.025, type = "golden"))
c <- seq(0.4, 5, by = 0.01)
alpha <- 0.025
power <- 0.9
pp <- sapply(X = levels, FUN = function(l) {
  PPpSceptical(level = l, c = c, alpha, power, alternative = "one.sided", type = "nominal")
})

## compute project power of 2 trials rule
za <- qnorm(p = 1 - alpha)
mu <- za + qnorm(p = power)
pp2TR <- power*pnorm(q = za, mean = sqrt(c)*mu, lower.tail = FALSE)

matplot(x = c, y = pp*100, type = "l", lty = 1, lwd = 2, las = 1, log = "x",
        xlab = bquote(italic(c)), ylab = "Project power (\%)", xlim = c(0.4, 5),
        ylim = c(0, 100))
lines(x = c, y = pp2TR*100, col = length(levels) + 1, lwd = 2)
abline(v = 1, lty = 2)
abline(h = 90, lty = 2, col = "lightgrey")
legend("bottomright", legend = c(names(levels), "2TR"), lty = 1, lwd = 2, 
       col = seq(1, length(levels) + 1))
}
