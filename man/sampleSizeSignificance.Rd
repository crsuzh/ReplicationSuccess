\name{sampleSizeSignificance}
\alias{sampleSizeSignificance}
\title{
  Computes the required relative sample size to achieve significance based on power
  or on the minimum relative effect size.}
\description{
  The relative sample size to achieve significance of the replication study is 
  computed based on the z-value of the original study, the significance level 
  and either the power or the minimum relative effect size.
  When the approach based on power is used, the arguments design prior,
  shrinkage, and relative heterogeneity also have to be specified.
}
\usage{
sampleSizeSignificance(zo, 
                       power, 
                       d, 
                       level = 0.025, 
                       alternative = "one.sided",
                       designPrior = "conditional", 
                       h = 0, 
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
    from the original study)
  }
  
  \item{level}{
    Significance level. Default is 0.025.
  }
   \item{alternative}{
    Either \code{"two.sided"}, \code{"one.sided"}, \code{"less"}, or \code{"greater"}.  
    Specifies direction of the alternative. 
    Defaults to \code{"one.sided"}, the same direction as the original estimate.  
  }
  \item{designPrior}{
    Is only taken into account when \code{"power"} is specified.
    Either \code{"conditional"}, \code{"predictive"}, or \code{"EB"}.  
    Defaults to \code{"conditional"}. If \code{"EB"}, the power is computed under
    a predictive distribution where the contribution of the original study is 
    shrunken towards zero based on the evidence in the original study 
    (with an empirical Bayes shrinkage estimator).
  }
  \item{h}{
  Is only taken into account when \code{"power"} is specified.
  The relative between-study heterogeneity, i.e. the ratio of the heterogeneity 
  variance to the variance of the original effect estimate.
  Default is \code{0} (no heterogeneity).
  Is only taken into account when \code{designPrior = "predictive"} or 
  \code{designPrior = "EB"}.
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
  The relative sample size to achieve significance in the specified direction. 
  If larger than 1000 then NA is returned. 
}
\references{
  Held, L. (2020). A new standard for the analysis and design of replication studies (with discussion). 
  \emph{Journal of the Royal Statistical Society: Series A (Statistics
    in Society)}, \bold{183}, 431-448.  
  \url{https://doi.org/10.1111/rssa.12493}
  
  Pawel, S., Held, L. (2020). Probabilistic forecasting of replication
  studies. \emph{PLoS ONE} 15(4):e0231416. 
  \url{https://doi.org/10.1371/journal.pone.0231416}
  
  Held, L., Micheloud, C. & Pawel, S. (2020). The assessment of replication 
  success based on relative effect size. \url{https://arxiv.org/abs/2009.07782}
}
\author{
  Leonhard Held, 
  Samuel Pawel,
  Charlotte Micheloud
}
\seealso{
  \code{\link{powerSignificance}}
}
\examples{
par(mfrow = c(1,2))
sampleSizeSignificance(zo = p2z(0.005), power = 0.8)
sampleSizeSignificance(zo = p2z(0.005, alternative = "greater"), power = 0.8)
sampleSizeSignificance(zo = p2z(0.005), power = 0.8, designPrior = "predictive")

sampleSizeSignificance(zo = 3, power = 0.8, designPrior = "predictive", 
                       shrinkage = 0.5, h = 0.25)
sampleSizeSignificance(zo = 3, power = 0.8, designPrior = "EB", 
                       h = 0.5)
                       
# required relative sample size for 0.8 power as function of original p-value
zo <- p2z(seq(0.0001, 0.05, 0.0001))
plot(z2p(zo), sampleSizeSignificance(zo = zo, designPrior = "conditional", power = 0.8),
     type = "l", ylim = c(0.5, 10), log = "y", lwd = 1.5, ylab = "Relative sample size",
     xlab = expression(italic(p)[o]), las = 1)
lines(z2p(zo), sampleSizeSignificance(zo = zo, designPrior = "predictive", power = 0.8), 
      lwd = 2, lty = 2)
lines(z2p(zo), sampleSizeSignificance(zo = zo, designPrior = "EB", power = 0.8),
      lwd = 1.5, lty = 3)
legend("topleft", legend = c("conditional", "predictive", "EB"), 
       title = "Design prior", lty = c(1, 2, 3), lwd = 1.5, bty = "n")
       
sampleSizeSignificance(zo = p2z(0.005), d = 1)
sampleSizeSignificance(zo = p2z(0.005), d = 0.5)
# required relative effect size of 0.8 power as function of original p-value
zo <- p2z(seq(0.0001, 0.05, 0.0001))
plot(z2p(zo), sampleSizeSignificance(zo = zo, d = 0.8, level = 0.025),
     type = "l", ylim = c(0.5, 10), log = "y", lwd = 1.5, ylab = "Relative sample size",
     xlab = expression(italic(p)[o]), las = 1)
}
