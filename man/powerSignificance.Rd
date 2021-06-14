\name{powerSignificance}
\alias{powerSignificance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Computes the power for significance}
%%  ~~function to do ... ~~
\description{
  The power for significance is computed based on the result of
  the original study, the corresponding variance ratio
  and the design prior.
 % Computes the probability that a replication study yields a significant effect estimate in the specified direction.

}
\usage{
powerSignificance(zo, c = 1, level = 0.025, designPrior = "conditional",
                  alternative = "one.sided", h = 0, shrinkage = 0)
}
\arguments{
  \item{zo}{
    A vector of z-values from original studies. 
  }
  \item{c}{
    The ratio of the variances of the original and replication effect
    estimates. This is usually the ratio of the sample size of the
    replication study to the sample size of the original study. 
  }
  \item{level}{
    Significance level. Default is 0.025.
  }
  \item{designPrior}{
    Either \code{"conditional"}, \code{"predictive"}, or \code{"EB"}.  
    Defaults to \code{"conditional"}. If \code{"EB"}, the power is computed under
    a predictive distribution where the contribution of the original study is
    shrunken towards zero based on the evidence in the original study (with an
    empirical Bayes shrinkage estimator).
  }
  \item{alternative}{
    Either \code{"two.sided"}, \code{"one.sided"}, \code{"less"}, or \code{"greater"}.  
    Specifies direction of the alternative. 
    Defaults to \code{"one.sided"}, the same direction as the original estimate.
  }
  \item{h}{
  The relative between-study heterogeneity, i.e. the ratio of the heterogeneity
  variance to the variance of the original effect estimate. 
  Default is \code{0} (no heterogeneity).
  Is only taken into account when \code{designPrior = "predictive"} or 
  \code{designPrior = "EB"}.
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
  \details{
    This extends the 'replication probability', the probability of repeating a statistically significant result by Goodman (1992), to the case of possibly unequal sample sizes (see Senn (2002)), possible between study heterogeneity and shrinkage (see Pawel and Held (2020)). 
    
    %%  ~~ If necessary, more details than the description above ~~
  }
  \value{
    The probability that a replication study yields a significant effect estimate in the specified direction. An error is returned if it is impossible to obtain the specified power.
  }
  \references{
    Goodman, S. N. (1992). A comment on replication, p-values and evidence, 
    \emph{Statistics in Medicine}, \bold{11}, 875--879. 
    \url{https://dx.doi.org/10.1002/sim.4780110705}
    
    Senn, S. (2002). Letter to the Editor, \emph{Statistics in Medicine}, 
    \bold{21}, 2437--2444. 

    Held, L. (2020). A new standard for the analysis and design of replication 
    studies (with discussion). 
    \emph{Journal of the Royal Statistical Society: Series A (Statistics in 
    Society)}. 183(2):431 - 448. \url{https://doi.org/10.1111/rssa.12493}

   Pawel, S., Held, L. (2020). Probabilistic forecasting of replication studies.
   PLoS ONE 15(4):e0231416. \url{https://doi.org/10.1371/journal.pone.0231416}
   
   Held, L., Micheloud, C. & Pawel, S. (2020). The assessment of replication 
   success based on relative effect size. \url{https://arxiv.org/abs/2009.07782}

  }
  \author{
    Leonhard Held,
    Samuel Pawel
  }
  \seealso{
  \code{\link{sampleSizeSignificance}}, \code{\link{powerSignificanceInterim}}
}
  \examples{
powerSignificance(zo = p2z(0.005), c = 2)
powerSignificance(zo = p2z(0.005), c = 2, designPrior = "predictive")
powerSignificance(zo = p2z(0.005), c = 2, alternative = "two.sided")
powerSignificance(zo = -3, c = 2, designPrior = "predictive", 
                  alternative = "less")
powerSignificance(zo = p2z(0.005), c = 1/2)
powerSignificance(zo = p2z(0.005), c = 1/2, designPrior = "predictive")
powerSignificance(zo = p2z(0.005), c = 1/2, alternative = "two.sided")
powerSignificance(zo = p2z(0.005), c = 1/2, designPrior = "predictive",
                  alternative = "two.sided")
powerSignificance(zo = p2z(0.005), c = 1/2, designPrior = "predictive",
                  alternative = "greater", h = 0.5, shrinkage = 0.5)
powerSignificance(zo = p2z(0.005), c = 1/2, designPrior = "EB",
                  alternative = "two.sided", h = 0.5)
                  
# power as function of original p-value
po <- seq(0.0001, 0.06, 0.0001)
plot(po, 
     powerSignificance(zo = p2z(po), designPrior = "conditional"),
     type = "l", ylim = c(0, 1), lwd = 1.5, las = 1, ylab = "Power", 
     xlab = expression(italic(p)[o]))
lines(po, 
      powerSignificance(zo = p2z(po), designPrior = "predictive"), 
      lwd = 2, lty = 2)
lines(po, 
      powerSignificance(zo = p2z(po), designPrior = "EB"), 
      lwd = 1.5, lty = 3)
legend("topright", legend = c("conditional", "predictive", "EB"), 
       title = "Design prior", lty = c(1, 2, 3), lwd = 1.5, bty = "n")
}