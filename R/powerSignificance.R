#' @export
.powerSignificance_ <- function(zo,
                                c = 1, 
                                level = 0.025,
                                designPrior = c("conditional", "predictive", "EB"),
                                alternative = c("one.sided", "two.sided"),
                                h = 0,
                                shrinkage = 0,
                                strict = FALSE) {
    stopifnot(is.numeric(zo),
              length(zo) == 1,
              is.finite(zo),
              
              is.numeric(c),
              length(c) == 1,
              is.finite(c),
              0 <= c,
              
              is.numeric(level),
              length(level) == 1,
              is.finite(level),
              0 < level, level < 1,
              
              !is.null(designPrior))
    designPrior <- match.arg(designPrior)

    stopifnot(!is.null(alternative))
    alternative <- match.arg(alternative)

    stopifnot(is.numeric(h),
              length(h) == 1,
              is.finite(h),
              0 <= h,

              is.numeric(shrinkage),
              length(shrinkage) == 1,
              is.finite(shrinkage),
              0 <= shrinkage, shrinkage < 1,

              is.logical(strict),
              length(strict) == 1)


    ## determine direction of alternative and critical value of zr
    v <- p2z(p = level, alternative = alternative) 
    zo  <- abs(zo)
    
    ## shrinkage is the shrinkage factor; s is 1 - shrinkage factor
    s <- 1 - shrinkage
    
    ## determine parameters of predictive distribution of tr
    if(designPrior == "conditional"){
        mu <- s*zo*sqrt(c)
        sigma <- 1
    } else if(designPrior == "predictive"){
        mu <- s*zo*sqrt(c)
        sigma <- sqrt(c + 1 + 2*h*c)
    } else{ ## designPrior == "EB"
        s <- pmax(1 - (1 + h)/zo^2, 0)
        mu <- s*zo*sqrt(c)
        sigma <- sqrt(s*c*(1 + h) + 1 + h*c)
    }
    
    ## compute replication probability
    pSig <- pnorm(q = v, mean = mu, sd = sigma, lower.tail = FALSE)

    ## when strict == TRUE, add probability in the other direction for "two.sided"
    if (alternative == "two.sided" && strict){
        pSig <- pSig + pnorm(q = -v, mean = mu, sd = sigma)
    }

    return(pSig)
}

#' Computes the power for significance
#'
#' The power for significance is computed based on the result of
#' the original study, the corresponding variance ratio,
#' and the design prior.
#' @param zo Numeric vector of z-values from original studies.
#' @param c Numeric vector of variance ratios of the original and replication
#' effect estimates. This is usually the ratio of the sample
#' size of the replication study to the sample size of the original study.
#' @param level Numeric vector of significance levels. The default is 0.025.
#' @param designPrior Either "conditional" (default), "predictive", or "EB".
#' If "EB", the power is computed under a predictive distribution, where
#' the contribution of the original study is shrunken towards zero based
#' on the evidence in the original study (with an empirical Bayes shrinkage estimator).
#' @param alternative Either "one.sided" (default) or "two.sided".
#' Specifies if the significance level is one-sided or two-sided.
#' If the significance level is one-sided, then power calculations are based on a
#' one-sided assessment of significance in the direction of the
#' original effect estimates.
#' @param h The relative between-study heterogeneity, i.e., the ratio of the heterogeneity
#' variance to the variance of the original effect estimate.
#' Default is 0 (no heterogeneity).
#' Is only taken into account when \code{designPrior} = "predictive" or
#' \code{designPrior} = "EB".
#' @param shrinkage Numeric vector with values in [0,1). Defaults to 0.
#' Specifies the shrinkage of the original effect estimate towards zero, e.g.,
#' the effect is shrunken by a factor of 25\% for \code{shrinkage = 0.25}.
#' Is only taken into account if the \code{designPrior} is "conditional" or "predictive".
#' @param strict Logical vector indicating whether the probability for significance
#' in the opposite direction of the original effect estimate should also be 
#' taken into account. Default is \code{FALSE}. 
#' Only taken into account when \code{alternative} = "two.sided".
#' @return The probability that a replication study yields a significant effect estimate
#' in the specified direction.
#' @details \code{powerSignificance} is the vectorized version of \code{.powerSignificance_}.
#' \code{\link[base]{Vectorize}} is used to vectorize the function.
#' @references
#' Goodman, S. N. (1992). A comment on replication, p-values and evidence, 
#' \emph{Statistics in Medicine}, \bold{11}, 875--879. 
#' \doi{10.1002/sim.4780110705}
#'      
#' Senn, S. (2002). Letter to the Editor, \emph{Statistics in Medicine}, 
#' \bold{21}, 2437--2444. 
#' 
#' Held, L. (2020). A new standard for the analysis and design of replication
#' studies (with discussion). \emph{Journal of the Royal Statistical Society:
#' Series A (Statistics in Society)}, \bold{183}, 431-448.
#' \doi{10.1111/rssa.12493}

#' 
#' Pawel, S., Held, L. (2020). Probabilistic forecasting of replication studies.
#' \emph{PLoS ONE}. \bold{15}, e0231416. \doi{10.1371/journal.pone.0231416}
#'
#' Held, L., Micheloud, C., Pawel, S. (2021). The assessment of replication
#' success based on relative effect size. \url{https://arxiv.org/abs/2009.07782}
#' @seealso \code{\link{sampleSizeSignificance}}, \code{\link{powerSignificanceInterim}}
#' @author Leonhard Held, Samuel Pawel, Charlotte Micheloud, Florian Gerber
#' @examples
#' powerSignificance(zo = p2z(0.005), c = 2)
#' powerSignificance(zo = p2z(0.005), c = 2, designPrior = "predictive")
#' powerSignificance(zo = p2z(0.005), c = 2, alternative = "two.sided")
#' powerSignificance(zo = -3, c = 2, designPrior = "predictive",
#'                   alternative = "one.sided")
#' powerSignificance(zo = p2z(0.005), c = 1/2)
#' powerSignificance(zo = p2z(0.005), c = 1/2, designPrior = "predictive")
#' powerSignificance(zo = p2z(0.005), c = 1/2, alternative = "two.sided")
#' powerSignificance(zo = p2z(0.005), c = 1/2, designPrior = "predictive",
#'                   alternative = "two.sided")
#' powerSignificance(zo = p2z(0.005), c = 1/2, designPrior = "predictive",
#'                   alternative = "one.sided", h = 0.5, shrinkage = 0.5)
#' powerSignificance(zo = p2z(0.005), c = 1/2, designPrior = "EB",
#'                   alternative = "two.sided", h = 0.5)
#'                   
#' # power as function of original p-value
#' po <- seq(0.0001, 0.06, 0.0001)
#' plot(po, powerSignificance(zo = p2z(po), designPrior = "conditional"),
#'      type = "l", ylim = c(0, 1), lwd = 1.5, las = 1, ylab = "Power", 
#'      xlab = expression(italic(p)[o]))
#' lines(po, powerSignificance(zo = p2z(po), designPrior = "predictive"),
#'       lwd = 2, lty = 2)
#' lines(po, powerSignificance(zo = p2z(po), designPrior = "EB"),
#'       lwd = 1.5, lty = 3)
#' legend("topright", legend = c("conditional", "predictive", "EB"), 
#'        title = "Design prior", lty = c(1, 2, 3), lwd = 1.5, bty = "n")
#' @export
powerSignificance <- Vectorize(.powerSignificance_)
