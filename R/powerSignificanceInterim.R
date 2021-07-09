.powerSignificanceInterim_ <- function(zo, 
                                      zi, 
                                      c = 1, 
                                      f = 1/2,
                                      level = 0.025,
                                      designPrior = c("conditional", "informed predictive", "predictive"),
                                      analysisPrior = c("flat", "original"),
                                      alternative = c("one.sided", "two.sided"),
                                      shrinkage = 0) 
{

    stopifnot(is.numeric(zo),
              length(zo) > 0,
              is.finite(zo),
              
              is.numeric(zi),
              length(zi) > 0,
              is.finite(zi),

              is.numeric(c),
              length(c) > 0,
              is.finite(c),
              0 <= c,

              is.numeric(f),
              length(f) > 0,
              is.finite(f),
              0 <= f, f <= 1,

              is.numeric(level),
              length(level) > 0,
              is.finite(level),
              0 < level, level < 1,

              !is.null(designPrior))
    designPrior <- match.arg(designPrior)

    stopifnot(!is.null(analysisPrior))
    analysisPrior <- match.arg(analysisPrior)

    stopifnot(!is.null(alternative))
    alternative <- match.arg(alternative)
              
    stopifnot(is.numeric(shrinkage),
              length(shrinkage) > 0,
              is.finite(shrinkage),
              0 <= shrinkage, shrinkage < 1)
              
  s <- 1 - shrinkage
  
  v <- p2z(p = level, alternative = alternative)
  if(sign(zo) == -1) zi = (-1)*zi # revert the signs when zo is neg
  
  zos <- s * abs(zo)
  
  if (designPrior == "conditional")
    if (analysisPrior == "flat"){
      pSig <- pnorm(zos * sqrt(c * (1 - f) ) + zi*sqrt(f) / (sqrt(1-f)) - 
                      sqrt(1 / (1 - f)) * v)
    } else if (analysisPrior == "original") {
      return(NA) ## For now, we are not interested in the case where the design prior is conditional and the analysis prior normal.
    }
  
  if (designPrior == "informed predictive") {
    if (analysisPrior == "flat") {
      term1 <- sqrt(((1 - f) * c) / ((c*f + 1) * (1 + c))) * zos
      term2 <- sqrt(f*(1 + c) / ((1 - f) * (c*f + 1))) * zi
      term3 <- sqrt((c*f + 1) / ((1 + c) * (1 - f))) * v
      pSig <- pnorm(term1 + term2 - term3)
    }
    else if (analysisPrior == "original") {
      term1 <- sqrt(1 + (c*(1-f)/(c*f + 1))) * sqrt(1/(c*(1-f)))*zos
      term2 <- sqrt(1 + (c*(1-f)/(c*f + 1))) * sqrt(f/(1-f)) * zi
      term3 <- sqrt((c*f + 1)/(c*(1-f))) * v
      pSig <- pnorm(term1 + term2 - term3)
    }
  }
  
  if (designPrior == "predictive"){
    if (analysisPrior == "flat") {
      pSig <- pnorm((zi - sqrt(f) * v) / sqrt(1 - f))
    } else if (analysisPrior == "original"){
      return(NA)
    }
  }
  return(pSig)
}

#' Interim power of a replication study
#'
#' Computes the power of a replication study taking into account data from an interim analysis.
#'
#' @param zo Numeric vector of z-values from original studies.
#' @param zi Numeric vector of z-values from interim analyses of replication studies.
#' @param c Ratio of the sample size of the replication study to the sample size of the original study.
#' Default is 1. 
#' @param f Fraction of the replication study already completed. Default is 0.5.
#' @param level Significance level. Default is 0.025.
#' @param designPrior Either "conditional" (default), "informed predictive", or "predictive".
#' "informed predictive" refers to an informative normal prior coming from the original study.
#' "predictive" refers to a flat prior.
#' @param analysisPrior Either "flat" (default) or "original".
#' @param alternative Either "one.sided" (default) or "two.sided".
#' Specifies if the significance level is one-sided or two-sided.
#' @details This is an extension of \code{powerSignificance()} and adapts the `interim power'
#' from section 6.6.3 of Spiegelhalter et al. (2004) to the setting of replication studies.
#' @param shrinkage Numeric vector with values in [0,1). Defaults to 0.
#' Specifies the shrinkage of the original effect estimate towards zero, e.g.,
#' the effect is shrunken by a factor of 25\% for \code{shrinkage=0.25}.
#' @details \code{powerSignificanceInterim} is the vectorized version of 
#'  \code{.powerSignificanceInterim_}.
#' \code{\link[base]{Vectorize}} is used to vectorize the function.
#' @return The probability of statistical significance in the specified direction 
#' at the end of the replication study given the data collected so far 
#' in the replication study.
#' @seealso \code{\link{sampleSizeSignificance}}, \code{\link{powerSignificance}}
#' @references Spiegelhalter, D. J., Abrams, K. R., and Myles, J. P. (2004).
#' Bayesian Approaches to Clinical Trials and Health-Care
#' Evaluation, volume 13. John Wiley & Sons
#'
#' Micheloud, C., Held, L. (2021). Power Calculations for Replication Studies.
#' \url{https://arxiv.org/abs/2004.10814}
#' @author Charlotte Micheloud
#' @examples
#' powerSignificanceInterim(zo = 2, zi = 2, c = 1, f = 1/2,
#'                          designPrior = "conditional",
#'                          analysisPrior = "flat")
#'
#' powerSignificanceInterim(zo = 2, zi = 2, c = 1, f = 1/2,
#'                          designPrior = "informed predictive",
#'                          analysisPrior = "flat")
#'
#' powerSignificanceInterim(zo = 2, zi = 2, c = 1, f = 1/2,
#'                          designPrior = "predictive",
#'                          analysisPrior = "flat")
#'
#' powerSignificanceInterim(zo = 2, zi = -2, c = 1, f = 1/2,
#'                          designPrior = "conditional",
#'                          analysisPrior = "flat")
#'
#' powerSignificanceInterim(zo = 2, zi = 2, c = 1, f = 1/2,
#'                          designPrior = "conditional",
#'                          analysisPrior = "flat",
#'                          shrinkage = 0.25)
#' @export
powerSignificanceInterim <- Vectorize(.powerSignificanceInterim_)
