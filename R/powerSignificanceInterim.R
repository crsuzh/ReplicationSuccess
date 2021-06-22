#' Interim power of a replication study.
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
#' @param alternative Specifies if the replication success level is "greater" (default), "less", or "two.sided".
#' @param shrinkage Numeric vector with values in [0,1]. Defaults to 0.
#' @details This is an extension of \code{PowerSignificance()} and adapts the `interim power'
#' from section 6.6.3 of Spiegelhalter et al. (2004) to the setting of replication studies.
#' @return
#' @seealso \code{\link{sampleSizeSignificance}}, \code{\link{powerSignificance}}
#' @references Spiegelhalter, D. J., Abrams, K. R., and Myles, J. P. (2004).
#' Bayesian Approaches to Clinical Trials and Health-Care
#' Evaluation, volume 13. John Wiley & Sons
#'
#' Micheloud, C., Held, L. (2020). Power Calculations for Replication Studies.
#' \url{https://arxiv.org/abs/2004.10814}
#' @author Charlotte Micheloud
#' Specifies the shrinkage of the original effect estimate towards zero, e.g.,
#' the effect is shrunken by a factor of 25\% for \code{shrinkage=0.25}.
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
powerSignificanceInterim <- function(zo, 
                                     zi, 
                                     c = 1, 
                                     f = 1/2,
                                     level = 0.025,
                                     designPrior = "conditional",
                                     analysisPrior = "flat",
                                     alternative = "greater",
                                     shrinkage = 0) 
{
  if (!(designPrior %in% c("conditional", "informed predictive", "predictive"))) 
    stop("designPrior must be either \"conditional\", \"informed predictive\", or \"predictive\"")
  if (!(analysisPrior %in% c("flat", "original"))) 
    stop("analysisPrior must be either \"flat\" or \"original\"")
  if (min(c, na.rm = TRUE) < 0) 
    stop("c must be larger than 0")
  if ((min(f, na.rm = TRUE) < 0 || max(f, na.rm = TRUE) > 
       1)) 
    stop("f must be in [0, 1]")
  if ((min(shrinkage, na.rm = TRUE) < 0 || max(shrinkage, na.rm = TRUE) > 
       1)) 
    stop("shrinkage must be in [0, 1]")
  
  s <- 1 - shrinkage
  
  v <- p2z(p = level, alternative = alternative)
  zo <- s * zo
  
  if (designPrior == "conditional")
    if (analysisPrior == "flat"){
      pSig <- pnorm(zo * sqrt(c * (1 - f) ) + zi*sqrt(f) / (sqrt(1-f)) - 
                      sqrt(1 / (1 - f)) * v)
    } else if (analysisPrior == "original") {
      return(NA) ## For now, we are not interested in the case where the design prior is conditional and the analysis prior normal.
    }
  
  if (designPrior == "informed predictive") {
    if (analysisPrior == "flat") {
      term1 <- sqrt(((1 - f) * c) / ((c*f + 1) * (1 + c))) * zo
      term2 <- sqrt(f*(1 + c) / ((1 - f) * (c*f + 1))) * zi
      term3 <- sqrt((c*f + 1) / ((1 + c) * (1 - f))) * v
      pSig <- pnorm(term1 + term2 - term3)
    }
    else if (analysisPrior == "original") {
      term1 <- sqrt(1 + (c*(1-f)/(c*f + 1))) * sqrt(1/(c*(1-f)))*zo
      term2 <- sqrt(1 + (c*(1-f)/(c*f + 1))) * sqrt(f/(1-f)) * zi
      term3 <- sqrt((c*f + 1)/(c*(1-f))) * v
      pSig = pnorm(term1 + term2 - term3)
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
