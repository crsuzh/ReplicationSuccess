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
  
  # s is 1 - shrinkage
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