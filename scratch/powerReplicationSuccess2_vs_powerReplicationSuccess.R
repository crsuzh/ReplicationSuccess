rm(list=ls())
options(width=100)
library(ReplicationSuccess)
library(testthat)
library(dplyr)
## sapply(list.files("../R", pattern='\\.R$', full.names = TRUE), source)

## - h is not an agrument of powerReplicationSuccess() ???


powerReplicationSuccess2 <- function(zo,
                                     c = 1, 
                                     level = 0.025,
                                     designPrior = "conditional",
                                     alternative = "one.sided",
                                     type = "golden",
                                     shrinkage = 0, 
                                     h = 0){
  resV <- mapply(FUN = function(zo, c, level, designPrior, 
                                alternative, type, shrinkage) {  
    # sanity checks
    if (is.na(zo))
      return(NA)
    # if (!(designPrior %in% c("conditional", "predictive", "EB")))
    #   stop('designPrior must be either "conditional", "predictive", "EB"')
    if (!is.numeric(c) || c < 0)
      stop("c must be numeric and larger than 0")
    if (!is.numeric(level) || (level <= 0 || level >= 1))
      stop("level must be numeric and in (0, 1)!")
    if (!is.numeric(shrinkage) || (shrinkage < 0 || shrinkage > 1)) 
      stop("shrinkage must be numeric and in [0, 1]")
    zo <-  abs(zo)
    # check if original study was not significant, then power is zero
    p <- z2p(z = zo, alternative = alternative)
    if (p > levelSceptical(level = level, alternative = alternative, 
                           type = type)) 
      power <- 0
    
    alphaS <-  levelSceptical(level = level, alternative = alternative, 
                            type = type)
    zalphaS <-  p2z(alphaS, alternative = alternative)
    k <-  zo^2/zalphaS^2
    dmin <- effectSizeReplicationSuccess(zo, c, level, alternative, type)
    
    if(designPrior == "conditional"){
      # if(alternative == "one.sided"){
        power <- pnorm(sqrt(c)*zo*(1-shrinkage)-zalphaS*sqrt(1 + c/(k-1)))
      # } else if (alternative == "two.sided") {
      #   power <- pnorm(sqrt(c)*zo*((1-shrinkage)-dmin)) + 
      #     pnorm(sqrt(c)*zo*(-dmin-(1-shrinkage)))
      # }
    } else if (designPrior == "predictive") {
      # if(alternative == "one.sided"){
        num <- (1-shrinkage)*zo*sqrt(c) - zalphaS*sqrt(1 + c/(k - 1))
        denom <- sqrt(c*(1 + 2*h) + 1)
        power <- pnorm(num/denom)
      # } else if (alternative == "two.sided") {
      #   power <- pnorm(1/sqrt(1+1/c)*zo*((1-shrinkage)-dmin)) +  
      #     pnorm(1/sqrt(1+1/c)*zo*(-dmin - (1-shrinkage))) 
      # }
    
    } else if (designPrior == "EB"){
      shrinkage <- pmin((1 + h)/zo^2, 1)
      num <- (1-shrinkage)*zo*sqrt(c) - zalphaS*sqrt(1 + c/(k - 1))
      denom <- sqrt(c*(1 - shrinkage + 2*h - shrinkage*h) + 1)
      power <- pnorm(num/denom)
    }
    return(power)
  }, zo, c, level, designPrior, alternative, type, shrinkage)
  
  return(resV)
}


## test if sampleSizeSignificance2() yields same results as 
vec01 <- c(0.001, 0.2532, 0.99)
vec01bound <- c(0, 0.0386, 0.5031, 1)
vec55 <- c(-5, -2.6288, 0, 0.0427, 4)
alternative <- c("two.sided", "one.sided", "less", "greater")
designPrior <- c("conditional", "predictive", "EB")
type <- c("golden", "nominal", "liberal", "controlled")
pars_grid <- expand.grid(zo=vec55,
                         c=vec01*3,
                         level=vec01,
                         alternative=alternative,
                         designPrior=designPrior,
                         type=type,
                         h=abs(vec55),
                         shrinkage=vec01bound)

## test all configurations separately
pars_grid <- cbind(pars_grid, new=NA, new_error=NA, legacy=NA, legacy_error=NA)
powerReplicationSuccess <- ReplicationSuccess::powerReplicationSuccess
for(i in seq_len(nrow(pars_grid))){
    prs2 <- try(do.call("powerReplicationSuccess2", args = pars_grid[i,1:8]), silent=TRUE)
    if(inherits(prs2, "try-error")){
        pars_grid[i,"new_error"] <- attr(prs2, "condition")$message
        pars_grid[i,"new"] <- NA
    } else {
        pars_grid[i,"new_error"] <- NA
        pars_grid[i,"new"] <- prs2
    }


    
    prs <- try(do.call("powerReplicationSuccess", args = pars_grid[i,c(1:6,8)]), silent=TRUE)
    if(inherits(prs, "try-error")){
        pars_grid[i,"legacy_error"] <- attr(prs, "condition")$message
        pars_grid[i,"legacy"] <- NA
    } else {
        pars_grid[i,"legacy_error"] <- NA
        pars_grid[i,"legacy"] <- prs
    }
}




pars_grid %>% filter(abs(new - legacy) > 0.001 |
                     (is.finite(new)  & !is.finite(new)) |
                     (!is.finite(new) & is.finite(new)))     -> problems
problems %>% nrow()
problems %>% head(n=100)

