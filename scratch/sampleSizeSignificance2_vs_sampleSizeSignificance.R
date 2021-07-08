rm(list=ls())
options(width=100)
library(ReplicationSuccess)
library(testthat)
library(dplyr)
## sapply(list.files("../R", pattern='\\.R$', full.names = TRUE), source)


## from new-formulas.R
sampleSizeSignificance2 <- function(zo,
                                    power = NA,
                                    d = NA,
                                    level = 0.025,
                                    alternative = "one.sided",
                                    designPrior = "conditional",
                                    h = 0,
                                    shrinkage = 0) {

  ## check that the length of all aguments is <= 1
  if(any(length(zo) > 1, length(power) > 1, length(d) > 1, length(level) > 1,
         length(alternative) > 1, length(designPrior) > 1, length(h) > 1,
         length(shrinkage) > 1))
    stop("all arguments have to be of length one")

  ## checks that only one of 'power' and 'd' has to be specified
  if (is.na(d) && is.na(power))
    stop("either 'power' or 'd' has to be specified")
  if (!is.na(d) && !is.na(power))
    stop("only one of 'power' or 'd' has to be specified")

  ## common input checks for both 'power' and 'd' approach
  
  if (!is.numeric(level) || (level <= 0 || level >= 1))
    stop("level must be numeric and in (0, 1)!")
  if (!(alternative %in% c("one.sided", "two.sided")))
    stop("alternative must be 'one.sided' or 'two.sided'")

  ## sample size calculation based on power
  if (is.na(d)) {
    ## input checks
    if (!(designPrior %in% c("conditional", "predictive", "EB")))
      stop('designPrior must be either "conditional", "predictive", or "EB"')
    if (!is.numeric(power) || (power <= 0 || power >= 1))
      stop("power must be numeric and in (0, 1)")
    if (level >= power)
      stop("power must larger than level")
    if (!is.numeric(h) || h < 0)
      stop("h must be numeric and cannot be negative")
    if (!is.numeric(shrinkage) || (shrinkage < 0 || shrinkage > 1))
      stop("shrinkage must be numeric and in [0, 1]")

    ## preliminary computations
    u <- qnorm(p = power)
    v <- p2z(p = level, alternative = alternative)
    zoabs <- abs(zo)

    ## conditional
    if (designPrior == "conditional") {
      c <- (u + v)^2*(1/((1 - shrinkage)*zoabs))^2
    } else {
      ## computing parameters
      if (designPrior == "EB") {
        shrinkage <- pmin((1 + h)/zoabs^2, 1)
        H <- 1 - shrinkage + 2*h - shrinkage*h
      } else {
        H <- 1 + 2*h
      }
      ## checking whether power larger than power limit
      ## powLim <- pnorm(q = (1 - shrinkage)*zoabs/sqrt(H))
      ## if (is.na(powLim) power > powLim) {
      ##   c <- NaN
      ##   warning(paste0("Power cannot be larger than ", round(powLim, 3)))
      ## } else {
        if ((zoabs^2*(1 - shrinkage)^2 <= H*u^2) && power > 0.5) {
          c <- NaN
        } else {
          zos <- (1 - shrinkage)*zoabs
          num <- zos*v + u*sqrt(zos^2 + H*(v^2 - u^2))
          denom <- zos^2 - u^2*H
          sqrtc <- num/denom
          if (is.na(sqrtc) || sqrtc < 0) {
            c <- NaN
          } else {
            c <- sqrtc^2
          }
        ## }
      }
    }
  } else { ## sample size calculation based on relative effect size
    ## input checks
    if (!is.numeric(d)) stop("d must be numeric")
    zalpha <- qnorm(1 - level)
    zalpha <- p2z(p = level, alternative = alternative)
    c <- zalpha^2/(d^2*zo^2)
  }
  return(c)
}


## test if sampleSizeSignificance2() yields same results as 


vec01 <- c(0.001, 0.2532, 0.99)
vec01bound <- c(0, 0.0386, 0.5031, 1)
vec55 <- c(-5, -2.6288, 0, 0.0427, 4)
alternative <- c("two.sided", "one.sided") #, "less", "greater")
designPrior <- c("conditional", "predictive", "EB")
## power should only be larger than level
powvec <- c(0.499, 0.51, 0.8, 0.975)
levelvec <- c(0.001, 0.025, 0.2, 0.49)
pars_grid_power <- expand.grid(zo=vec55,
                               power=powvec,
                               d=NA,
                               level=levelvec,
                               alternative=alternative,
                               designPrior=designPrior,
                               h=abs(vec55),
                               shrinkage=vec01bound, stringsAsFactors = FALSE)
pars_grid_d <- expand.grid(zo=vec55,
                           power=NA,
                           d=vec01,
                           level=.025,
                           alternative="one.sided",
                           designPrior="conditional",
                           h=0,
                           shrinkage=0, stringsAsFactors = FALSE)
pars_grid <- rbind(pars_grid_power, pars_grid_d)

## test all configurations separately
pars_grid <- cbind(pars_grid, new=NA, legacy=NA)
sampleSizeSignificance <- ReplicationSuccess::sampleSizeSignificance
for(i in seq_len(nrow(pars_grid))){
  cat(".")
  if (i%%80==0) cat("\n")
    pars_grid[i,9] <- do.call("sampleSizeSignificance2", args = pars_grid[i,1:8])
    pars_grid[i,10] <- do.call("sampleSizeSignificance", args = pars_grid[i,1:8])
}


pars_grid %>% filter(abs(new - legacy) > 0.001 |
                     (is.finite(new)  & !is.finite(legacy)) |
                     (!is.finite(new) & is.finite(legacy))) %>%
  ## numerical limit of legacy implementation was c = 1000
  filter(!(!is.finite(legacy) & new > 1000)) -> problems
problems %>% nrow()
problems %>% select(-d)
