rm(list=ls())
options(width=100)
library(ReplicationSuccess)
library(testthat)
library(dplyr)
## sapply(list.files("../R", pattern='\\.R$', full.names = TRUE), source)

powerReplicationSuccess2 <- function(zo,
                                     c = 1, 
                                     level = 0.025,
                                     designPrior = "conditional",
                                     alternative = "one.sided",
                                     type = "golden",
                                     shrinkage = 0, 
                                     h = 0,
                                     strict = FALSE) {
  ## check that the length of all aguments is <= 1
  if (any(length(zo) > 1, length(c) > 1, length(level) > 1,
          length(designPrior) > 1, length(alternative) > 1,
          length(type) > 1, length(shrinkage) > 1, length(h) > 1,
          length(strict) > 1))
    stop("all arguments have to be of length one")

  ## input checks
  if (!is.numeric(zo))
    stop("zo must be numeric")
  if (!is.numeric(c) || c < 0)
    stop("c must be numeric and larger than 0")
  if (!is.numeric(level) || (level <= 0 || level >= 1))
    stop("level must be numeric and in (0, 1)!")
  if (!(designPrior %in% c("conditional", "predictive", "EB")))
    stop('designPrior must be either "conditional", "predictive", "EB"')
  if (!(alternative %in% c("one.sided", "two.sided")))
    stop("alternative must be 'one.sided' or 'two.sided'")
  if (!(type %in% c("nominal", "golden", "controlled", "liberal")))
    stop("type must be either 'nominal', 'golden', 'controlled', 'liberal'")
  if (!is.numeric(shrinkage) || (shrinkage < 0 || shrinkage > 1))
    stop("shrinkage must be numeric and in [0, 1]")
  if (!is.numeric(h) || h < 0)
    stop("h must be numeric and cannot be negative")
  if (!is.logical(strict))
    stop("strict must be logical")

  ## take the absolute value of zo for easier computations
  zoabs <- abs(zo)

  ## if zoas < zalphaS, power is zero
  alphaS <- levelSceptical(level = level, alternative = alternative, type = type)
  zalphaS <- p2z(alphaS, alternative = alternative)
  if (zoabs < zalphaS) {
    power <- 0
  } else {
    ## computing necessary quantities
    dmin <- effectSizeReplicationSuccess(zo = zoabs, c = c, level = level,
                                         alternative = alternative, type = type)
    zrmin <- dmin*zoabs*sqrt(c)

    if (designPrior == "conditional") {
      power <- pnorm(q = zrmin, mean = zoabs*(1 - shrinkage)*sqrt(c), sd = 1,
                     lower.tail = FALSE)
      ## computing in both directions as in legacy implementation
      ## power2 <- pnorm(q = -zrmin, mean = zoabs*(1 - shrinkage)*sqrt(c), sd = 1,
      ##                 lower.tail = TRUE)
      ## power <- power + power2

      if (strict == TRUE && alternative == "two.sided") {
        power2 <- pnorm(q = -zrmin, mean = zoabs*(1 - shrinkage)*sqrt(c), sd = 1,
                      lower.tail = TRUE)
        power <- power + power2
      }
    } else if (designPrior == "predictive") {
      power <- pnorm(q = zrmin, mean = zoabs*(1 - shrinkage)*sqrt(c),
                     sd = sqrt(c*(1 + 2*h) + 1), lower.tail = FALSE)
      ## computing in both directions as in legacy implementation
      ## power2 <- pnorm(q = -zrmin, mean = zoabs*(1 - shrinkage)*sqrt(c),
      ##                sd = sqrt(c*(1 + 2*h) + 1), lower.tail = TRUE)
      ## power <- power + power2

      if (strict == TRUE && alternative == "two.sided") {
        power2 <- pnorm(q = -zrmin, mean = zoabs*(1 - shrinkage)*sqrt(c),
                     sd = sqrt(c*(1 + 2*h) + 1), lower.tail = TRUE)
        power <- power + power2
      }
    } else { ## designPrior == "EB"
      EBshrinkage <- pmin((1 + h)/zoabs^2, 1)
      power <- pnorm(q = zrmin, mean = zoabs*(1 - EBshrinkage)*sqrt(c),
                     sd = sqrt((1 - EBshrinkage)*c*(1 + h) + 1 + c*h),
                     lower.tail = FALSE)
      ## computing in both directions as in legacy implementation
      ## power2 <- pnorm(q = -zrmin, mean = zoabs*(1 - EBshrinkage)*sqrt(c),
      ##                 sd = sqrt((1 - EBshrinkage)*c*(1 + h) + 1 + c*h),
      ##                 lower.tail = TRUE)
      ## power <- power + power2

      if (strict == TRUE && alternative == "two.sided") {
        power2 <- pnorm(q = -zrmin, mean = zoabs*(1 - EBshrinkage)*sqrt(c),
                        sd = sqrt((1 - EBshrinkage)*c*(1 + h) + 1 + c*h),
                        lower.tail = TRUE)
        power <- power + power2
      }
    }
  }
  return(power)
}


## test if powerReplicationSuccess2 yields the same result
cvec <- c(0.001, 0.5, 1, 2, 100)
vec01bound <- c(0, 0.0386, 0.5031, 1)
vec55 <- c(-5, -2.6288, 0, 0.0427, 4)
alternative <- c("two.sided", "one.sided")
designPrior <- c("conditional", "predictive", "EB")
## usually level not larger than 0.5
levelvec <- c(0.001, 0.025, 0.2, 0.49)
type <- c("golden", "nominal") ##, "liberal", "controlled")
pars_grid <- expand.grid(zo=vec55,
                         c=cvec,
                         level=levelvec,
                         alternative=alternative,
                         designPrior=designPrior,
                         type=type,
                         h=0, ## heterogeneity not supported in legacy version
                         shrinkage=vec01bound,
                         strict = TRUE, stringsAsFactors = FALSE)

## test all configurations separately
pars_grid <- cbind(pars_grid, new=NA, legacy=NA, new_error=NA, legacy_error=NA)
powerReplicationSuccess <- ReplicationSuccess::powerReplicationSuccess
for(i in seq_len(nrow(pars_grid))){
  ## new implementation
  prs2 <- try(do.call("powerReplicationSuccess2", args = pars_grid[i,1:9]), silent=TRUE)
  if(inherits(prs2, "try-error")){
    pars_grid[i,"new_error"] <- attr(prs2, "condition")$message
    pars_grid[i,"new"] <- NA
  } else {
    pars_grid[i,"new_error"] <- NA
    pars_grid[i,"new"] <- prs2
  }
  ## legacy implementation
  prs <- try(do.call("powerReplicationSuccess", args = pars_grid[i,c(1:6,8)]), silent=TRUE)
  if(inherits(prs, "try-error")){
    pars_grid[i,"legacy_error"] <- attr(prs, "condition")$message
    pars_grid[i,"legacy"] <- NA
  } else {
    pars_grid[i,"legacy_error"] <- NA
    pars_grid[i,"legacy"] <- prs
  }
  cat(".")
  if (i%%80 == 0) cat("\n")
}


pars_grid %>% filter(abs(new - legacy) > 0.001 |
                     (is.finite(new)  & !is.finite(legacy)) |
                     (!is.finite(new) & is.finite(legacy))) -> problems
## computing mean of predictive distribution of zr (under conditional and predictive DP)
problems$zrmean <- problems$zo*sqrt(problems$c)*(1 - problems$shrinkage)
problems %>% head(n=100)
paste0(round(nrow(problems)/nrow(pars_grid)*100, 1), "% problems ",
       "(", nrow(problems), "/", nrow(pars_grid), ")")
summary(factor(problems$alternative)) ## no problem with two.sided
## hist(problems$zrmean)
## problems %>%
##   filter(abs(zrmean) > 1)
