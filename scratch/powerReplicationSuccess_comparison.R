rm(list=ls())
try(detach("package:ReplicationSuccess", unload=TRUE), silent=TRUE)
system("make -C .. lib")
library("ReplicationSuccess", lib.loc = "../lib/")
library(testthat)
library(dplyr)


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

f_num <- ReplicationSuccess:::powerReplicationSuccessNum
for(i in seq_len(nrow(pars_grid))){
  ## new implementation
  prs2 <- try(do.call("powerReplicationSuccess", args = pars_grid[i,1:9]), silent=TRUE)
  if(inherits(prs2, "try-error")){
    pars_grid[i,"new_error"] <- attr(prs2, "condition")$message
    pars_grid[i,"new"] <- NA
  } else {
    pars_grid[i,"new_error"] <- NA
    pars_grid[i,"new"] <- prs2
  }
    ## legacy implementation
    
  prs <- try(do.call("f_num", args = pars_grid[i,c(1:6,8)]), silent=TRUE)
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
problems %>% select(-strict) %>% head(n=100)
paste0(round(nrow(problems)/nrow(pars_grid)*100, 1), "% problems ",
       "(", nrow(problems), "/", nrow(pars_grid), ")")
summary(factor(problems$alternative)) ## no problem with two.sided
## hist(problems$zrmean)
## problems %>%
##   filter(abs(zrmean) > 1)
