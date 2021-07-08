rm(list=ls())
options(width=100)
try(detach("package:ReplicationSuccess", unload=TRUE), silent=TRUE)
system("make -C .. lib")
library("ReplicationSuccess", lib.loc = "../lib/")
library(dplyr)



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
sampleSizeSignificanceNum <- ReplicationSuccess:::.sampleSizeSignificanceNum_
for(i in seq_len(nrow(pars_grid))){
  cat(".")
  if (i%%80==0) cat("\n")
    pars_grid[i,9] <- do.call("sampleSizeSignificance", args = pars_grid[i,1:8])
    pars_grid[i,10] <- do.call("sampleSizeSignificanceNum", args = pars_grid[i,1:8])
}


pars_grid %>% filter(abs(new - legacy) > 0.001, 
                     (is.finite(new)  & !is.finite(legacy)) |
                     (!is.finite(new) & is.finite(legacy))) %>%
  ## numerical limit of legacy implementation was c = 1000
  filter(!(!is.finite(legacy) & new > 1000)) -> problems
problems %>% nrow()
problems %>% select(-d)
