rm(list=ls())
options(width=100)
library(ReplicationSuccess)
library(testthat)
library(dplyr)
## sapply(list.files("../R", pattern='\\.R$', full.names = TRUE), source)

## h : new argument???

sampleSizeReplicationSuccess2 = function(zo, power, level = 0.025, 
                                         type = "golden", 
                                         designPrior = "conditional", 
                                         alternative = "one.sided", 
                                         shrinkage = 0, 
                                         h = 0){
    
    cV <- mapply(FUN = function(zo, power, level, type, designPrior, 
                                alternative, shrinkage) {
        alphaS <-  levelSceptical(level = level, alternative = alternative, 
                                  type = type)
        zalphaS <-  p2z(alphaS, alternative = alternative)
        k <-  zo^2/zalphaS^2
        u <-  qnorm(power)
                                        # sanity check
        phi <-  (sqrt(5) + 1)/2
        thresh.zo <-  zalphaS*sqrt(phi)
        s <- shrinkage
        if(zo < thresh.zo) {
            warning(paste("zo must be larger than", round(thresh.zo, 3)))
            x <- NA
        } 
        if(designPrior == "conditional"){
            if (s > 1- 1/sqrt(k^2 - k)) {
                c <- NA
            } else {
                num <- (k-1)*k*u*(1-s) + sqrt((k-1)*((k-1)*(u^2*k^2*(1-s)^2) - ((1-s)^2*(k^2-k)-1)*(u^2*k-zo^2)))
                denom <- zo*((1-s)^2*(k^2-k)-1)
                x <-  num/denom
                c <-  x^2
            }
        }
        
        if(designPrior == "predictive"){
            part1 <- (1-s)/sqrt(k)
            part2 <- sqrt((1-s)^2/k - (1/k - u^2/zo^2)*((1-s)^2 - (u^2/zo^2)*
                                                        (1 + 2*h - 1/(k-1))))
            part3 <-  1/k - u^2/zo^2
            x1 <-  (part1 + part2)/part3
            x2 <- (part1 - part2)/part3
            if (power < 0.5) {
                
                c <- ifelse(x1^2 > 1/(k-1), 1/(x1^2 - 1/(k-1)), NaN)
            } else {
                c <- ifelse(x2^2 > 1/(k-1), 1/(x2^2 - 1/(k-1)), NaN)
            }
                                        # if (c < 0){
                                        #   c <- NA
                                        # }
        }
        
        if(designPrior == "EB"){
            s <- pmin((1 + h)/zo^2, 1)
            H <- 1 - s + 2*h - s*h
            part1 <- (1-s)/sqrt(k)
            part2 <- sqrt((1-s)^2/k - (1/k - u^2/zo^2)*((1-s)^2 - (u^2/zo^2)*
                                                        (H - 1/(k-1))))
            part3 <-  1/k - u^2/zo^2
            x1 <-  (part1 + part2)/part3
            x2 <- (part1 - part2)/part3
            if (power < 0.5) {
                c <- 1/(x1^2 - 1/(k-1))
            } else {
                c <- 1/(x2^2 - 1/(k-1))
            }
                                        # if (c < 0){
                                        #   c <- NA
                                        # }
        }
        
        
        return(c)
    }, zo, power, level, type, designPrior, 
    alternative, shrinkage)
    return(cV)
}



## test if sampleSizeSignificance2() yields same results as 
vec01 <- c(0.001, 0.2532, 0.99)
vec01bound <- c(0, 0.0386, 0.5031, 1)
vec55 <- c(-5, -2.6288, 0, 0.0427, 4)
alternative <- c("two.sided", "one.sided", "less", "greater")
designPrior <- c("conditional", "predictive", "EB")
type <- c("golden", "nominal", "liberal", "controlled")
pars_grid <- expand.grid(zo=vec55,
                         power=vec01,
                         level=vec01,
                         type=type,
                         designPrior=designPrior,
                         alternative=alternative,
                         shrinkage=vec01bound,
                         h=abs(vec55),
                         new=NA, new_error=NA, legacy=NA, legacy_error=NA)

## test all configurations separately
sampleSizeReplicationSuccess <- ReplicationSuccess::sampleSizeReplicationSuccess
for(i in seq_len(nrow(pars_grid))){
    prs2 <- try(do.call("sampleSizeReplicationSuccess2", args = pars_grid[i,1:8]),
                silent=TRUE)
    if(inherits(prs2, "try-error")){
        pars_grid[i,"new_error"] <- attr(prs2, "condition")$message
        pars_grid[i,"new"] <- NA
    } else {
        pars_grid[i,"new_error"] <- NA
        pars_grid[i,"new"] <- prs2
    }

    prs <- try(do.call("sampleSizeReplicationSuccess", args = pars_grid[i,1:7]),
               silent=TRUE)
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
pars_grid %>% nrow()

pars_grid %>% select(-new_error, -legacy_error) %>% head(n=100)



