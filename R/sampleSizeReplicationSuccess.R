sampleSizeReplicationSuccess <- function(zo,
                                         power = NA,
                                         d = NA,
                                         level = 0.025,
                                         alternative = "one.sided",
                                         type = "golden", 
                                         designPrior = "conditional",
                                         shrinkage = 0){
    # target function for uniroot
    target <- function(zo, c, p, level, designPrior, alternative, type = type,
                       shrinkage){
        zr2 <- zr2.quantile(zo = zo, c = c, p = p, designPrior = designPrior, 
                            shrinkage = shrinkage)
        pC <- pSceptical(zo = zo, zr = sqrt(zr2), c = c, 
                         alternative = alternative, type = type)
        return(pC - level)
    }
    mylower <- 0
    myupper <- 1000
    
    # vectorize function in all arguments
    cV <- mapply(FUN = function(zo, power, d, level, alternative, type, 
                                designPrior, shrinkage) {
        # checks that only one of 'power' and 'd' has to be specified
        if (is.na(d) & is.na(power))  stop("either 'power' or 'd' has to be specified")
        if (!is.na(d) & !is.na(power))  stop("only one of 'power' or 'd' has to be specified")
        
        # sample size calculation based on power
        if (is.na(d)){
            # sanity checks
            if (!(designPrior %in% c("conditional", "predictive", "EB")))
                stop('designPrior must be either "conditional", "predictive", "EB"')
            if(!is.numeric(power) || (power <= 0 || power >= 1))
                stop("power must be numeric and in (0, 1)!")
            if(!is.numeric(level) || (level <= 0 || level >= 1))
                stop("level must be numeric and in (0, 1)!")
            if (!is.numeric(shrinkage) || (shrinkage < 0 || shrinkage > 1)) 
                stop("shrinkage must be numeric and in [0, 1]")
            
            target.l <- target(c = mylower, 
                               zo = zo, 
                               p = 1 - power, 
                               level = level,
                               designPrior = designPrior,
                               alternative = alternative,
                               type = type,
                               shrinkage = shrinkage)
            target.u <- target(c = myupper, 
                               zo = zo, 
                               p = 1 - power, 
                               level = level,
                               designPrior = designPrior,
                               alternative = alternative,
                               type = type,
                               shrinkage = shrinkage)
            if (sign(target.l) == sign(target.u)) {
                if(sign(target.u) > 0)
                    c <- Inf
                else 
                    c <- NA
            }
            else {
                c <- uniroot(f = target, 
                             lower = mylower, 
                             upper = myupper, 
                             zo = zo, 
                             p = 1 - power, 
                             level = level,
                             designPrior = designPrior,
                             alternative = alternative,
                             type = type,
                             shrinkage = shrinkage)$root
            }
        } else { # sample size calculation based on relative effect size
            # sanity checks
            if (!is.numeric(d)) 
                stop("d must be numeric")
            if (!is.numeric(level) || (level <= 0 || level >= 1)) 
                stop("level must be numeric and in (0, 1)!")
            
            alphas <- levelSceptical(level = level, 
                                     alternative = alternative, 
                                     type = type)
            zalphas <- p2z(alphas, alternative = alternative)
            K <- zo^2/zalphas^2
            denom <- d^2*K - 1/(K-1)
            if (zalphas > zo){
                warning(paste("Replication success is not achievable at this level as", 
                              zo, " < ", round(p2z(levelSceptical(level = level,
                                                                  alternative = alternative,
                                                                  type = type)),
                                               3)))
                c <- NA
            } else { 
                c <- ifelse(denom > 0, 1/denom, NA) 
            }
        }
        return(c)
    }, zo, power, d, level, alternative, type, designPrior, shrinkage)
    
    return(cV)
}

    
