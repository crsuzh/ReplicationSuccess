sampleSizeSignificance <- function(zo,
                                   power = NA,
                                   d = NA,
                                   level = 0.025,
                                   alternative = "one.sided",
                                   designPrior = "conditional",
                                   h = 0,
                                   shrinkage = 0) {
    
    # Target function for calculating required sample size using uniroot
    ClassicalTarget <- function(c, zo, level, power, alternative,
                                h, shrinkage, designPrior){
        term <- powerSignificance(zo = zo, 
                                  c = c, 
                                  level = level,
                                  designPrior = designPrior,
                                  alternative = alternative,
                                  h = h,
                                  shrinkage = shrinkage)
        return(term - power)
    }
    n.l <- 0
    n.u <- 1000
    
    # vectorize function in all arguments
    cV <- mapply(FUN = function(zo, power, d, level, alternative, designPrior,
                                h, shrinkage) {
        # checks that only one of 'power' and 'd' has to be specified
        if (is.na(d) & is.na(power))  stop("either 'power' or 'd' has to be specified")
        if (!is.na(d) & !is.na(power))  stop("only one of 'power' or 'd' has to be specified")
        
        # sample size calculation based on power
        if (is.na(d)) {
            
            # sanity checks
            if (!(designPrior %in% c("conditional", "predictive", "EB")))
                stop('designPrior must be either "conditional", "predictive", or "EB"')
            if (!is.numeric(power) || (power <= 0 || power >= 1)) 
                stop("power must be numeric and in (0, 1)")
            if (!is.numeric(level) || (level <= 0 || level >= 1)) 
                stop("level must be numeric and in (0, 1)")
            if (!is.numeric(h) || h < 0)
                stop("h must be numeric and cannot be negative")
            if (!is.numeric(shrinkage) || (shrinkage < 0 || shrinkage > 1)) 
                stop("shrinkage must be numeric and in [0, 1]")
            
            # s is 1 - shrinkage
            s <- 1 - shrinkage
            
            # for conditional designPrior use analytical solution
            if (designPrior == "conditional") {
                u <- qnorm(p = power)
                v <- p2z(p = level, alternative = alternative)
                c <- (u + v)^2*(1/(s*zo))^2
            }
            
            # for predictive and EB designPrior use uniroot
            if (designPrior %in% c("predictive", "EB")) {
                
                # compute upper bound of power
                # if (designPrior == "EB") s <- pmax(1 - (1 + h)/zo^2, 0)
                # power.limit <- pnorm(sqrt(1/(s*(1 + h) + h))*s*abs(zo))
                # if (power > power.limit) {
                #     power.limit.r <- floor(power.limit * 1000)/1000
                #     warning(paste("power too large, power should not exceed",
                #                    power.limit.r,
                #                   "for a zo of",
                #                   zo,
                #                   "\n"))
                #     c <- NaN
                # } else {
                
                # check whether desired power can be achieved for max c = n.u
                target.l <- ClassicalTarget(c = n.l, 
                                            zo = zo,
                                            level = level,
                                            power = power,
                                            alternative = alternative,
                                            h = h,
                                            shrinkage = shrinkage,
                                            designPrior = designPrior)
                target.u <- ClassicalTarget(c = n.u, 
                                            zo = zo,
                                            level = level,
                                            power = power,
                                            alternative = alternative,
                                            h = h,
                                            shrinkage = shrinkage,
                                            designPrior = designPrior)
                if (sign(target.l) == sign(target.u)) 
                    c <- NaN
                # determine c to achieve desired power
                else c <- uniroot(f = ClassicalTarget, 
                                  lower = n.l, 
                                  upper = n.u,
                                  zo = zo,
                                  level = level,
                                  power = power, 
                                  alternative = alternative,
                                  h = h,
                                  shrinkage = shrinkage,
                                  designPrior = designPrior)$root
            }
            # }
        } else { # sample size calculation based on relative effect size
            # sanity checks
            if (!is.numeric(d)) 
                stop("d must be numeric")
            if (!is.numeric(level) || (level <= 0 || level >= 1)) 
                stop("level must be numeric and in (0, 1)!")
            
            zalpha <- qnorm(1- level)
            zalpha <- p2z(p = level, alternative = alternative)
            c <- zalpha^2/(d^2*zo^2)
        }
        return(c)
    }, zo, power, d, level, alternative, designPrior, h, shrinkage)
    
    return(cV)
}

    