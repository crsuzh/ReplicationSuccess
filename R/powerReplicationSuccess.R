# function that returns z_r^2 quantile for given z_o, c
zr2.quantile <- function(zo, 
                         c, 
                         p, 
                         designPrior,
                         shrinkage){
    
    if (designPrior == "predictive"){
        lambda <- (1 - shrinkage)^2*zo^2/(1 + 1/c)
        factor <- c + 1 
        # h <- 0 # relative heterogeneity h, implement later
        # lambda <- (1 - shrinkage)^2*zo^2/(1/c + 1 + 2*h)
        # factor <- 1 + c + 2*h*c
    }
    if (designPrior == "conditional"){
        lambda <- (1 - shrinkage)^2*c*zo^2
        factor <- 1
    }
    if (designPrior == "EB") {
        s <- pmax(1 - 1/zo^2, 0)
        lambda <- zo^2*s^2/(s + 1/c)
        factor <- s*c + 1
        # h <- 0 # relative heterogeneity h, implement later
        # s <- pmax(1 - (1 + h)/zo^2, 0)
        # lambda <- zo^2*s^2/(s*(1 + h) + 1/c + h)
        # factor <- s*c*(1 + h) + 1 + h*c
    }
    if (lambda < 100)
        res <- qchisq(p = p, df = 1, ncp = lambda)
    else
        res <- qnorm(p = p, mean = sqrt(lambda), sd = 1)^2
    return(factor*res)
}

powerReplicationSuccess <- function(zo,
                                    c = 1, 
                                    level = 0.025,
                                    designPrior = "conditional",
                                    alternative = "one.sided",
                                    type = "golden",
                                    shrinkage = 0){
    
    targetPower <- function(power, zo, c, level, designPrior, alternative, type,
                            shrinkage){
        zr2 <- zr2.quantile(zo = zo, c = c, p = 1 - power, 
                            designPrior = designPrior, shrinkage = shrinkage)
        pC <- pSceptical(zo = zo, zr = sqrt(zr2), c = c,
                         alternative = alternative, type = type)
        return(pC - level)
        # pC <- pSceptical(zo = zo, zr = sqrt(zr2), c = c, 
        #                  alternative = alternative)
        # return(pC - levelSceptical(level = level, alternative = alternative,
        #                                type = type))
    }
    
    # vectorize function in all arguments
    resV <- mapply(FUN = function(zo, c, level, designPrior, alternative, type,
                                  shrinkage) {
        
        # sanity checks
        if (is.na(zo))
            return(NA)
        if (!(designPrior %in% c("conditional", "predictive", "EB")))
            stop('designPrior must be either "conditional", "predictive", "EB"')
        if (!is.numeric(c) || c < 0)
            stop("c must be numeric and larger than 0")
        if (!is.numeric(level) || (level <= 0 || level >= 1))
            stop("level must be numeric and in (0, 1)!")
        if (!is.numeric(shrinkage) || (shrinkage < 0 || shrinkage > 1)) 
            stop("shrinkage must be numeric and in [0, 1]")
        
        # check if original study was not significant, then power is zero
        zo <- abs(zo)
        p <- z2p(z = zo, alternative = alternative)
        eps <- 1e-5
        mylower <- eps
        myupper <- 1 - eps
        
        if (p > levelSceptical(level = level, alternative = alternative, 
                               type = type)) res <- 0
        else {
            target.l <- targetPower(power = mylower, 
                                    zo = zo, 
                                    c = c, 
                                    level = level,
                                    designPrior = designPrior,
                                    alternative = alternative,
                                    type = type,
                                    shrinkage = shrinkage)
            target.u <- targetPower(power = myupper, 
                                    zo = zo, 
                                    c = c, 
                                    level = level,
                                    designPrior = designPrior,
                                    alternative = alternative,
                                    type = type,
                                    shrinkage = shrinkage)
            if (sign(target.l) == sign(target.u)) {
                if ((sign(target.l) >= 0) & (sign(target.u) >= 0))
                    res <- 0
                if ((sign(target.l) < 0) & (sign(target.u) < 0))
                    res <- 1
            }
            else {
                res <- uniroot(f = targetPower, 
                               lower = mylower, 
                               upper = myupper,
                               zo = zo, 
                               c = c, 
                               level = level,
                               designPrior = designPrior,
                               alternative = alternative,
                               type = type,
                               shrinkage = shrinkage)$root
            }
        }
        return(res)
    }, zo, c, level, designPrior, alternative, type, shrinkage)
    
    return(resV)
}
