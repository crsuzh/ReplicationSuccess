powerSignificance <- function(zo,
                              c = 1, 
                              level = 0.025,
                              designPrior = "conditional",
                              alternative = "one.sided",
                              h = 0,
                              shrinkage = 0) {
                              # strict = FALSE){
    
    # vectorize function in all arguments 
    pSigV <- mapply(FUN = function(zo, c, level, designPrior, 
                                   alternative, h, shrinkage) {
                                   # alternative, h, shrinkage, strict) {
        # sanity checks
        if (!(designPrior %in% c("conditional", "predictive", "EB")))
            stop('designPrior must be either "conditional", "predictive", or "EB"')
        if (!is.numeric(c) || c < 0)
            stop("c must be numeric and larger than 0")
        if (!is.numeric(h) || h < 0)
            stop("h must be numeric and cannot be negative")
        if (!is.numeric(shrinkage) || (shrinkage < 0 || shrinkage > 1)) 
            stop("shrinkage must be numeric and in [0, 1]")
        if (!is.numeric(level) || (level <= 0 || level >= 1))
            stop("level must be numeric and in (0, 1)!")
    
        # determine direction of alternative and critical value of zr
        v <- p2z(p = level, alternative = alternative) 
        lowertail <- FALSE
        if (alternative == "less") lowertail <- TRUE
        if (alternative %in% c("one.sided", "two.sided")) zo  <- abs(zo)
        
        # shrinkage is the shrinkage factor; s is 1 - shrinkage factor
        s <- 1 - shrinkage
        
        # determine parameters of predictive distribution of tr
        if(designPrior == "conditional"){
            mu <- s*zo*sqrt(c)
            sigma <- 1
        }
        if(designPrior == "predictive"){
            mu <- s*zo*sqrt(c)
            sigma <- sqrt(c + 1 + 2*h*c)
        }
        if (designPrior == "EB"){
            s <- pmax(1 - (1 + h)/zo^2, 0)
            mu <- s*zo*sqrt(c)
            sigma <- sqrt(s*c*(1 + h) + 1 + h*c)
        }
        
        # compute replication probability
        pSig <- pnorm(q = v, mean = mu, sd = sigma, lower.tail = lowertail)
        # if (alternative == "two.sided" && strict == TRUE)
        # pSig + pnorm(q = -v, mean = mu, sd = sigma)
        
        return(pSig)
    # }, zo, c, level, designPrior, alternative, d, shrinkage, strict)
    }, zo, c, level, designPrior, alternative, h, shrinkage)
    
    return(pSigV)
}
