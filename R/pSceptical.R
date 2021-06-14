pSceptical <- function(zo,
                       zr,
                       c, 
                       alternative = "one.sided",
                       type = "golden"){
    
    ## vectorize function in all arguments
    resV <- mapply(FUN = function(zo, zr, c, alternative, type) {
        ## sanity checks
        # if (!(alternative %in% c("one.sided", "two.sided", "greater", "less")))
        #     stop('alternative must be either "one.sided", "two.sided", "greater" or "less"')
        if (!(alternative %in% c("one.sided", "two.sided")))
            stop('alternative must be either "one.sided" or "two.sided"')
        if (!is.numeric(c) || c < 0)
            stop("c must be numeric and larger than 0")
        if (!(type %in% c("nominal", "liberal", "controlled", "golden")))
            stop('type must be either "nominal", "liberal", "controlled", or "golden"')
        
        z <- zSceptical(zo = zo, zr = zr, c = c)
        if(type == "nominal")
            result <- z
        if(type == "liberal"){
            result <- z*sqrt(2)
        }
        if(type == "controlled"){
            result <- p2z(p = sqrt((1 - pnorm(2*z))/2), alternative = "greater")
        }
        if(type == "golden"){
            ## golden ratio 
            phi <- (sqrt(5) + 1)/2  
            result <- z*sqrt(phi)
        }
        res <- z2p(z = result, alternative = "two.sided")
        if(alternative == "one.sided") {
            if(sign(zo) == sign(zr)) 
                res <- res/2
            else 
                res <- 1 - res/2
        }
        # if(alternative == "greater") {
        #     if(sign(zo) == 1) {
        #         if(sign(zr) == 1) res <- res/2
        #         if(sign(zr) == -1) res <- 1 - res/2
        #     }
        #     if(sign(zo) != 1)
        #         res <- NA
        # }
        # if(alternative == "less") {
        #     if(sign(zo) == -1) {
        #         if(sign(zr) == -1) res <- res/2
        #         if(sign(zr) == 1) res <- 1 - res/2
        #     }
        #     if(sign(zo) != -1)
        #         res <- NA
        # }
        return(res)
    }, zo, zr, c, alternative, type)
    return(resV)
    }
