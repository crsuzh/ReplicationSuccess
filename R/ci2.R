ci2se <- function(lower, 
                  upper, 
                  conf.level = 0.95, 
                  ratio = FALSE){
    
    stopifnot(length(lower) == length(upper))
    stopifnot(sum(lower >= upper) == 0)
    stopifnot(conf.level > 0 & conf.level < 1)
    
    level <- 1 - conf.level
    q <- qnorm(p = 1 - level/2, lower.tail = TRUE)
    
    if(ratio == TRUE){
        stopifnot(sum(lower <= 0) == 0)
        lower <- log(lower)
        upper <- log(upper)
    }
    se <- (upper - lower)/(2*q)
    return(se)
}

ci2estimate <- function(lower, 
                        upper, 
                        ratio = FALSE, 
                        antilog = FALSE){
    
    stopifnot(length(lower) == length(upper))
    stopifnot(sum(lower >= upper) == 0)
    
    if(ratio == TRUE){
        stopifnot(sum(lower <= 0) == 0)
        lower <- log(lower)
        upper <- log(upper)
    }
    res <- (lower + upper)/2
    if((ratio == TRUE) & (antilog == TRUE))
        res <- exp(res)
    return(res)
}


ci2z <- function(lower, 
                 upper, 
                 conf.level = 0.95, 
                 ratio = FALSE){
    
    stopifnot(length(lower) == length(upper))
    stopifnot(sum(lower >= upper) == 0)
    stopifnot(conf.level > 0 & conf.level < 1)
    estimate <- ci2estimate(lower = lower, upper = upper, ratio = ratio)
    se <- ci2se(lower = lower, upper = upper, 
                conf.level = conf.level, ratio = ratio)
    z <- estimate/se
    return(z)
}

ci2p <- function(lower, 
                 upper, 
                 conf.level = 0.95, 
                 ratio = FALSE,
                 alternative = "two.sided"){

    z <- ci2z(lower = lower, upper = upper, 
              conf.level = conf.level, ratio = ratio)
    p <- z2p(z = z, alternative = alternative)
    return(p)
}

