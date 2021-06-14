## harmonic mean chi-squared test based on estimates and standard errors

hMeanChiSqMu <- function(thetahat, se, w = rep(1, length(thetahat)),
                         mu = 0, alternative = "greater", bound=TRUE){
    stopifnot(min(w) > 0)
    stopifnot(min(se) > 0)
    if (!(alternative %in% c("greater", "less", "two.sided", "none")))
        stop('alternative must be either "greater", "less", "two.sided" or "none"')
    n <- length(thetahat)
    m <- length(mu)
    if(alternative != "none"){
        z <- (thetahat - mu)/se
        zH2 <- sum(sqrt(w))^2/sum(w/z^2)
        res <- pchisq(zH2, df = 1, lower.tail = FALSE)
        check.greater <- (min(z) > 0)
        check.less <- (max(z) < 0)
        break.p <- 1/(2^n)
        if(alternative == "greater"){
            if(bound == TRUE)
                res <- ifelse(check.greater, res/(2^n), paste(">", format(break.p, scientific = FALSE)))
            if(bound == FALSE)
                res <- ifelse((check.greater | check.less), res/(2^n), NA)
        }
        if(alternative == "less"){
            if(bound == TRUE)
                res <- ifelse(check.less, res/(2^n), paste(">", format(break.p, scientific = FALSE)))
            if(bound == FALSE)
                res <- ifelse((check.greater | check.less), res/(2^n), NA)
        }
        if(alternative == "two.sided"){
            if(bound == TRUE)
                res <- ifelse((check.greater | check.less), res/(2^(n-1)), paste(">", format(2*break.p, scientific = FALSE)))
            if(bound == FALSE)
                res <- ifelse((check.greater | check.less), res/(2^(n-1)), NA)
        }
    }
    if(alternative == "none"){
        zH2 <- numeric()
        ## needs to allow for vectorial arugments
        for(i in 1:length(mu)){
            z <- (thetahat - mu[i])/se
            zH2[i] <- sum(sqrt(w))^2/sum(w/z^2)
        }
        res <- pchisq(zH2, df = 1, lower.tail = FALSE)
    }    
    return(res)
}



