Qtest <- function(thetao, thetar, seo, ser) {
    # vectorize function in all arguments
    pvalQvec <- mapply(FUN = function(thetao, thetar, seo, ser) {
        # sanity checks
        if (!is.numeric(thetao)) 
            stop("thetao must be numeric")
        if (!is.numeric(thetar)) 
            stop("thetar must be numeric")
        if (!is.numeric(seo) || seo <= 0)
            stop("seo must be numeric and larger than 0")
        if (!is.numeric(ser) || ser <= 0)
            stop("ser must be numeric and larger than 0")
        
        # compute Q-test statistic and p-value
        Q <- (thetao - thetar)^2/(seo^2 + ser^2)
        pvalQ <- stats::pchisq(q = Q, df = 1, ncp = 0, lower.tail = FALSE)
        return(pvalQ)
    }, thetao, thetar, seo, ser)
    return(pvalQvec)
}
