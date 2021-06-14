## confidence interval based on harmonic mean chi-squared test
# require(rootSolve) # Samuel: should never load libraries in R packages
hMeanChiSqCI <- function(thetahat, se, w = rep(1, length(thetahat)),
                         alternative = "two.sided", level = 0.95){
    stopifnot(min(w) > 0)
    stopifnot(min(se) > 0)
    if (!(alternative %in% c("two.sided", "greater", "less", "none")))
        stop('alternative must be either "two.sided", "greater", "less" or "none"')
    stopifnot((level > 0) | (level < 1))
    ## target function to compute the limits of the CI
    target <- function(limit, thetahat, se, w=w, alternative=alternative, alpha){
        res <- hMeanChiSqMu(thetahat, se, w=w, mu=limit, alternative=alternative, bound=FALSE)-alpha
        return(res)
    }
    mini <- which.min(thetahat)
    maxi <- which.max(thetahat)
    mint <- thetahat[mini]
    maxt <- thetahat[maxi]
    minse <- se[mini]
    maxse <- se[maxi]
    alpha <- 1-level
    z <- -qnorm(alpha)
    eps <- 1e-6
    factor <- 5
    if(alternative=="none"){
        CI <- rootSolve::uniroot.all(target, thetahat=thetahat, se=se, w=w, 
                                     alternative=alternative, alpha=alpha, 
                                     lower=mint-factor*z*minse, 
                                     upper=maxt+factor*z*maxse)
    }
    if(alternative=="two.sided"){
        lower <- uniroot(target, thetahat=thetahat, se=se, w=w, alternative=alternative,
                         alpha=alpha, lower=mint-factor*z*minse, upper=mint-eps*minse)$root
        upper <- uniroot(target, thetahat=thetahat, se=se, w=w, alternative=alternative,
                         alpha=alpha, lower=maxt+eps*maxse, upper=maxt+factor*z*maxse)$root
        CI <- c(lower, upper)
    }
    if(alternative=="greater"){
        lower <- uniroot(target, thetahat=thetahat, se=se, w=w, alternative=alternative,
                         alpha=alpha, lower=mint-factor*z*minse, upper=mint-eps*minse)$root
        upper <- Inf
        CI <- c(lower, upper)
    }
    if(alternative=="less"){
        lower <- -Inf
        upper <- uniroot(target, thetahat=thetahat, se=se, w=w, alternative=alternative,
                         alpha=alpha, lower=maxt+eps*maxse, upper=maxt+factor*z*maxse)$root
        CI <- c(lower, upper)
    }
    return(CI)
}

