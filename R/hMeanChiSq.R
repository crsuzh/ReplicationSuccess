#' p-values and confidence intervals from the harmonic mean chi-squared test
#'
#' @rdname hMeanChiSq
#' @param z Numeric vector of z-values. 
#' @param w Numeric vector of weights.
#' @param alternative Either "greater" (default), "less", "two.sided", or "none".
#' Specifies the alternative to be considered in the computation of the p-value.
#' @param bound If \code{FALSE} (default), p-values that cannot be computed are reported as \code{NaN}.
#' If \code{TRUE}, they are reported as "> bound".
#' @return \code{hMeanChiSq} returns the p-values from the harmonic mean chi-squared test
#' based on the study-specific z-values. 
#' @references Held, L. (2020). The harmonic mean chi-squared test to substantiate scientific findings.
#' \emph{Journal of the Royal Statistical Society: Series C (Applied Statistics)}, \bold{69}, 697-708.
#' \doi{10.1111/rssc.12410}
#' @author Leonhard Held
#' @examples
#' ## Example from Fisher (1999) as discussed in Held (2020)
#' pvalues <- c(0.0245, 0.1305, 0.00025, 0.2575, 0.128)
#' lower <- c(0.04, 0.21, 0.12, 0.07, 0.41)
#' upper <- c(1.14, 1.54, 0.60, 3.75, 1.27)
#' se <- ci2se(lower, upper, ratio=TRUE)
#' estimate <- ci2estimate(lower, upper, ratio=TRUE)
#'
#' ## hMeanChiSq() --------
#' hMeanChiSq(p2z(pvalues, alternative="less"), alternative="less")
#' hMeanChiSq(p2z(pvalues, alternative="less"), alternative="two.sided")
#' hMeanChiSq(p2z(pvalues, alternative="less"), alternative="none")
#'
#' hMeanChiSq(p2z(pvalues, alternative="less"),  w=1/se^2, alternative="less")
#' hMeanChiSq(p2z(pvalues, alternative="less"),  w=1/se^2, alternative="two.sided")
#' hMeanChiSq(p2z(pvalues, alternative="less"),  w=1/se^2, alternative="none")
#' @export
hMeanChiSq <- function(z, w = rep(1, length(z)),
                       alternative = c("greater", "less", "two.sided", "none"), bound=FALSE){
    stopifnot(is.numeric(z),
              length(z) > 0,
              is.finite(z),

              is.numeric(w),
              length(w) == length(z),
              is.finite(w),
              min(w) > 0,
              
              !is.null(alternative))
    alternative <- match.arg(alternative)

    stopifnot(is.logical(bound),
              length(bound) == 1,
              is.finite(bound))
              
    n <- length(z)
    zH2 <- sum(sqrt(w))^2 / sum(w / z^2)
    res <- pchisq(zH2, df = 1, lower.tail = FALSE)
    check_greater <- min(z) >= 0
    check_less <- max(z) <= 0
    break_p <- 1 / (2^n)
    if(alternative == "greater"){
        if(bound)
            res <- if(check_greater) res/(2^n) else paste(">", format(break_p, scientific = FALSE))
        else
            res <- if(check_greater) res/(2^n) else NaN
    }
    if(alternative == "less"){
        if(bound)
            res <- if(check_less) res/(2^n) else paste(">", format(break_p, scientific = FALSE))
        else
            res <- if(check_less) res/(2^n) else NaN
    }
    if(alternative == "two.sided"){
        if(bound)
            res <- if(check_greater || check_less) res/(2^(n-1)) else paste(">", format(2*break_p, scientific = FALSE))
        else
            res <- if(check_greater || check_less) res/(2^(n-1)) else NaN
    }
    ## no chnage to res for alternative == "none"
    return(res)
}

#' @rdname hMeanChiSq
#' @param thetahat Numeric vector of parameter estimates. 
#' @param se Numeric vector of standard errors.
#' @param mu The null hypothesis value. Defaults to 0.
#' @return \code{hMeanChiSqMu} returns the p-value from the harmonic mean chi-squared test
#' based on study-specific estimates and standard errors.
#' @examples
#' 
#'
#' ## hMeanChiSqMu() --------
#' hMeanChiSqMu(thetahat=estimate, se=se, alternative="two.sided")
#' hMeanChiSqMu(thetahat=estimate, se=se, w=1/se^2, alternative="two.sided")
#' hMeanChiSqMu(thetahat=estimate, se=se, alternative="two.sided", mu=-0.1)
#' @export
hMeanChiSqMu <- function(thetahat, se, w = rep(1, length(thetahat)), mu = 0,
                         alternative =c("greater", "less", "two.sided", "none"), bound=FALSE){
    stopifnot(is.numeric(thetahat),
              length(thetahat) > 0,
              is.finite(thetahat),

              is.numeric(se),
              length(se) == 1 || length(se) == length(thetahat),
              is.finite(se),
              min(se) > 0,

              is.numeric(w),
              length(w) == length(thetahat),
              is.finite(w),
              min(w) > 0,

              is.numeric(mu),
              length(mu) == 1 || length(mu) == length(thetahat),
              is.finite(mu),

              !is.null(alternative))
    alternative <- match.arg(alternative)
    stopifnot(is.logical(bound),
              length(bound) == 1,
              is.finite(bound))
    
    n <- length(thetahat)
    m <- length(mu)
    if(alternative != "none"){
        z <- (thetahat - mu) / se
        zH2 <- sum(sqrt(w))^2 / sum(w/z^2)
        res <- pchisq(zH2, df = 1, lower.tail = FALSE)
        check_greater <- min(z) >= 0
        check_less <- max(z) <= 0
        break_p <- 1/(2^n)
        if(alternative == "greater"){
            if(bound)
                res <- if(check_greater) res/(2^n) else paste(">", format(break_p, scientific = FALSE))
            else
                res <- if(check_greater || check_less) res/(2^n) else NaN
        }
        if(alternative == "less"){
            if(bound)
                res <- if(check_less) res/(2^n) else paste(">", format(break_p, scientific = FALSE))
            else
                res <- if(check_greater || check_less) res/(2^n) else NaN
        }
        if(alternative == "two.sided"){
            if(bound)
                res <- if(check_greater || check_less) res/(2^(n-1)) else paste(">", format(2*break_p, scientific = FALSE))
            else
                res <- if(check_greater || check_less) res/(2^(n-1)) else NaN
        }
    }
    if(alternative == "none"){
        zH2 <- numeric(m)
        sw <- sum(sqrt(w))^2
        for(i in 1:m){
            z <- (thetahat - mu[i]) / se
            zH2[i] <-  sw / sum(w/z^2)
        }
        res <- pchisq(zH2, df = 1, lower.tail = FALSE)
    }    
    return(res)
}

#' @rdname hMeanChiSq
#' @param level Numeric vector specifying the level of the confidence interval. Defaults to 0.95.
#' @return \code{hMeanChiSqCI} returns confidence interval(s) from inverting the harmonic mean chi-squared test
#' based on study-specific estimates and standard errors. If \code{alternative} is "none",
#' the return value may be a set of (non-overlapping) confidence intervals.
#' In that case, the output is a vector of length 2n, where n is the number of confidence intervals.
#' @examples
#'
#'
#' ## hMeanChiSqCI() --------
#' ## two-sided
#' CI1 <- hMeanChiSqCI(thetahat=estimate, se=se, w=1/se^2, alternative="two.sided")
#' CI2 <- hMeanChiSqCI(thetahat=estimate, se=se, w=1/se^2, alternative="two.sided", level=0.99875)
#' ## one-sided
#' CI1b <- hMeanChiSqCI(thetahat=estimate, se=se, w=1/se^2, alternative="less", level=0.975)
#' CI2b <- hMeanChiSqCI(thetahat=estimate, se=se, w=1/se^2, alternative="less", level=1-0.025^2)
#' ## confidence intervals on hazard ratio scale
#' print(round(exp(CI1),2))
#' print(round(exp(CI2),2))
#' print(round(exp(CI1b),2))
#' print(round(exp(CI2b),2))
#' @export
#' @importFrom rootSolve uniroot.all
#' @import stats
hMeanChiSqCI <- function(thetahat, se, w = rep(1, length(thetahat)),
                         alternative = c("two.sided", "greater", "less", "none"),
                         level = 0.95){
    stopifnot(is.numeric(thetahat),
              length(thetahat) > 0,
              is.finite(thetahat),
              
              is.numeric(se),
              length(se) == 1 || length(se) == length(thetahat),
              is.finite(se),
              min(se) > 0,
              
              is.numeric(w),
              length(w) == length(thetahat),
              is.finite(w),
              min(w) > 0,

              !is.null(alternative))
    alternative <- match.arg(alternative)
    
    stopifnot(is.numeric(level),
              length(level) == 1,
              is.finite(level),
              0 < level, level <1)

    ## target function to compute the limits of the CI
    target <- function(limit, thetahat, se, w, alternative, alpha){
        hMeanChiSqMu(thetahat=thetahat, se=se, w=w, mu=limit, alternative=alternative,
                     bound=FALSE) - alpha
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
        ## FG: return full uniroot.all() output as CI?
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
