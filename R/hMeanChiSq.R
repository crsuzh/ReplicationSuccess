#' harmonic mean chi-squared test
#'
#' p-values and confidence intervals from the harmonic mean chi-squared test.
#' @rdname hMeanChiSq
#' @param z Numeric vector of z-values. 
#' @param w Numeric vector of weights.
#' @param alternative Either "greater" (default), "less", "two.sided", or "none".
#' Specifies the alternative to be considered in the computation of the p-value.
#' @param bound If \code{FALSE} (default), p-values that cannot be computed are reported as \code{NaN}.
#' If \code{TRUE}, they are reported as "> bound".
#' @return \code{hMeanChiSq}: returns the p-values from the harmonic mean chi-squared test
#' based on the study-specific z-values. 
#' @references Held, L. (2020). The harmonic mean chi-squared test to substantiate scientific findings.
#' \emph{Journal of the Royal Statistical Society: Series C (Applied Statistics)}, \bold{69}, 697-708.
#' \doi{10.1111/rssc.12410}
#' @author Leonhard Held, Florian Gerber
#' @examples
#' ## Example from Fisher (1999) as discussed in Held (2020)
#' pvalues <- c(0.0245, 0.1305, 0.00025, 0.2575, 0.128)
#' lower <- c(0.04, 0.21, 0.12, 0.07, 0.41)
#' upper <- c(1.14, 1.54, 0.60, 3.75, 1.27)
#' se <- ci2se(lower = lower, upper = upper, ratio = TRUE)
#' thetahat <- ci2estimate(lower = lower, upper = upper, ratio = TRUE)
#'
#' ## hMeanChiSq() --------
#' hMeanChiSq(z = p2z(p = pvalues, alternative = "less"),
#'            alternative = "less")
#' hMeanChiSq(z = p2z(p = pvalues, alternative = "less"),
#'            alternative = "two.sided")
#' hMeanChiSq(z = p2z(p = pvalues, alternative = "less"),
#'            alternative = "none")
#'
#' hMeanChiSq(z = p2z(p = pvalues, alternative = "less"),
#'            w = 1 / se^2, alternative = "less")
#' hMeanChiSq(z = p2z(p = pvalues, alternative = "less"),
#'            w = 1 / se^2, alternative = "two.sided")
#' hMeanChiSq(z = p2z(p = pvalues, alternative = "less"),
#'            w = 1 / se^2, alternative = "none")
#' @export
hMeanChiSq <- function(z, w = rep(1, length(z)),
                       alternative = c("greater", "less", "two.sided", "none"),
                       bound = FALSE){
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
            res <- if(check_greater) res / (2^n) else paste(">", format(break_p, scientific = FALSE))
        else
            res <- if(check_greater) res / (2^n) else NaN
    }
    if(alternative == "less"){
        if(bound)
            res <- if(check_less) res / (2^n) else paste(">", format(break_p, scientific = FALSE))
        else
            res <- if(check_less) res / (2^n) else NaN
    }
    if(alternative == "two.sided"){
        if(bound)
            res <- if(check_greater || check_less) res / (2^(n - 1)) else
                        paste(">", format(2*break_p, scientific = FALSE))
        else
            res <- if(check_greater || check_less) res / (2^(n - 1)) else NaN
    }
    ## no chnage to res for alternative == "none"
    return(res)
}

#' @rdname hMeanChiSq
#' @param thetahat Numeric vector of parameter estimates. 
#' @param se Numeric vector of standard errors.
#' @param mu The null hypothesis value. Defaults to 0.
#' @return \code{hMeanChiSqMu}: returns the p-value from the harmonic mean chi-squared test
#' based on study-specific estimates and standard errors.
#' @examples
#' 
#'
#' ## hMeanChiSqMu() --------
#' hMeanChiSqMu(thetahat = thetahat, se = se, alternative = "two.sided")
#' hMeanChiSqMu(thetahat = thetahat, se = se, w = 1 / se^2,
#'              alternative = "two.sided")
#' hMeanChiSqMu(thetahat = thetahat, se = se, alternative = "two.sided",
#'              mu = -0.1)
#' @export
hMeanChiSqMu <- function(thetahat, se, w = rep(1, length(thetahat)), mu = 0,
                         alternative = c("greater", "less", "two.sided", "none"),
                         bound = FALSE){
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
              length(mu) > 0,
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
        zH2 <- sum(sqrt(w))^2 / sum(w / z^2)
        res <- pchisq(zH2, df = 1, lower.tail = FALSE)
        check_greater <- min(z) >= 0
        check_less <- max(z) <= 0
        break_p <- 1 / (2^n)
        if(alternative == "greater"){
            if(bound)
                res <- if(check_greater) res / (2^n) else paste(">", format(break_p, scientific = FALSE))
            else
                res <- if(check_greater || check_less) res/(2^n) else NaN
        }
        if(alternative == "less"){
            if(bound)
                res <- if(check_less) res / (2^n) else paste(">", format(break_p, scientific = FALSE))
            else
                res <- if(check_greater || check_less) res / (2^n) else NaN
        }
        if(alternative == "two.sided"){
            if(bound)
                res <- if(check_greater || check_less) res / (2^(n - 1)) else
                             paste(">", format(2*break_p, scientific = FALSE))
            else
                res <- if(check_greater || check_less) res / (2^(n - 1)) else NaN
        }
    }
    if(alternative == "none"){
        zH2 <- numeric(m)
        sw <- sum(sqrt(w))^2
        for(i in 1:m){
            z <- (thetahat - mu[i]) / se
            zH2[i] <-  sw / sum(w / z^2)
        }
        res <- pchisq(q = zH2, df = 1, lower.tail = FALSE)
    }    
    return(res)
}

#' @rdname hMeanChiSq
#' @param level Numeric vector specifying the level of the confidence interval. Defaults to 0.95.
#' @param wGamma Numeric vector of length \code{unique(thetahat) - 1} specifying weights used to
#' summarize the gamma values, i.e.,
#' the local minima of the p-value function between the thetahats. Defaults is a vector of 1s.
#' @return \code{hMeanChiSqCI}: returns a list containing confidence interval(s)
#' obtained by inverting the harmonic mean chi-squared test based on study-specific
#' estimates and standard errors. The list contains:
#' \item{CI}{Confidence interval(s).}\cr\cr
#' If the \code{alternative} is "none", the list also contains:
#' \item{gamma}{Local minima of the p-value function between the thetahats.}
#' \item{gammaMean}{Mean of all gammas weighted by \code{wGamma}.}
#' \item{gammaHMean}{Harmonic mean of all gammas weighted by \code{wGamma}.}
#' @examples
#'
#' ## hMeanChiSqCI() --------
#' ## two-sided
#' CI1 <- hMeanChiSqCI(thetahat = thetahat, se = se, w = 1 / se^2,
#'                     alternative = "two.sided")
#' CI2 <- hMeanChiSqCI(thetahat = thetahat, se = se, w = 1 / se^2,
#'                     alternative = "two.sided", level = 0.99875)
#' ## one-sided
#' CI1b <- hMeanChiSqCI(thetahat = thetahat, se = se, w = 1 / se^2,
#'                      alternative = "less", level = 0.975)
#' CI2b <- hMeanChiSqCI(thetahat = thetahat, se = se, w = 1 / se^2,
#'                      alternative = "less", level = 1 - 0.025^2)
#'
#' ## confidence intervals on hazard ratio scale 
#' print(exp(CI1$CI), digits = 2)
#' print(exp(CI2$CI), digits = 2)
#' print(exp(CI1b$CI), digits = 2)
#' print(exp(CI2b$CI), digits = 2)
#'
#'
#' ## example with confidence region consisting of disjunct intervals
#' thetahat2 <- c(-3.7, 2.1, 2.5)
#' se2 <- c(1.5, 2.2, 3.1)
#' level <- 0.95; alpha <- 1 - level
#' muSeq <- seq(-7, 6, length.out = 1000)
#' pValueSeq <- hMeanChiSqMu(thetahat = thetahat2, se = se2,
#'                           alternative = "none", mu = muSeq)
#' (hm <- hMeanChiSqCI(thetahat = thetahat2, se = se2, alternative = "none"))
#'
#' plot(x = muSeq, y = pValueSeq, type = "l", panel.first = grid(lty = 1),
#'      xlab = expression(mu), ylab = "p-value")
#' abline(v = thetahat2, h = alpha, lty = 2)
#' arrows(x0 = hm$CI[, 1], x1 = hm$CI[, 2], y0 = alpha,
#'        y1 = alpha, col = "darkgreen", lwd = 3, angle = 90, code = 3)
#' points(hm$gamma, col = "red", pch = 19, cex = 2)
#'
#' @export
#' @import stats
hMeanChiSqCI <- function(thetahat, se, w = rep(1, length(thetahat)),
                         alternative = c("two.sided", "greater", "less", "none"),
                         level = 0.95, wGamma = rep(1, length(unique(thetahat)) - 1)){
    stopifnot(is.numeric(thetahat),
              length(thetahat) > 0,
              is.finite(thetahat))
    stopifnot(is.numeric(se),
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
              0 < level, level < 1,

              is.numeric(wGamma),
              length(wGamma) == length(unique(thetahat)) -1,
              is.finite(w),
              min(w) > 0)

    ## target function to compute the limits of the CI
    target <- function(limit){
        hMeanChiSqMu(thetahat = thetahat, se = se, w = w, mu = limit,
                     alternative = alternative, bound = FALSE) - alpha
    }

    ## sort 'thetahat', 'se', 'w'
    indOrd <- order(thetahat)
    thetahat <- thetahat[indOrd]; se <- se[indOrd]; w <- w[indOrd]

    ## minima are only search between distinct thetahat elements
    thetahatUnique <- unique(thetahat)
    nThetahatUnique <- length(thetahatUnique)
    
    mini <- which.min(thetahat)
    maxi <- which.max(thetahat)
    mint <- thetahat[mini]
    maxt <- thetahat[maxi]
    minse <- se[mini]
    maxse <- se[maxi]
    alpha <- 1 - level
    z1 <- max(-qnorm(alpha), 1)
    eps <- 1e-6
    factor <- 5
    if(alternative == "none"){
        
        ## ----------------------------
        ## find lower bound such that: lower < thetahat[1] AND target(lower) < 0 
        lower <- mint - z1 * minse
        while(target(lower) > 0)
            lower <- lower - minse
        
        ## find root between 'lower' and 'thetahat[1]'
        CIlower <- uniroot(f = target, lower = lower, upper = thetahat[1])$root
        
        ## -------------------------
        ## check between thetahats whether 'target' goes below 'alpha'
        ## if so, search CI limits
        CImiddle <- matrix(NA, nrow = 2, ncol = nThetahatUnique - 1)
        gam <- matrix(NA, nrow = nThetahatUnique - 1, ncol = 2)
        colnames(gam) <- c("minimum", "pvalue_fun/gamma")
        for(i in 1:(nThetahatUnique - 1)){
            opt <- optimize(f = target, lower = thetahatUnique[i],
                            upper = thetahatUnique[i + 1])
                gam[i,] <- c(opt$minimum, opt$objective + alpha)
                if(opt$objective <= 0){
                    CImiddle[1, i] <- uniroot(f = target, lower = thetahatUnique[i],
                                              upper = opt$minimum)$root
                    CImiddle[2, i] <- uniroot(f = target, lower = opt$minimum,
                                              upper = thetahatUnique[i + 1])$root
                }
        }
        CImiddle <- CImiddle[!is.na(CImiddle)]
        
        ## -------------------------
        ## find upper bound such that:
        ## upper > thetahat[length(thetahat)] AND target(upper) < 0 
        upper <- maxt + maxse
        while(target(upper) > 0)
            upper <- upper + z1 * maxse
        
        ## find root between 'lower' and 'thetahat[1]'
        CIupper <- uniroot(f = target, lower = thetahat[length(thetahat)],
                           upper = upper)$root
        CI <- matrix(c(CIlower, CImiddle, CIupper), ncol = 2, byrow = TRUE)
        colnames(CI) <- c("lower", "upper")
        return(list(CI = CI,
                    gamma = gam,
                    gammaMean = weighted.mean(x = gam[,"pvalue_fun/gamma"], w = wGamma),
                    gammaHMean = sum(wGamma) / sum(wGamma / gam[,"pvalue_fun/gamma"])))
    }
    if(alternative == "two.sided"){
        lower <- uniroot(f = target,
                         lower = mint - factor * z1 * minse,
                         upper = mint - eps * minse)$root
        upper <- uniroot(f = target, lower = maxt + eps * maxse,
                         upper = maxt + factor * z1 * maxse)$root
        return(list(CI = cbind(lower, upper)))
    }
    if(alternative == "greater"){
        lower <- uniroot(f = target,
                         lower = mint - factor * z1 * minse,
                         upper = mint - eps * minse)$root
        upper <- Inf
        return(list(CI = cbind(lower, upper)))
    }
    if(alternative == "less"){
        lower <- -Inf
        upper <- uniroot(f = target,
                         lower = maxt + eps * maxse,
                         upper = maxt + factor * z1 * maxse)$root
        return(list(CI = cbind(lower, upper)))
    }
    stop("function not get here.")
}

#' Find multiple roots in interval
#'
#' Searches the interval from lower to upper for several roots
#' (i.e., zero's) of a univariate function \code{f}. 
#' @param f the function for which the root is sought.
#' \code{f} should be vectorized in the first argument.
#' @param interval A vector containing the end-points of the interval to be
#' searched for the root.
#' @param n Number of subintervals on which \code{link[stats]{uniroot}} is called.
#' Default is 1000.
#' @param lower The lower end point of the interval to be searched.
#' @param upper The upper end point of the interval to be searched.
#' @param tol See help of \code{link[stats]{uniroot}}.
#' @param maxiter See help of \code{link[stats]{uniroot}}.
#' @param trace See help of \code{link[stats]{uniroot}}.
#' @param ... Additional named or unnamed arguments to be passed to \code{f}.
#' @return A numeric vector of the roots found in the interval.
#' @references This function is inspired by rootSolve::uniroot.all(),
#' package version 1.8.2.2.
#' @import stats
#' @seealso \code{\link[base]{Vectorize}} 
#' @examples
#' f <- function (x) cos(2*x)^3
#' (roots <- unirootAll(f = f, interval = c(0, 10)))
#' f(roots) 
#' @export 
unirootAll <- function(f, interval, lower = min(interval), upper = max(interval), 
                       n = 1000,
                       tol = .Machine$double.eps^0.2,
                       maxiter = 1000, trace = 0,
                       ...) 
{
    stopifnot(is.function(f), length(formals(f)) >= 1,
              is.numeric(lower), length(lower) == 1, is.finite(lower),
              is.numeric(upper), length(upper) == 1, is.finite(upper),
              lower < upper,
              is.numeric(n), length(n) == 1, is.finite(n), n >= 2)
    n <- round(n)
    ## 'tol', 'maxiter', 'trace' are checked in uniroot()
    xseq <- seq(lower, upper, length.out = n + 1)
    mod <- f(xseq, ...)
    index <- which(mod[1:n] * mod[2:(n + 1)] < 0)
    rootsOffGrid <- numeric(length = length(index))
    for (i in seq_along(index))
        rootsOffGrid[i] <- uniroot(f = f, lower = xseq[index[i]],
                                   upper = xseq[index[i] + 1], maxiter = maxiter,
                                   tol = tol, trace = trace, ...)$root
    rootsGrid <- xseq[which(mod == 0)]
    if(length(rootsGrid) > 0)
        return(sort(c(rootsGrid, rootsOffGrid)))
    rootsOffGrid
}
