#' Convert between estimates, z-values, p-values, and confidence intervals
#' 
#' @rdname conversionHelpers
#' @param lower Numeric vector of lower confidence interval bounds.
#' @param upper Numeric vector of upper confidence interval bounds.
#' @param conf.level The confidence level of the confidence intervals. Default is 0.95.
#' @param ratio Indicates whether the confidence interval is for a ratio, e.g. an
#' odds ratio, relative risk or hazard ratio. If \code{TRUE}, the standard error
#' of the log ratio is computed. Defaults to \code{FALSE}.
#' @return \code{ci2se} returns a numeric vector of standard errors. 
#' @examples
#' ci2se(lower = 1, upper = 3)
#' ci2se(lower = 1, upper = 3, ratio = TRUE)
#' ci2se(lower = 1, upper = 3, conf.level = 0.9)
#' 
#' @export
ci2se <- function(lower, 
                  upper, 
                  conf.level = 0.95, 
                  ratio = FALSE){

    stopifnot(is.numeric(lower),
              is.finite(lower),
              length(lower)>0,
              
              is.numeric(upper),
              is.finite(upper),
              length(upper)>0,

              length(upper)==length(lower),
              lower <= upper,

              is.numeric(conf.level),
              is.finite(conf.level),
              length(conf.level)==1,
              0 < conf.level, conf.level < 1,

              is.lgocial(ratio),
              is.finite(ratio),
              length(ratio)==1)
    
    level <- 1 - conf.level
    q <- qnorm(p = 1 - level/2, lower.tail = TRUE)
    
    if(ratio){
        stopifnot(lower>0, upper>0)
        lower <- log(lower)
        upper <- log(upper)
    }
    se <- (upper - lower)/(2*q)
    return(se)
}

#' @rdname conversionHelpers
#' @param antilog Indicates whether the estimate is reported on the ratio scale.
#' Only applies if \code{ratio = TRUE}. Defaults to \code{FALSE}.
#' @return \code{ci2estimate} returns a numeric vector of parameter estimates.
#' @examples
#' ci2estimate(lower = 1, upper = 3)
#' ci2estimate(lower = 1, upper = 3, ratio = TRUE)
#' ci2estimate(lower = 1, upper = 3, ratio = TRUE, antilog = TRUE)
#'
#' @export
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

#' @rdname conversionHelpers
#' @return \code{ci2z} returns a numeric vector of z-values.
#' @examples
#' ci2z(lower = 1, upper = 3)
#' ci2z(lower = 1, upper = 3, ratio = TRUE)
#' ci2z(lower = 1, upper = 3, conf.level = 0.9)
#' 
#' @export
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

#' @rdname conversionHelpers
#' @param alternative Direction of the alternative of the p-value. 
#' Either "one.sided" (default), "two.sided", "less", or "greater".
#' If "one.sided" or "two.sided" is specified, the z-value is assumed to be positive.
#' @return \code{ci2p} returns a numeric vector of p-values.
#' @examples
#' ci2p(lower = 1, upper = 3)
#' ci2p(lower = 1, upper = 3, alternative = "one.sided")
#' 
#' @export
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

#' @rdname conversionHelpers
#' @param z Numeric vector of z-values.
#' @return \code{z2p} returns a numeric vector of p-values.
#' @examples
#' z2p(z = c(1, 2, 5))
#' z2p(z = c(1, 2, 5), alternative = "less")
#' z2p(z = c(1, 2, 5), alternative = "greater")
#' z <- seq(-3, 3, by = 0.01)
#' plot(z, z2p(z), type = "l", xlab = "z", ylab = "p", ylim = c(0, 1))
#' lines(z, z2p(z, alternative = "greater"), lty = 2)
#' legend("topright", c("two-sided", "greater"), lty = c(1, 2), bty = "n")
#'
#' @export
z2p <- function(z, 
                alternative = "two.sided"){
    
    # vectorize function in all arguments
    pV <- mapply(FUN = function(z, alternative) {
        if (!is.numeric(z))
            stop("z must be numeric")
        if (!(alternative %in% c("less", "greater", "two.sided", "one.sided")))
            stop('alternative must be either "less", "greater", "two.sided", or "one.sided"')
        
        if (alternative == "two.sided")
            p <- 2*pnorm(abs(z), lower.tail = FALSE)
        
        if (alternative == "less")
            p <- pnorm(q = z, lower.tail = TRUE)
        
        if (alternative == "greater" | alternative == "one.sided")
            p <- pnorm(q = z, lower.tail = FALSE)
        
        return(p)    
    }, z, alternative)
    return(pV)
}

#' @rdname conversionHelpers
#' @param p Numeric vector of p-values.
#' @return \code{p2z} returns a numeric vector of z-values.
#' @examples
#' p2z(p = c(0.005, 0.01, 0.05))
#' p2z(p = c(0.005, 0.01, 0.05), alternative = "greater")
#' p2z(p = c(0.005, 0.01, 0.05), alternative = "less")
#' p <- seq(0.001, 0.05, 0.0001)
#' plot(p, p2z(p), type = "l", ylim = c(0, 3.5), ylab = "z")
#' lines(p, p2z(p, alternative = "greater"), lty = 2)
#' legend("bottomleft", c("two-sided", "greater"), lty = c(1, 2), bty = "n")
#' @export
p2z <- function(p, 
                alternative = "two.sided"){
  
  # vectorize function in both arguments
  zVec <- mapply(FUN = function(p, alternative) {
    
    # sanity checks
    if (!is.numeric(p) || (p <= 0 || p > 1))
      stop("p must be numeric and in (0,1]!")
    if (!(alternative %in% c("less", "greater", "two.sided", "one.sided")))
      stop('alternative must be either "less", "greater", "two.sided", or "one.sided"')
    
    if (alternative == "two.sided")
      z <- qnorm(p = p/2, lower.tail = FALSE)
    if (alternative == "less")
      z <- qnorm(p = p, lower.tail = TRUE)
    if (alternative == "greater" || alternative == "one.sided")
      z <- qnorm(p = p, lower.tail = FALSE)
    
    return(z)
    
  }, p, alternative)
  
  return(zVec)
}
