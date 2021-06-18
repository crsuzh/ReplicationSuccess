#' Transforms confidence intervals into standard errors.
#' 
#' @param lower Numeric vector of lower confidence interval bounds.
#' @param upper Numeric vector of upper confidence interval bounds.
#' @param conf.level The confidence level of the confidence intervals. Default is 0.95.
#' @param ratio Indicates whether the confidence interval is for a ratio, e.g. an
#' odds ratio, relative risk or hazard ratio. If \code{TRUE}, the standard error
#' of the log ratio is computed. Defaults to \code{FALSE}.
#' @return A numeric vector of standard errors. 
#' @seealso \code{\link{ci2p}}, \code{\link{ci2z}}, \code{\link{ci2estimate}}, \code{\link{p2z}}, \code{\link{z2p}}
#' @examples
#' ci2se(lower = 1, upper = 3)
#' ci2se(lower = 1, upper = 3, ratio = TRUE)
#' ci2se(lower = 1, upper = 3, conf.level = 0.9)
#' @export
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

#' Transforms confidence intervals into parameter estimates.
#' 
#' @param lower Numeric vector of lower confidence interval bounds.
#' @param upper Numeric vector of upper confidence interval bounds.
#' @param ratio Indicates whether the confidence interval is for a ratio, e.g. an
#' odds ratio, relative risk or hazard ratio. Defaults to \code{FALSE}.
#' @param antilog Indicates whether the estimate is reported on the ratio scale.
#' Only applies if \code{ratio = TRUE}. Defaults to \code{FALSE}.
#' @return A numeric vector of parameter estimates.
#' @seealso \code{\link{ci2p}}, \code{\link{ci2z}}, \code{\link{ci2se}}, \code{\link{p2z}}, \code{\link{z2p}}
#' @examples
#' ci2estimate(lower = 1, upper = 3)
#' ci2estimate(lower = 1, upper = 3, ratio = TRUE)
#' ci2estimate(lower = 1, upper = 3, ratio = TRUE, antilog = TRUE)
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


#' Transforms confidence intervals into z-values.
#' 
#' @param lower Numeric vector of lower confidence interval bounds.
#' @param upper Numeric vector of upper confidence interval bounds.
#' @param conf.level The confidence level of the confidence intervals. Default is 0.95.
#' @param ratio Indicates whether the confidence interval is for a ratio, e.g. an
#' odds ratio, relative risk or hazard ratio. If \code{TRUE}, the
#' z-value of the log ratio is computed. Defaults to \code{FALSE}.
#' @return A numeric vector of z-values.
#' @seealso \code{\link{ci2p}}, \code{\link{ci2estimate}}, \code{\link{ci2se}}, \code{\link{p2z}}, \code{\link{z2p}}
#' @examples
#' ci2z(lower = 1, upper = 3)
#' ci2z(lower = 1, upper = 3, ratio = TRUE)
#' ci2z(lower = 1, upper = 3, conf.level = 0.9)
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

#' Transforms confidence intervals into p-values.
#' 
#' @param lower Numeric vector of lower confidence interval bounds.
#' @param upper Numeric vector of upper confidence interval bounds.
#' @param conf.level The confidence level of the confidence intervals. Default is 0.95.
#' @param ratio Indicates whether the confidence interval is for a ratio, e.g. an
#' odds ratio, relative risk or hazard ratio. Defaults to \code{FALSE}.
#' @param alternative Specifies whether the p-values are "two.sided" (default) or "one.sided".
#' @return A numeric vector of p-values.
#' @seealso \code{\link{ci2p}}, \code{\link{ci2estimate}}, \code{\link{ci2se}}, \code{\link{p2z}}, \code{\link{z2p}}
#' @examples
#' ci2p(lower = 1, upper = 3)
#' ci2p(lower = 1, upper = 3, alternative = "one.sided")
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


#' Transforms z-values into p-values.
#' 
#' @param z Numeric a vector of z-values.
#' @param alternative Specifies direction of the alternative of p-value.
#' Either "one.sided" (default), "two.sided", "less", or "greater".  
#' @return A Numeric vector of p-values.
#' @seealso \code{\link{ci2p}}, \code{\link{ci2estimate}}, \code{\link{ci2se}}, \code{\link{p2z}}, \code{\link{z2p}}
#' @examples
#' z2p(z = c(1, 2, 5))
#' z2p(z = c(1, 2, 5), alternative = "less")
#' z2p(z = c(1, 2, 5), alternative = "greater")
#'
#' z <- seq(-3, 3, by = 0.01)
#' plot(z, z2p(z), type = "l", xlab = "z", ylab = "p", ylim = c(0, 1))
#' lines(z, z2p(z, alternative = "greater"), lty = 2)
#' legend("topright", c("two-sided", "greater"), lty = c(1, 2), bty = "n")
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

