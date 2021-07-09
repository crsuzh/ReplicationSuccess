#' @export
.T1EpSceptical_ <- function(level, c,
                            alternative = c("one.sided", "two.sided", "greater", "less"),
                            type = c("golden", "nominal", "liberal", "controlled")) {  
    
    stopifnot(is.numeric(level),
              length(level) == 1,
              is.finite(level),
              0 < level, level < 1,

              is.numeric(c),
              length(c) == 1,
              is.finite(c),
              0 <= c,
                            
              !is.null(alternative))
    alternative <- match.arg(alternative)
    
    stopifnot(!is.null(type))
    type <- match.arg(type)
        
    ## compute normal quantile corresponding to level and type
    alphas <- levelSceptical(level = level, 
                             alternative = alternative, 
                             type = type)
    zas <- p2z(alphas, alternative = alternative)
    
    if (alternative == "two.sided") {
        ## if c = 1 compute analytically
        if (c == 1) {
            t1err <- 2*(1 - stats::pnorm(q = 2*zas))
            return(t1err)
        } else {  ## if c != 1 use numerical integration
            
            ## define function to integrate over zo from zas to Infty
            intFun <- function(zo) {
                K <- zo^2/zas^2
                ## compute minimal zr to achieve replication success given zo and level
                zrmin <- zas*sqrt(1 + c/(K - 1))
                ## return integrand: P(|zr| >= zrmin)*dnorm(zo)
                2*(1 - stats::pnorm(q = zrmin))*stats::dnorm(x = zo)
            } 
        }
    }
    if (alternative == "one.sided") {
        ## if c = 1 compute analytically
        if (c == 1) {
            t1err <- 1 - stats::pnorm(q = 2*zas)
            return(t1err)
        } else { ## if c != 1 use numerical integration
            
                                        # define function to integrate over zo from zas to Infty
            intFun <- function(zo) {
                K <- zo^2/zas^2
                ## compute minimal zr to achieve replication success given zo and level
                zrmin <- zas*sqrt(1 + c/(K - 1))
                ## compute integrand: P(zr >= zrmin)*dnorm(zo)
                (1 - stats::pnorm(q = zrmin))*stats::dnorm(x = zo)
            }
        }
    }
    if (alternative == "greater") {
        ## if c = 1 compute analytically
        if (c == 1) {
            t1err <- (1 - stats::pnorm(q = 2*zas))/2
            return(t1err)
        } else { ## if c != 1 use numerical integration
            
            ## define function to integrate over zo from zas to Infty
            intFun <- function(zo) {
                K <- zo^2/zas^2
                ## compute minimal zr to achieve replication success given zo and level
                zrmin <- zas*sqrt(1 + c/(K - 1))
                ## compute integrand: P(zr >= zrmin)*dnorm(zo)
                (1 - stats::pnorm(q = zrmin))*stats::dnorm(x = zo)
            }
        }
    }
    if (alternative == "less") {
        ## if c = 1 compute analytically
        if (c == 1) {
            t1err <- stats::pnorm(q = 2*zas)/2
            return(t1err)
        } else { ## if c != 1 use numerical integration
            
            ## define function to integrate over zo from zas to Infty
            intFun <- function(zo) {
                K <- zo^2/zas^2
                ## compute maximal zr to achieve replication success given zo and level
                zrmax <- zas*sqrt(1 + c/(K - 1))
                ## compute integrand: P(zr <= zrmax)*dnorm(zo)
                stats::pnorm(q = zrmax)*stats::dnorm(x = zo)
            }
        }
    }
    if (alternative %in% c("one.sided", "two.sided")) {
        ## the integral is symmetric around zero for "one.sided" and "two.sided" 
        ## so we can multiply the integral from zas to Infty by 2
        t1err <- 2*stats::integrate(f = intFun, lower = zas, upper = Inf)$value
        return(t1err)
    }
    if (alternative == "greater") {
        t1err <- stats::integrate(f = intFun, lower = zas, upper = Inf)$value
        return(t1err)
    }
    if (alternative == "less") {
        t1err <- stats::integrate(f = intFun, lower = -Inf, upper = zas)$value
        return(t1err)
    }
    return(t1err)
}


#' Compute type-I error rate of the sceptical p-value
#'
#' The type-I error rate of the sceptical p-value is computed for a
#' specified level of replication success, the relative variance,
#' and the alternative hypothesis.
#' @param level Numeric vector of levels of replication success.
#' @param c Numeric vector of variance ratios of the original and replication
#' effect estimates. This is usually the ratio of the sample
#' size of the replication study to the sample size of the
#' original study.
#' @param alternative Either "one.sided" (one.sided), "two.sided", "greater", or "less".
#' If "one.sided", the type-I error rate is computed based on a one-sided assessment of
#' replication success in the direction of the original effect estimate.
#' If "two.sided", the type-I error rate is computed based
#' on a two-sided assessment of replication success regardless of the direction
#' of the original and replication effect estimate.
#' If "greater" or "less",  the type-I error rate is
#' computed based on a one-sided assessment of replication success
#' in the pre-specified direction of the original and replication effect estimate.
#' @param type Type of recalibration. Can be either "golden" (default), "nominal" (no recalibration),
#' "liberal", or "controlled". "golden" ensures that
#' for an original study just significant at the specified \code{level},
#' replication success is only possible if the replication effect estimate is at
#' least as large as the original one. See \code{\link{levelSceptical}} for details
#' about recalibration types.
#' @return The type-I error rate.
#' @details \code{T1EpSceptical} is the vectorized version of \code{.T1EpSceptical_}.
#' \code{\link[base]{Vectorize}} is used to vectorize the function.
#' @references
#' Held, L. (2020). The harmonic mean chi-squared test to substantiate
#' scientific findings. \emph{Journal of the Royal Statistical Society: Series C
#' (Applied Statistics)}, \bold{69}, 697-708. \doi{10.1111/rssc.12410}
#'
#' Held, L., Micheloud, C., Pawel, S. (2021). The assessment of replication success
#' based on relative effect size. \url{https://arxiv.org/abs/2009.07782}
#' @author Samuel Pawel, Leonhard Held
#' @seealso \code{\link{pSceptical}}, \code{\link{levelSceptical}}, \code{\link{PPpSceptical}}
#' @examples
#' ## compare type-I error rate for different levels of replication success
#' levels <- c("nominal" = levelSceptical(level = 0.025, type = "nominal"),
#'             "liberal" = levelSceptical(level = 0.025, type = "liberal"),
#'             "controlled" = levelSceptical(level = 0.025, type = "controlled"),
#'             "golden" = levelSceptical(level = 0.025, type = "golden"))
#' c <- seq(0.2, 5, by = 0.05)
#' t1 <- sapply(X = levels, FUN = function(l) {
#'   T1EpSceptical(level = l, c = c, alternative = "one.sided", type = "nominal")
#' })
#' matplot(x = c, y = t1*100, type = "l", lty = 1, lwd = 2, las = 1, log = "x",
#'         xlab = bquote(italic(c)), ylab = "Type-I error (%)", xlim = c(0.2, 5))
#' legend("topright", legend = names(levels), lty = 1, lwd = 2, col = seq_along(levels))
#' 
#' ## check that one.sided controlled level controls type-I error rate for c = 1 
#' ## at alpha = 0.05*0.025 = 0.00125
#' T1EpSceptical(level = levelSceptical(level = 0.025, alternative = "one.sided", 
#'                                      type = "controlled"), 
#'               c = 1, alternative = "one.sided",  type = "nominal")
#' @export
T1EpSceptical <- Vectorize(.T1EpSceptical_)
