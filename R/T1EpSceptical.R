#' @export
.T1EpSceptical_ <- function(level, c,
                            alternative = c("one.sided", "two.sided", "greater", "less"),
                            type = c("golden", "nominal", "controlled")) {  

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
    if (alternative == "two.sided") {
      alphas <- levelSceptical(level = level,
                               alternative = "two.sided",
                               type = type,
                               c = c)
      zas <- p2z(alphas, alternative = "two.sided")

    }

    if (alternative != "two.sided") {
      alphas <- levelSceptical(level = level,
                               alternative = "one.sided",
                               type = type,
                               c = c)
      zas <- p2z(alphas, alternative = alternative)

    }

    # quick fix for alternative problems

    if(alternative == "two.sided") {
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


#' Compute overall type-I error rate of the sceptical p-value
#'
#' The overall type-I error rate of the sceptical p-value is computed for a
#' specified level, the relative variance,
#' and the alternative hypothesis.
#' @param level Threshold for the calibrated sceptical p-value (for all recalibration types).
#'  Default is 0.025.
#' @param c Numeric vector of variance ratios of the original and replication
#' effect estimates. This is usually the ratio of the sample
#' size of the replication study to the sample size of the
#' original study.
#' @param alternative Specifies if \code{level}
#' is "two.sided" or one.sided ("one.sided", "greater", or "less").
#' If "one.sided", the type-I error rate is computed based on a one-sided assessment
#' of replication success in the direction of the original effect estimate.
#' If "two.sided", the type-I error rate is computed based
#' on a two-sided assessment of replication success regardless of the direction
#' of the original and replication effect estimate.
#' If "greater" or "less",  the type-I error rate is
#' computed based on a one-sided assessment of replication success
#' in the pre-specified direction of the original and replication effect estimate.
#' @param type Type of recalibration. Recalibration type can be either "golden"
#' (default), "nominal" (no recalibration), or "controlled".
#' @return The overall type-I error rate.
#' @details \code{T1EpSceptical} is the vectorized version of \code{.T1EpSceptical_}.
#' \code{\link[base]{Vectorize}} is used to vectorize the function.
#' @references
#' Held, L. (2020). The harmonic mean chi-squared test to substantiate
#' scientific findings. \emph{Journal of the Royal Statistical Society: Series C
#' (Applied Statistics)}, \bold{69}, 697-708. \doi{10.1111/rssc.12410}
#'
#' Held, L., Micheloud, C., Pawel, S. (2022). The assessment of replication
#'     success based on relative effect size. The Annals of Applied Statistics.
#'     16:706-720. \doi{10.1214/21-AOAS1502}
#'
#' Micheloud, C., Balabdaoui, F., Held, L. (2023).
#' Beyond the two-trials rule: Type-I error control and sample size planning
#' with the sceptical p-value. \url{https://arxiv.org/abs/2207.00464}
#'
#' @author Samuel Pawel, Leonhard Held
#' @seealso \code{\link{pSceptical}}, \code{\link{levelSceptical}}, \code{\link{PPpSceptical}}
#' @examples
#' ## compare type-I error rate for different recalibration types
#' types <- c("nominal", "golden", "controlled")
#' c <- seq(0.2, 5, by = 0.05)
#' t1 <- sapply(X = types, FUN = function(t) {
#'   T1EpSceptical(type = t, c = c, alternative = "greater", level = 0.025)
#' })
#' matplot(x = c, y = t1*100, type = "l", lty = 1, lwd = 2, las = 1, log = "x",
#'         xlab = bquote(italic(c)), ylab = "Type-I error (%)", xlim = c(0.2, 5))
#' legend("topright", legend = types, lty = 1, lwd = 2, col = seq_along(types))
#' 
#' @export
T1EpSceptical <- Vectorize(.T1EpSceptical_)

