.PPpSceptical_ <- function(level, c, alpha, power,
                           alternative = c("one.sided", "two.sided"),
                           type = c("golden", "nominal", "controlled")) {

    stopifnot(is.numeric(level),
              length(level) == 1,
              is.finite(level),
              0 < level, level < 1,

              is.numeric(c),
              length(c) == 1,
              is.finite(c),
              0 <= c,

              is.numeric(alpha),
              length(alpha) == 1,
              is.finite(alpha),
              0 < alpha, alpha < 1,

              is.numeric(power),
              length(power) == 1,
              is.finite(power),
              0 < power, power < 1,

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

    }

    if (alternative != "two.sided") {
      alphas <- levelSceptical(level = level,
                               alternative = "one.sided",
                               type = type,
                               c = c)
    }

    # quick fix for alternative problems
    ## abs(.) is needed to deal with alternative="less"
    zas <- abs(p2z(p = alphas, alternative = alternative))
    #cm: really correct?


    ## compute mean based on alpha and power
    ## abs(.) is needed to deal with alternative="less"
    mu <- abs(p2z(p = alpha, alternative = alternative)) + stats::qnorm(p = power)

    ## compute project power with numerical integration
    if (alternative == "two.sided") {
        ## define function to integrate over zo
        intFun <- function(zo) {
            ## compute minimal zr to achieve replication success given zo and level
            K <- zo^2/zas^2
            zrmin <- zas*sqrt(1 + c/(K - 1))

            ## compute integrand
            ifelse(sign(zo) == 1,
                   ## on positive side of plane (zo, zr > 0): P(|zr| >= zrmin)*dnorm(zo)
            (stats::pnorm(q = zrmin, mean = sqrt(c)*mu, lower.tail = FALSE) +
             stats::pnorm(q = -zrmin, mean = sqrt(c)*mu, lower.tail = TRUE))*
            stats::dnorm(x = zo, mean = mu),

            ## on negative side of plane (zo, zr < 0): P(zr <= -zrmin)*dnorm(zo)
            (stats::pnorm(q = zrmin, mean = sqrt(c)*mu, lower.tail = FALSE) +
             stats::pnorm(q = -zrmin, mean = sqrt(c)*mu, lower.tail = TRUE))*
            stats::dnorm(x = zo, mean = mu)
            )
        }
    }

    if (alternative != "two.sided") {
                                        # define function to integrate over zo
        intFun <- function(zo) {
            ## compute minimal zr to achieve replication success given zo and level
            K <- zo^2/zas^2
            zrmin <- zas*sqrt(1 + c/(K - 1))

            ## compute integrand
            ifelse(sign(zo) == 1,
                   ## on positive side of plane (zo, zr > 0): P(zr >= zrmin)*dnorm(zo)
                   stats::pnorm(q = zrmin, mean = sqrt(c)*mu, lower.tail = FALSE)*
                   stats::dnorm(x = zo, mean = mu),

                   ## on negative side of plane (zo, zr < 0): P(zr <= -zrmin)*dnorm(zo)
                   ## (will be very small for large mu)
                   stats::pnorm(q = -zrmin, mean = sqrt(c)*mu, lower.tail = TRUE)*
                   stats::dnorm(x = zo, mean = mu)
                   )
        }
    }

    if (alternative == "two.sided") {
        ## integrate zo, zr over region where replication success possible
        pp <- stats::integrate(f = intFun, lower = zas, upper = Inf)$value +
                                                                   stats::integrate(f = intFun, lower = -Inf, upper = -zas)$value
    }
    if (alternative == "one.sided") {
        ## integrate zo, zr over region where replication success possible
        pp <- stats::integrate(f = intFun, lower = zas, upper = Inf)$value
    }

    return(pp)
}

#' Compute project power of the sceptical p-value
#'
#' The project power of the sceptical p-value is computed for a
#' specified level, the relative variance,
#' significance level and power for a standard significance test of
#' the original study, and the alternative hypothesis.
#' @param level Threshold for the calibrated sceptical p-value.
#'  Default is 0.025.
#' @param c Numeric vector of variance ratios of the original and replication
#' effect estimates. This is usually the ratio of the sample
#' size of the replication study to the sample size of the
#' original study.
#' @param alpha Significance level for a standard significance test in
#' the original study. Default is 0.025.
#' @param power Power to detect the assumed effect with a standard significance test
#' in the original study.
#' @param alternative Specifies if \code{level} and
#' \code{alpha} are "two.sided" or "one.sided".
#' @param type Type of recalibration. Can be either "golden" (default), "nominal" (no recalibration),
#'  or "controlled".
#' @return The project power of the sceptical p-value
#' @details \code{PPpSceptical} is the vectorized version of
#' the internal function \code{.PPpSceptical_}.
#' \code{\link[base]{Vectorize}} is used to vectorize the function.
#' @references
#' Held, L. (2020). The harmonic mean chi-squared test to substantiate
#' scientific findings. \emph{Journal of the Royal Statistical Society: Series C
#' (Applied Statistics)}, \bold{69}, 697-708. \doi{10.1111/rssc.12410}
#'
#' Held, L., Micheloud, C., Pawel, S. (2022). The assessment of replication
#'     success based on relative effect size. The Annals of Applied Statistics.
#'     16:706-720.\doi{10.1214/21-AOAS1502}
#'
#' Maca, J., Gallo, P., Branson, M., and Maurer, W. (2002).  Reconsidering some aspects
#' of the two-trials paradigm. \emph{Journal of Biopharmaceutical Statistics}, \bold{12},
#' 107-119. \doi{10.1081/bip-120006450}
#'
#' @seealso \code{\link{pSceptical}}, \code{\link{levelSceptical}}, \code{\link{T1EpSceptical}}
#' @author Leonhard Held, Samuel Pawel
#' @examples
#' ## compare project power for different recalibration types
#' types <- c("nominal", "golden", "controlled")
#' c <- seq(0.4, 5, by = 0.01)
#' alpha <- 0.025
#' power <- 0.9
#' pp <- sapply(X = types, FUN = function(t) {
#'   PPpSceptical(type = t, c = c, alpha, power, alternative = "one.sided",
#'                level = 0.025)
#' })
#'
#' ## compute project power of 2 trials rule
#' za <- qnorm(p = 1 - alpha)
#' mu <- za + qnorm(p = power)
#' pp2TR <- power * pnorm(q = za, mean = sqrt(c) * mu, lower.tail = FALSE)
#'
#' matplot(x = c, y = pp * 100, type = "l", lty = 1, lwd = 2, las = 1, log = "x",
#'         xlab = bquote(italic(c)), ylab = "Project power (%)", xlim = c(0.4, 5),
#'         ylim = c(0, 100))
#' lines(x = c, y = pp2TR * 100, col = length(types) + 1, lwd = 2)
#' abline(v = 1, lty = 2)
#' abline(h = 90, lty = 2, col = "lightgrey")
#' legend("bottomright", legend = c(types, "2TR"), lty = 1, lwd = 2,
#'        col = seq(1, length(types) + 1))
#' @export
PPpSceptical <- Vectorize(.PPpSceptical_)
