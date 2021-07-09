#' Computes Box's tail probability
#'
#' \code{pBox} computes Box's tail probabilities based on the z-values of the
#' original and the replication study, the corresponding variance ratio,
#' and the significance level. 
#' @rdname pBox
#' @param zo Numeric vector of z-values from the original studies.
#' @param zr Numeric vector of z-values from replication studies.
#' @param c Numeric vector of variance ratios of the original and replication
#' effect estimates. This is usually the ratio of the sample
#' size of the replication study to the sample size of the
#' original study.
#' @param level Numeric vector of significance levels. Default is 0.05.
#' @param alternative Either "two.sided" (default) or "one.sided".
#' Specifies whether two-sided or one-sided Box's tail probabilities are computed.
#' @return \code{pBox} returns Box's tail probabilities. 
#' @references Box, G.E.P. (1980). Sampling and Bayes' inference in scientific
#' modelling and robustness (with discussion). \emph{Journal of the Royal Statistical
#' Society, Series A}, \bold{143}, 383-430.
#'
#' Held, L. (2020). A new standard for the analysis and design of replication
#' studies (with discussion). \emph{Journal of the Royal Statistical Society:
#' Series A (Statistics in Society)}, \bold{183}, 431-448.
#' \doi{10.1111/rssa.12493}
#' @author Leonhard Held
#' @examples
#' pBox(zo = p2z(0.01), zr = p2z(0.02), c = 2)
#' pBox(zo = p2z(0.02), zr = p2z(0.01), c = 1/2)
#' pBox(zo = p2z(0.02, alternative = "one.sided"),
#'      zr = p2z(0.01, alternative = "one.sided"),
#'      c = 1/2, alternative = "one.sided")
#' @export
pBox <- function(zo, zr, c, level = 0.05,
                 alternative = c("two.sided", "one.sided")){
    stopifnot(!is.null(alternative))
    alternative <- match.arg(alternative)    
    ## all other input parameters are checked in zBox()

    z <- zBox(zo = zo, zr = zr, c = c, level = level, alternative = alternative)
    res <- z2p(z = z)
    if(alternative == "one.sided")
        res <- ifelse(sign(zo) == sign(zr), res / 2, 1 - res / 2)
    return(res)
}

#' @rdname pBox
#' @return \code{zBox} returns the z-values used in \code{pBox}.
#' @export 
zBox <- function(zo, zr, c, level = 0.05,
                 alternative = c("two.sided", "one.sided")){
    stopifnot(is.numeric(zo),
              length(zo) >= 1,
              is.finite(zo),

              is.numeric(zr),
              length(zr) >= 1,
              is.finite(zr),

              is.numeric(c),
              length(c) >= 1,
              is.finite(c),
              c > 0,

              is.numeric(level),
              length(level) >= 1,
              is.finite(level),
              0 < level, level < 1,
              
              !is.null(alternative))
    alternative <- match.arg(alternative)

    z <- p2z(p = level, alternative = alternative)
    den <- ifelse((zo^2 > z^2), c / (zo^2 / z^2 - 1) + 1, NA)
    res <- zr^2 / den
    return(sqrt(res))
}

