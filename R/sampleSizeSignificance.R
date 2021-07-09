.sampleSizeSignificance_ <- function(zo,
                                     power = NA,
                                     d = NA,
                                     level = 0.025,
                                     alternative = c("one.sided", "two.sided"),
                                     designPrior = c("conditional", "predictive", "EB"),
                                     h = 0,
                                     shrinkage = 0) {

    stopifnot(is.numeric(zo),
              length(zo) == 1,
              is.finite(zo))

    stopifnot(length(power) == 1,
              length(d) == 1)
    if (is.na(d) && is.na(power))  stop("either 'power' or 'd' has to be specified")
    if (!is.na(d) && !is.na(power))  stop("only one of 'power' or 'd' has to be specified")
    if (!is.na(d)) {
        stopifnot(is.numeric(d),
                  is.finite(d))
    } else { #!is.na(power)
        stopifnot(is.numeric(power),
                  0 < power, power < 1)
    }

    stopifnot(is.numeric(level),
              length(level) == 1,
              is.finite(level),
              0 < level, level < 1,

              !is.null(alternative))
    alternative <- match.arg(alternative)

    stopifnot(!is.null(designPrior))
    designPrior <- match.arg(designPrior)

    stopifnot(is.numeric(h),
              length(h) == 1,
              is.finite(h),
              0 <= h,

              is.numeric(shrinkage),
              length(shrinkage) == 1,
              is.finite(shrinkage),
              0 <= shrinkage, shrinkage < 1)

    ## sample size calculation based on power
    if (is.na(d)) {
        ## only allow power > level
        stopifnot(level < power)

        u <- qnorm(p = power)
        v <- p2z(p = level, alternative = alternative)
        zoabs <- abs(zo)

        ## conditional
        if (designPrior == "conditional") {
            c <- (u + v)^2*(1/((1 - shrinkage)*zoabs))^2
        } else {
            ## computing parameters
            if (designPrior == "EB") {
                shrinkage <- pmin((1 + h)/zoabs^2, 1)
                H <- 1 - shrinkage + 2*h - shrinkage*h
            } else {
                H <- 1 + 2*h
            }
            ## checking whether power larger than power limit
            ## powLim <- pnorm(q = (1 - shrinkage)*zoabs/sqrt(H))
            ## if (is.na(powLim) power > powLim) {
            ##   c <- NaN
            ##   warning(paste0("Power cannot be larger than ", round(powLim, 3)))
            ## } else {
            if ((zoabs^2*(1 - shrinkage)^2 <= H*u^2) && power > 0.5) {
                c <- NaN
            } else {
                zos <- (1 - shrinkage)*zoabs
                num <- zos*v + u*sqrt(zos^2 + H*(v^2 - u^2))
                denom <- zos^2 - u^2*H
                sqrtc <- num/denom
                if (is.na(sqrtc) || sqrtc < 0) {
                    c <- NaN
                } else {
                    c <- sqrtc^2
                }
            }
        }
    } else { ## sample size calculation based on relative effect size
        zalpha <- p2z(p = level, alternative = alternative)
        c <- zalpha^2/(d^2*zo^2)
    }


    return(c)
}

#' Computes the required relative sample size to achieve significance
#' based on power or on the minimum relative effect size
#'
#' The relative sample size to achieve significance of the replication study is
#' computed based on the z-value of the original study, the significance level
#' and either the power or the minimum relative effect size. When the approach
#' based on power is used, the arguments design prior, shrinkage, and relative
#' heterogeneity also have to be specified.
#' @name sampleSizeSignificance
#' @rdname sampleSizeSignificance
#' @author Leonhard Held, Samuel Pawel, Charlotte Micheloud, Florian Gerber
#' @param zo A vector of z-values from original studies.
#' @param power The power to achieve replication success.
#' @param d The minimum relative effect size (ratio of the effect estimate from
#'     the replication study to the effect estimate from the original study).
#' @param level Significance level. Default is 0.025.
#' @param alternative Either "one.sided" (default) or "two.sided". Specifies
#'     direction of the alternative. "one.sided" assumes an effect in the same
#'     direction as the original estimate.
#' @param designPrior Is only taken into account when \code{power} is specified.
#'     Either "conditional" (default), "predictive", or "EB". If "EB", the power
#'     is computed under a predictive distribution where the contribution of the
#'     original study is shrunken towards zero based on the evidence in the
#'     original study (with an empirical Bayes shrinkage estimator).
#' @param h Is only taken into account when \code{power} is specified and
#'     \code{designPrior} is "predictive" or "EB". The relative between-study
#'     heterogeneity, i.e., the ratio of the heterogeneity variance to the
#'     variance of the original effect estimate. Default is 0 (no
#'     heterogeneity).
#' @param shrinkage Is only taken into account when \code{power} is specified. A
#'     number in [0,1) with default 0. Specifies the shrinkage of the original effect
#'     towards zero (e.g., \code{shrinkage = 0.25} implies shrinkage by a
#'     factor of 25\%). Is only taken into account when \code{designPrior} is
#'     "conditional" or "predictive".
#' @return The relative sample size to achieve significance in the specified
#'     direction. If impossible to achieve the desired power for specified
#'     inputs \code{NaN} is returned.
#' @details \code{sampleSizeSignificance} is the vectorized version of
#'     \code{.sampleSizeSignificance_}. \code{\link[base]{Vectorize}} is used to
#'     vectorize the function.
#' @seealso \code{\link{powerSignificance}}
#' @references
#' Held, L. (2020). A new standard for the analysis and design of replication
#' studies (with discussion). \emph{Journal of the Royal Statistical Society:
#' Series A (Statistics in Society)}, \bold{183}, 431-448.
#' \doi{10.1111/rssa.12493}
#'
#' Pawel, S., Held, L. (2020). Probabilistic forecasting of replication studies.
#' \emph{PLoS ONE}. \bold{15}, e0231416. \doi{10.1371/journal.pone.0231416}
#'
#' Held, L., Micheloud, C., Pawel, S. (2021). The assessment of replication
#' success based on relative effect size. \url{https://arxiv.org/abs/2009.07782}
#' @examples
#' sampleSizeSignificance(zo = p2z(0.005), power = 0.8)
#' sampleSizeSignificance(zo = p2z(0.005, alternative = "two.sided"), power = 0.8)
#' sampleSizeSignificance(zo = p2z(0.005), power = 0.8, designPrior = "predictive")
#'
#' sampleSizeSignificance(zo = 3, power = 0.8, designPrior = "predictive",
#'                        shrinkage = 0.5, h = 0.25)
#' sampleSizeSignificance(zo = 3, power = 0.8, designPrior = "EB",  h = 0.5)
#'
#' # sample size to achieve  0.8 power as function of original p-value
#' zo <- p2z(seq(0.0001, 0.05, 0.0001))
#' oldPar <- par(mfrow = c(1,2))
#' plot(z2p(zo), sampleSizeSignificance(zo = zo, designPrior = "conditional", power = 0.8),
#'      type = "l", ylim = c(0.5, 10), log = "y", lwd = 1.5, ylab = "Relative sample size",
#'      xlab = expression(italic(p)[o]), las = 1)
#' lines(z2p(zo), sampleSizeSignificance(zo = zo, designPrior = "predictive", power = 0.8),
#'       lwd = 2, lty = 2)
#' lines(z2p(zo), sampleSizeSignificance(zo = zo, designPrior = "EB", power = 0.8),
#'       lwd = 1.5, lty = 3)
#' legend("topleft", legend = c("conditional", "predictive", "EB"),
#'        title = "Design prior", lty = c(1, 2, 3), lwd = 1.5, bty = "n")
#'
#' sampleSizeSignificance(zo = p2z(0.005), d = 1)
#' sampleSizeSignificance(zo = p2z(0.005), d = 0.5)
#' # sample size based on minimum relative effect size of 0.8
#' zo <- p2z(seq(0.0001, 0.05, 0.0001))
#' plot(z2p(zo), sampleSizeSignificance(zo = zo, d = 0.8, level = 0.025),
#'      type = "l", ylim = c(0.5, 10), log = "y", lwd = 1.5, ylab = "Relative sample size",
#'     xlab = expression(italic(p)[o]), las = 1)
#' par(oldPar)
#' @export
sampleSizeSignificance <- Vectorize(.sampleSizeSignificance_)



## Functions for numerical implementation
sampleSizeSignificanceTarget <- function(c, zo, level, power, alternative,
                            h, shrinkage, designPrior){
    term <- powerSignificance(zo = zo, 
                              c = c, 
                              level = level,
                              designPrior = designPrior,
                              alternative = alternative,
                              h = h,
                              shrinkage = shrinkage)
    return(term - power)
}

.sampleSizeSignificanceNum_ <- function(zo,
                                        power = NA,
                                        d = NA,
                                        level = 0.025,
                                        alternative = c("one.sided", "two.sided", "less", "greater"),
                                        designPrior = c("conditional", "predictive", "EB"),
                                        h = 0,
                                        shrinkage = 0) {



    stopifnot(is.numeric(zo),
              length(zo) == 1,
              is.finite(zo))
    
    stopifnot(length(power) == 1,
              length(d) == 1)
    if (is.na(d) && is.na(power))  stop("either 'power' or 'd' has to be specified")
    if (!is.na(d) && !is.na(power))  stop("only one of 'power' or 'd' has to be specified")
    if (!is.na(d)) {
        stopifnot(is.numeric(d),
                  is.finite(d))
    } else { #!is.na(power)
        stopifnot(is.numeric(power),
                  0 < power, power < 1)
    }
    
    stopifnot(is.numeric(level),
              length(level) == 1,
              is.finite(level),
              0 < level, level < 1,
              
              !is.null(alternative))
    alternative <- match.arg(alternative)

    stopifnot(!is.null(designPrior))
    designPrior <- match.arg(designPrior)
    
    stopifnot(is.numeric(h),
              length(h) == 1,
              is.finite(h),
              0 <= h,

              is.numeric(shrinkage),
              length(shrinkage) == 1,
              is.finite(shrinkage),
              0 <= shrinkage, shrinkage < 1)

    n.l <- 0
    n.u <- 1000
    
    ## sample size calculation based on power
    if (is.na(d)) {
        s <- 1 - shrinkage
        
        ## for conditional designPrior use analytical solution
        if (designPrior == "conditional") {
            u <- qnorm(p = power)
            v <- p2z(p = level, alternative = alternative)
            c <- (u + v)^2*(1/(s*zo))^2
        }
        
        
        ## for predictive and EB designPrior use uniroot
        if (designPrior %in% c("predictive", "EB")) {
            
                                        # compute upper bound of power
                                        # if (designPrior == "EB") s <- pmax(1 - (1 + h)/zo^2, 0)
                                        # power.limit <- pnorm(sqrt(1/(s*(1 + h) + h))*s*abs(zo))
                                        # if (power > power.limit) {
                                        #     power.limit.r <- floor(power.limit * 1000)/1000
                                        #     warning(paste("power too large, power should not exceed",
                                        #                    power.limit.r,
                                        #                   "for a zo of",
                                        #                   zo,
                                        #                   "\n"))
                                        #     c <- NaN
                                        # } else {
            
                                        # check whether desired power can be achieved for max c = n.u
            target.l <- sampleSizeSignificanceTarget(c = n.l, 
                                                     zo = zo,
                                                     level = level,
                                                     power = power,
                                                     alternative = alternative,
                                                     h = h,
                                                     shrinkage = shrinkage,
                                                     designPrior = designPrior)
            target.u <- sampleSizeSignificanceTarget(c = n.u, 
                                                     zo = zo,
                                                     level = level,
                                                     power = power,
                                                     alternative = alternative,
                                                     h = h,
                                                     shrinkage = shrinkage,
                                                     designPrior = designPrior)
            if (sign(target.l) == sign(target.u)) 
                c <- NaN
                                        # determine c to achieve desired power
            else c <- uniroot(f = sampleSizeSignificanceTarget, 
                              lower = n.l, 
                              upper = n.u,
                              zo = zo,
                              level = level,
                              power = power, 
                              alternative = alternative,
                              h = h,
                              shrinkage = shrinkage,
                              designPrior = designPrior)$root
        }
    } else { # sample size calculation based on relative effect size
        zalpha <- qnorm(1- level)
        zalpha <- p2z(p = level, alternative = alternative)
        c <- zalpha^2/(d^2*zo^2)
    }
    return(c)
}


sampleSizeSignificanceNum <- Vectorize(.sampleSizeSignificanceNum_)
