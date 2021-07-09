#' @export
.sampleSizeReplicationSuccess_ <- function(zo,
                                           power = NA,
                                           d = NA,
                                           level = 0.025,
                                           alternative = c("one.sided", "two.sided"),
                                           type = c("golden", "nominal", "liberal", "controlled"),
                                           designPrior = c("conditional", "predictive", "EB"),
                                           shrinkage = 0,
                                           h = 0){

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

    stopifnot(!is.null(type))
    type <- match.arg(type)

    stopifnot(!is.null(designPrior))
    designPrior <- match.arg(designPrior)

    stopifnot(is.numeric(shrinkage),
              length(shrinkage) == 1,
              is.finite(shrinkage),
              0 <= shrinkage, shrinkage < 1,

              is.numeric(h),
              is.finite(h),
              0 <= h)

    ## computing some quantities
    zoabs <- abs(zo)
    alphaS <- levelSceptical(level = level, alternative = alternative,
                             type = type)
    zalphaS <- p2z(p = alphaS, alternative = alternative)
    k <- zoabs^2/zalphaS^2

    ## if zoabs < zalphaS, replication success impossible
    if (zoabs < zalphaS) {
        warning(paste("Replication success at level", signif(level, 3),
                      "impossible for |zo| <", round(zalphaS, 3)))
        c <- NaN
    } else {

        ## sample size calculation based on power
        if (is.na(d)) {
            stopifnot(level < power)

            ## computing power quantile
            u <- qnorm(p = power)

            ## determining parameters based on design prior
            if (designPrior == "conditional") {
                s <- shrinkage # minor: in some functions (powerReplicationSuccess eg) we have s <- 1 - shrinkage
                H <- 0
            } else if (designPrior == "predictive") {
                s <- shrinkage
                H <- 1 + 2*h
            } else { ## designPrior == "EB"
                ## computing empirical Bayes shrinkage factor
                s <- pmin((1 + h)/zoabs^2, 1)
                H <- 1 - s + 2*h - s*h
            }

            ## checking whether power > powerlimit
            if (designPrior != "conditional") {
                powLim <- pnorm(q = (1 - s)*zoabs, mean = zalphaS/sqrt(k - 1), sd = sqrt(H))
            } else { ## for conditional more complicated
                zlim <- zalphaS/sqrt(k - 1)
                if (zoabs*(1 - s) > zlim) {
                    powLim <- 1
                } else if (isTRUE(all.equal(zoabs*(1 - s), zlim, tolerance = 1e-5))) {
                    powLim <- 0.5
                } else {
                    ## power-curve is non-monotone with a maximum...
                    cmax <- pmax((k - 1)/(zalphaS^2/(k - 1)/(1 - s)^2/zoabs^2 - 1), 0,
                                 na.rm = TRUE)
                    powLim <- powerReplicationSuccess(zo = zoabs, c = cmax, level = level,
                                                      designPrior = "conditional",
                                                      alternative = alternative, type = type,
                                                      shrinkage = shrinkage, h = h)
                }
            }
            if (power > powLim) {
                c <- NaN
                warning(paste(designPrior, "power cannot be larger than",
                              round(powLim, 3), "for supplied input"))
            } else {

                ## solving (quadratic) equation
                A <- 1/k - u^2/zoabs^2
                B <- -2*(1 - s)/sqrt(k)
                C <- (1 - s)^2 - u^2/zoabs^2*(H - 1/(k - 1))

                ## check whether quadratic term cancels
                if (isTRUE(all.equal(A, 0, tolerance = 1e-5))) {
                    res <- 1/(C^2/B^2 - 1/(k - 1))
                } else {
                    ## select correct solution
                    if (power > 0.5) {
                        x <- 0.5*(-B - sqrt(B^2 - 4*A*C))/A
                    } else {
                        x <- 0.5*(-B + sqrt(B^2 - 4*A*C))/A
                    }
                    res <- 1/(x^2 - 1/(k - 1))
                }

                ## relative variances need to be positive
                if (is.na(res) || res < 0) {
                    c <- NaN
                } else {
                    c <- res
                }
            }

        } else {  ## sample size calculation based on relative effect size
            denom <- d^2*k - 1/(k - 1)
            if (denom > 0) {
                c <- 1/denom
            } else {
                c <- NaN
            }
        }
    }
    return(c)
}

#' Computes the required relative sample size to achieve replication success
#' based on power or on the minimum relative effect size
#'
#' The relative sample size to achieve replication success is computed based on
#' the z-value of the original study, the replication success level, the type of
#' recalibration and either the power or the minimum relative effect size. When
#' the approach based on power is used, the design prior also has to be
#' specified.
#' @param zo Numeric vector of z-values from original studies.
#' @param power The power to achieve replication success.
#' @param d The minimum relative effect size (ratio of the effect estimate from
#'     the replication study to the effect estimate from the original study) to
#'     achieve replication success.
#' @param level Numeric vector of replication success levels. The default is
#'     0.025.
#' @param alternative Either "one.sided" (default) or "two.sided". Specifies if
#'     the replication success level is one-sided or two-sided.
#' @param type Type of recalibration. Can be either "golden" (default),
#'     "nominal" (no recalibration), "liberal", "controlled". "golden" ensures
#'     that for an original study just significant at the specified
#'     \code{level}, replication success is only possible if the replication
#'     effect estimate is at least as large as the original one. See
#'     \code{\link{levelSceptical}} for details about recalibration types.
#' @param designPrior Is only taken into account when \code{power} is specified.
#'     Either "conditional" (default), "predictive", or "EB". If "EB", the power
#'     is computed under a predictive distribution where the contribution of the
#'     original study is shrunken towards zero based on the evidence in the
#'     original study (with an empirical Bayes shrinkage estimator).
#' @param shrinkage Is only taken into account when \code{power} is specified. A
#'     number in [0,1) with default 0. Specifies the shrinkage of the original
#'     effect estimate towards zero (e.g., the effect is shrunken by a factor of
#'     25\% for \code{shrinkage = 0.25}). Is only taken into account when the
#'     \code{designPrior} is "conditional" or "predictive".
#' @param h Is only taken into account when \code{power} is specified and
#'     \code{designPrior} is "predictive" or "EB". The relative between-study
#'     heterogeneity, i.e., the ratio of the heterogeneity variance to the
#'     variance of the original effect estimate. Default is 0 (no
#'     heterogeneity).
#' @return The relative sample size for replication success. If impossible to
#'     achieve the desired power for specified inputs \code{NaN} is returned.
#' @details \code{sampleSizeReplicationSuccess} is the vectorized version of
#'     \code{.sampleSizeReplicationSuccess_}. \code{\link[base]{Vectorize}} is
#'     used to vectorize the function.
#' @references
#' Held, L. (2020). A new standard for the analysis and design of replication
#' studies (with discussion). \emph{Journal of the Royal Statistical Society:
#' Series A (Statistics in Society)}, \bold{183}, 431-448.
#' \doi{10.1111/rssa.12493}
#'
#' Held, L., Micheloud, C., Pawel, S. (2021). The assessment of replication success
#' based on relative effect size. \url{https://arxiv.org/abs/2009.07782}
#' @author Leonhard Held, Charlotte Micheloud, Samuel Pawel, Florian Gerber
#' @seealso \code{\link{pSceptical}}, \code{\link{powerReplicationSuccess}}, \code{\link{levelSceptical}}
#' @examples
#' ## based on power
#' sampleSizeReplicationSuccess(zo = p2z(0.0025), power = 0.8, level = 0.025,
#'                              type = "golden")
#' sampleSizeReplicationSuccess(zo = p2z(0.0025), power = 0.8, level = 0.025,
#'                              type = "golden", designPrior = "predictive")
#'
#' ## based on minimum relative effect size
#' sampleSizeReplicationSuccess(zo = p2z(0.0025), d = 0.9, level = 0.025,
#'                              type = "nominal")
#' sampleSizeReplicationSuccess(zo = p2z(0.0025), d = 0.9, level = 0.025,
#'                              type = "golden")
#' @export
sampleSizeReplicationSuccess <- Vectorize(.sampleSizeReplicationSuccess_)


## numerical implementation
sampleSizeReplicationSuccessTarget <- function(zo, c, p, level, designPrior, alternative, type = type,
                   shrinkage){
    zr2 <- zr2quantile(zo = zo, c = c, p = p, designPrior = designPrior, 
                       shrinkage = shrinkage)
    pC <- pSceptical(zo = zo, zr = sqrt(zr2), c = c, 
                     alternative = alternative, type = type)
    return(pC - level)
}

.sampleSizeReplicationSuccessNum_ <- function(zo,
                                              power = NA,
                                              d = NA,
                                              level = 0.025,
                                              alternative = c("one.sided", "two.sided"),
                                              type = c("golden", "nominal", "liberal", "controlled"),
                                              designPrior = c("conditional", "predictive", "EB"),
                                              shrinkage = 0){

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
    
    stopifnot(!is.null(type))
    type <- match.arg(type)
    
    stopifnot(!is.null(designPrior))
    designPrior <- match.arg(designPrior)
    
    stopifnot(is.numeric(shrinkage),
              length(shrinkage) == 1,
              is.finite(shrinkage),
              0 <= shrinkage, shrinkage < 1)

    mylower <- 0
    myupper <- 1000
    
    ## sample size calculation based on power
    if (is.na(d)){
        target.l <- sampleSizeReplicationSuccessTarget(c = mylower, 
                                                       zo = zo,
                                                       p = 1 - power,
                                                       level = level,
                                                       designPrior = designPrior,
                                                       alternative = alternative,
                                                       type = type,
                                                       shrinkage = shrinkage)
        target.u <- sampleSizeReplicationSuccessTarget(c = myupper, 
                                                       zo = zo,
                                                       p = 1 - power,
                                                       level = level,
                                                       designPrior = designPrior,
                                                       alternative = alternative,
                                                       type = type,
                                                       shrinkage = shrinkage)
        if (sign(target.l) == sign(target.u)) {
            if(sign(target.u) > 0)
                c <- Inf
            else 
                c <- NA
        }
        else {
            c <- uniroot(f = sampleSizeReplicationSuccessTarget, 
                         lower = mylower, 
                         upper = myupper, 
                         zo = zo, 
                         p = 1 - power, 
                         level = level,
                         designPrior = designPrior,
                         alternative = alternative,
                         type = type,
                         shrinkage = shrinkage)$root
        }
    } else { # sample size calculation based on relative effect size
        alphas <- levelSceptical(level = level, 
                                 alternative = alternative, 
                                 type = type)
        zalphas <- p2z(alphas, alternative = alternative)
        K <- zo^2/zalphas^2
        denom <- d^2*K - 1/(K-1)
        if (zalphas > zo){
            warning(paste("Replication success is not achievable at this level as", 
                          zo, " < ", round(p2z(levelSceptical(level = level,
                                                              alternative = alternative,
                                                              type = type)),
                                           3)))
            c <- NA
        } else { 
            c <- ifelse(denom > 0, 1/denom, NA) 
        }
    }
    return(c)
}

sampleSizeReplicationSuccessNum <- Vectorize(.sampleSizeReplicationSuccessNum_)
