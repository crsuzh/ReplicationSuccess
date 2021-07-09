#' @export
.powerReplicationSuccess_ <- function(zo,
                                      c = 1,
                                      level = 0.025,
                                      designPrior = c("conditional", "predictive", "EB"),
                                      alternative = c("one.sided", "two.sided"),
                                      type = c("golden", "nominal", "liberal", "controlled"),
                                      shrinkage = 0,
                                      h = 0,
                                      strict = FALSE){

    stopifnot(is.numeric(zo),
              length(zo) == 1,
              is.finite(zo),

              is.numeric(c),
              length(c) == 1,
              is.finite(c),
              0 <= c,

              is.numeric(level),
              length(level) == 1,
              is.finite(level),
              0 < level, level < 1,

              !is.null(designPrior))
    designPrior <- match.arg(designPrior)

    stopifnot(!is.null(alternative))
    alternative <- match.arg(alternative)

    stopifnot(!is.null(type))
    type <- match.arg(type)

    stopifnot(is.numeric(shrinkage),
              length(shrinkage) == 1,
              is.finite(shrinkage),
              0 <= shrinkage, shrinkage < 1,

              is.numeric(h),
              length(h) == 1,
              is.finite(h),
              0 <= h,

              is.logical(strict),
              length(strict) == 1)

    ## take the absolute value of zo for easier computations
    zoabs <- abs(zo)

    ## if zoas < zalphaS, power is zero
    alphaS <- levelSceptical(level = level, alternative = alternative, type = type)
    zalphaS <- p2z(p = alphaS, alternative = alternative)
    if (zoabs < zalphaS) {
        power <- 0
    } else {
        ## computing minimum zr to achieve replication success
        dmin <- effectSizeReplicationSuccess(zo = zoabs, c = c, level = level,
                                             alternative = alternative, type = type)
        zrmin <- dmin*zoabs*sqrt(c)

        if (designPrior == "conditional") {
            power <- pnorm(q = zrmin, mean = zoabs*(1 - shrinkage)*sqrt(c), sd = 1,
                           lower.tail = FALSE)

            if (strict && alternative == "two.sided") {
                power2 <- pnorm(q = -zrmin, mean = zoabs*(1 - shrinkage)*sqrt(c), sd = 1,
                                lower.tail = TRUE)
                power <- power + power2
            }
        } else if (designPrior == "predictive") {
            power <- pnorm(q = zrmin, mean = zoabs*(1 - shrinkage)*sqrt(c),
                           sd = sqrt(c*(1 + 2*h) + 1), lower.tail = FALSE)

            if (strict && alternative == "two.sided") {
                power2 <- pnorm(q = -zrmin, mean = zoabs*(1 - shrinkage)*sqrt(c),
                                sd = sqrt(c*(1 + 2*h) + 1), lower.tail = TRUE)
                power <- power + power2
            }
        } else { ## designPrior == "EB"
            EBshrinkage <- pmin((1 + h)/zoabs^2, 1)
            power <- pnorm(q = zrmin, mean = zoabs*(1 - EBshrinkage)*sqrt(c),
                           sd = sqrt((1 - EBshrinkage)*c*(1 + h) + 1 + c*h),
                           lower.tail = FALSE)

            if (strict && alternative == "two.sided") {
                power2 <- pnorm(q = -zrmin, mean = zoabs*(1 - EBshrinkage)*sqrt(c),
                                sd = sqrt((1 - EBshrinkage)*c*(1 + h) + 1 + c*h),
                                lower.tail = TRUE)
                power <- power + power2
            }
        }
    }
    return(power)
}


#' Computes the power for replication success
#'
#' Computes the power for replication success based on the result of the
#' original study, the corresponding variance ratio, and the design prior.
#' @param zo Numeric vector of z-values from original studies.
#' @param c Numeric vector of variance ratios of the original and replication
#'     effect estimates. This is usually the ratio of the sample size of the
#'     replication study to the sample size of the original study.
#' @param level Numeric vector of replication success levels. The default is
#'     0.025.
#' @param designPrior Either "conditional" (default), "predictive", or "EB". If
#'     "EB", the power is computed under a predictive distribution, where
#'     the contribution of the original study is shrunken towards zero based on
#'     the evidence in the original study (with an empirical Bayes shrinkage
#'     estimator).
#' @param alternative Either "one.sided" (default) or "two.sided". Specifies if
#'     the replication success level is one-sided or two-sided. If the
#'     replication success level is one-sided, then power calculations are based
#'     on a one-sided assessment of replication success in the direction of the
#'     original effect estimates.
#' @param type Recalibration type can be either "golden" (default), "nominal"
#'     (no recalibration), "liberal", or "controlled". \code{type} = "golden"
#'     ensures that for an original study just significant at the specified
#'     \code{level}, replication success is only possible if the replication
#'     effect estimate is larger than the original one. See
#'     \code{\link{levelSceptical}} for details about recalibration types.
#' @param shrinkage Numeric vector with values in [0,1). Defaults to 0.
#'     Specifies the shrinkage of the original effect estimate towards zero,
#'     e.g., the effect is shrunken by a factor of 25\% for
#'     \code{shrinkage = 0.25}. Is only taken into account if the
#'     \code{designPrior} is "conditional" or "predictive".
#' @param h Numeric vector of relative heterogeneity variances i.e., the ratio
#'     of the heterogeneity variance to the variance of the original effect
#'     estimate. Default is 0 (no heterogeneity). Is only taken into account
#'     when \code{designPrior} = "predictive" or \code{designPrior} = "EB".
#' @param strict Logical vector indicating whether the probability for
#'     replication success in the opposite direction of the original effect
#'     estimate should also be taken into account. Default is \code{FALSE}.
#'     Only taken into account when \code{alternative} = "two.sided".
#' @return The power for replication success.
#' @author Leonhard Held, Charlotte Micheloud, Samuel Pawel
#' @details \code{powerReplicationSuccess} is the vectorized version of
#'     \code{.powerReplicationSuccess_}. \code{\link[base]{Vectorize}} is used
#'     to vectorize the function.
#' @references
#' Held, L. (2020). A new standard for the analysis and design of replication
#' studies (with discussion). \emph{Journal of the Royal Statistical Society:
#' Series A (Statistics in Society)}, \bold{183}, 431-448.
#' \doi{10.1111/rssa.12493}
#'
#' Held, L., Micheloud, C., Pawel, S. (2021). The assessment of replication
#' success based on relative effect size. \url{https://arxiv.org/abs/2009.07782}
#' @seealso \code{\link{sampleSizeReplicationSuccess}}, \code{\link{pSceptical}},
#' \code{\link{levelSceptical}}
#' @examples
#' ## larger sample size in replication (c > 1)
#' powerReplicationSuccess(zo = p2z(0.005), c = 2, level = 0.025, designPrior = "conditional")
#' powerReplicationSuccess(zo = p2z(0.005), c = 2, level = 0.025, designPrior = "predictive")
#'
#' ## smaller sample size in replication (c < 1)
#' powerReplicationSuccess(zo = p2z(0.005), c = 1/2, level = 0.025, designPrior = "conditional")
#' powerReplicationSuccess(zo = p2z(0.005), c = 1/2, level = 0.025, designPrior = "predictive")
#' 
#' powerReplicationSuccess(zo = p2z(0.00005), c = 2, level = 0.05, 
#'                         alternative = "two.sided",  strict = TRUE, shrinkage = 0.9)
#' powerReplicationSuccess(zo = p2z(0.00005), c = 2, level = 0.05, 
#'                         alternative = "two.sided", strict = FALSE, shrinkage = 0.9)
#' 
#' @export
powerReplicationSuccess <- Vectorize(.powerReplicationSuccess_)


# function that returns z_r^2 quantile for given z_o, c
zr2quantile <- function(zo,
                        c,
                        p,
                        designPrior,
                        shrinkage){

    if (designPrior == "predictive"){
        lambda <- (1 - shrinkage)^2*zo^2/(1 + 1/c)
        factor <- c + 1
        # h <- 0 # relative heterogeneity h, implement later
        # lambda <- (1 - shrinkage)^2*zo^2/(1/c + 1 + 2*h)
        # factor <- 1 + c + 2*h*c
    }
    if (designPrior == "conditional"){
        lambda <- (1 - shrinkage)^2*c*zo^2
        factor <- 1
    }
    if (designPrior == "EB") {
        s <- pmax(1 - 1/zo^2, 0)
        lambda <- zo^2*s^2/(s + 1/c)
        factor <- s*c + 1
        # h <- 0 # relative heterogeneity h, implement later
        # s <- pmax(1 - (1 + h)/zo^2, 0)
        # lambda <- zo^2*s^2/(s*(1 + h) + 1/c + h)
        # factor <- s*c*(1 + h) + 1 + h*c
    }
    if (lambda < 100)
        res <- qchisq(p = p, df = 1, ncp = lambda)
    else
        res <- qnorm(p = p, mean = sqrt(lambda), sd = 1)^2
    return(factor*res)
}


powerReplicationSuccessTargetPower <- function(power, zo, c, level, designPrior, alternative, type,
                        shrinkage){
    zr2 <- zr2quantile(zo = zo, c = c, p = 1 - power, 
                       designPrior = designPrior, shrinkage = shrinkage)
    pC <- pSceptical(zo = zo, zr = sqrt(zr2), c = c,
                     alternative = alternative, type = type)
    return(pC - level)
                                        # pC <- pSceptical(zo = zo, zr = sqrt(zr2), c = c, 
                                        #                  alternative = alternative)
                                        # return(pC - levelSceptical(level = level, alternative = alternative,
                                        #                                type = type))
}

.powerReplicationSuccessNum_ <- function(zo,
                                         c = 1,
                                         level = 0.025,
                                         designPrior = c("conditional", "predictive", "EB"),
                                         alternative = c("one.sided", "two.sided"),
                                         type = c("golden", "nominal", "liberal", "controlled"),
                                         shrinkage = 0){

    stopifnot(is.numeric(zo),
              length(zo) == 1,
              is.finite(zo),
              
              is.numeric(c),
              length(c) == 1,
              is.finite(c),
              0 <= c,
              
              is.numeric(level),
              length(level) == 1,
              is.finite(level),
              0 < level, level < 1,

              !is.null(designPrior))
    designPrior <- match.arg(designPrior)

    stopifnot(!is.null(alternative))
    alternative <- match.arg(alternative)

    stopifnot(!is.null(type))
    type <- match.arg(type)

    stopifnot(is.numeric(shrinkage),
              length(shrinkage) == 1,
              is.finite(shrinkage),
              0 <= shrinkage, shrinkage < 1)


    ## check if original study was not significant, then power is zero
    zo <- abs(zo)
    p <- z2p(z = zo, alternative = alternative)
    eps <- 1e-5
    mylower <- eps
    myupper <- 1 - eps
    
    if (p > levelSceptical(level = level, alternative = alternative, 
                           type = type))
        res <- 0
    else {
        targetLower <- powerReplicationSuccessTargetPower(power = mylower, 
                                                        zo = zo, 
                                                        c = c, 
                                                        level = level,
                                                        designPrior = designPrior,
                                                        alternative = alternative,
                                                        type = type,
                                                        shrinkage = shrinkage)
        targetUpper <- powerReplicationSuccessTargetPower(power = myupper, 
                                                        zo = zo, 
                                                        c = c, 
                                                        level = level,
                                                        designPrior = designPrior,
                                                        alternative = alternative,
                                                        type = type,
                                                        shrinkage = shrinkage)
        if (sign(targetLower) == sign(targetUpper)) {
            if ((sign(targetLower) >= 0) & (sign(targetUpper) >= 0))
                res <- 0
            if ((sign(targetLower) < 0) & (sign(targetUpper) < 0))
                res <- 1
        }
        else {
            res <- uniroot(f = powerReplicationSuccessTargetPower, 
                           lower = mylower, 
                           upper = myupper,
                           zo = zo, 
                           c = c, 
                           level = level,
                           designPrior = designPrior,
                           alternative = alternative,
                           type = type,
                           shrinkage = shrinkage)$root
        }
    }
    return(res)
}

powerReplicationSuccessNum <- Vectorize(.powerReplicationSuccessNum_)
