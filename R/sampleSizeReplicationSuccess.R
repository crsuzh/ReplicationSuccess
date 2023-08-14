## numerical implementation
targetSS <- function(
    zo,
    c,
    power,
    level,
    designPrior,
    alternative,
    type = type,
    shrinkage) {

    term <- powerReplicationSuccess(
        zo = zo,
        c = c,
        level = level,
        designPrior = designPrior,
        alternative = alternative,
        type = type,
        shrinkage = shrinkage
    )
  return(term - power)
}


.sampleSizeReplicationSuccessNum_ <- function(
    zo,
    power = NA,
    # d = NA,
    level = 0.025,
    alternative = c("one.sided", "two.sided"),
    type = c("golden", "nominal", "controlled"),
    designPrior = c("conditional", "predictive", "EB"),
    shrinkage = 0) {

    stopifnot(is.numeric(zo),
              length(zo) == 1,
              is.finite(zo))

    stopifnot(length(power) == 1
              # length(d) == 1
             )
  # if (is.na(d) && is.na(power))  stop("either 'power' or 'd' has to be specified")
  # if (!is.na(d) && !is.na(power))  stop("only one of 'power' or 'd' has to be specified")
  # if (!is.na(d)) {
  #   stopifnot(is.numeric(d),
  #             is.finite(d))
  # } else { #!is.na(power)
    stopifnot(is.numeric(power),
              0 < power, power < 1)
  # }

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

    eps <- 10e-6
    mylower <- eps
    myupper <- 1000

    ## sample size calculation based on power
    # if (is.na(d)) {
    target.l <- targetSS(c = mylower,
                         zo = zo,
                         power = power,
                         level = level,
                         designPrior = designPrior,
                         alternative = alternative,
                         type = type,
                         shrinkage = shrinkage)
    target.u <- targetSS(c = myupper,
                         zo = zo,
                         power = power,
                         level = level,
                         designPrior = designPrior,
                         alternative = alternative,
                         type = type,
                         shrinkage = shrinkage)
    if (sign(target.l) == sign(target.u)) {
        if (sign(target.u) > 0)
            c <- Inf
        else
            c <- NA
    } else {
        c <- stats::uniroot(
            f = targetSS,
            lower = mylower,
            upper = myupper,
            zo = zo,
            power = power,
            level = level,
            designPrior = designPrior,
            alternative = alternative,
            type = type,
            shrinkage = shrinkage
        )$root
    }

  # }
  # based on d : not done for controlled yet
  # } else { # sample size calculation based on relative effect size
  #   alphas <- levelSceptical(level = level,
  #                            alternative = alternative,
  #                            type = type)
  #   zalphas <- p2z(alphas, alternative = alternative)
  #   K <- zo^2/zalphas^2
  #   denom <- d^2*K - 1/(K-1)
  #   if (zalphas > zo) {
  #     warning(paste("Replication success is not achievable at this level as",
  #                   zo, " < ", round(p2z(levelSceptical(level = level,
  #                                                       alternative = alternative,
  #                                                       type = type)),
  #                                    3)))
  #     c <- NA
  #   } else {
  #     c <- ifelse(denom > 0, 1/denom, NA)
  #   }
  # }
  return(c)
}


sampleSizeReplicationSuccessNum  <- Vectorize(.sampleSizeReplicationSuccessNum_)






.sampleSizeReplicationSuccess_ <- function(
    zo,
    power = NA,
    # d = NA,
    level = 0.025,
    alternative = c("one.sided", "two.sided"),
    type = c("golden", "nominal", "controlled"),
    designPrior = c("conditional", "predictive", "EB"),
    shrinkage = 0,
    h = 0) {

    stopifnot(is.numeric(zo),
              length(zo) == 1,
              is.finite(zo))

    stopifnot(length(power) == 1
              # length(d) == 1
              )
    # if (is.na(d) && is.na(power))  stop("either 'power' or 'd' has to be specified")
    # if (!is.na(d) && !is.na(power))  stop("only one of 'power' or 'd' has to be specified")
    # if (!is.na(d)) {
    #     stopifnot(is.numeric(d),
    #               is.finite(d))
    # } else { #!is.na(power)
        stopifnot(is.numeric(power),
                  0 < power, power < 1)
    # }

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
              0 <= h,

              level < power)

    if (type != "controlled") {
        ## computing some quantities
        zoabs <- abs(zo)
        alphaS <- levelSceptical(level = level, alternative = alternative,
                                 type = type)
        zalphaS <- p2z(p = alphaS, alternative = alternative)
        k <- zoabs^2 / zalphaS^2

        ## if zoabs < zalphaS, replication success impossible
        if (zoabs < zalphaS) {
            warning(paste("Replication success at level", signif(level, 3),
                          "impossible for |zo| <", round(zalphaS, 3)))
            c <- NaN
        } else {

            ## sample size calculation based on power
            # if (is.na(d)) {

            ## computing power quantile
            u <- stats::qnorm(p = power)

            ## determining parameters based on design prior
            if (designPrior == "conditional") {
                s <- shrinkage # minor: in some functions (powerReplicationSuccess eg) we have s <- 1 - shrinkage
                H <- 0
            } else if (designPrior == "predictive") {
                s <- shrinkage
                H <- 1 + 2 * h
            } else { ## designPrior == "EB"
                ## computing empirical Bayes shrinkage factor
                s <- pmin((1 + h) / zoabs^2, 1)
                H <- 1 - s + 2 * h - s * h
            }

            ## checking whether power > powerlimit
            if (designPrior != "conditional") {
                powLim <- stats::pnorm(
                    q = (1 - s) * zoabs,
                    mean = zalphaS / sqrt(k - 1),
                    sd = sqrt(H)
                )
            } else { ## for conditional more complicated
                zlim <- zalphaS / sqrt(k - 1)
                if (zoabs * (1 - s) > zlim) {
                    powLim <- 1
                } else if (isTRUE(all.equal(zoabs * (1 - s), zlim, tolerance = 1e-5))) {
                    powLim <- 0.5
                } else {
                    ## power-curve is non-monotone with a maximum...
                    cmax <- pmax(
                        (k - 1) / (zalphaS^2 / (k - 1) / (1 - s)^2 / zoabs^2 - 1),
                        0,
                        na.rm = TRUE
                    )
                    powLim <- powerReplicationSuccess(
                        zo = zoabs,
                        c = cmax,
                        level = level,
                        designPrior = "conditional",
                        alternative = alternative,
                        type = type,
                        shrinkage = shrinkage,
                        h = h
                    )
                }
            }
            if (power > powLim) {
                c <- NaN
                warning(paste(designPrior, "power cannot be larger than",
                              round(powLim, 3), "for supplied input"))
            } else {

                ## solving (quadratic) equation
                A <- 1 / k - u^2 / zoabs^2
                B <- -2 * (1 - s) / sqrt(k)
                C <- (1 - s)^2 - u^2 / zoabs^2 * (H - 1 / (k - 1))

                ## check whether quadratic term cancels
                if (isTRUE(all.equal(A, 0, tolerance = 1e-5))) {
                    res <- 1 / (C^2 / B^2 - 1 / (k - 1))
                } else {
                    ## select correct solution
                    if (power > 0.5) {
                        x <- 0.5 * (-B - sqrt(B^2 - 4 * A * C)) / A
                    } else {
                        x <- 0.5 * (-B + sqrt(B^2 - 4 * A * C)) / A
                    }
                    res <- 1 / (x^2 - 1 / (k - 1))
                }

                ## relative variances need to be positive
                if (is.na(res) || res < 0) {
                    c <- NaN
                } else {
                    c <- res
                }
            }

        # } else {  ## sample size calculation based on relative effect size
        #     denom <- d^2*k - 1/(k - 1)
        #     if (denom > 0) {
        #         c <- 1/denom
        #     } else {
        #         c <- NaN
        #     }
        # }
        }
    }

    if (type == "controlled") {
        # here put the numerical integration
        stopifnot(level < power)
        c <-  sampleSizeReplicationSuccessNum(
            zo = zo, power = power,
            level = level,
            alternative = alternative,
            type = "controlled",
            designPrior = designPrior,
            shrinkage = shrinkage
        )
    }

    return(c)
}

#' Computes the required relative sample size to achieve replication success
#' with the sceptical p-value
#'
#' The relative sample size to achieve replication success is computed based on
#' the z-value of the original study,  the type of
#' recalibration, the power and the design prior.
#' @param zo Numeric vector of z-values from original studies.
#' @param power The power to achieve replication success.
#' @param level Threshold for the calibrated sceptical p-value.
#'     Default is 0.025.
#' @param alternative Specifies if \code{level} is "one.sided" (default) or
#'     "two.sided". If "one.sided" then sample size calculations are based
#'     on a one-sided assessment of replication success in the direction of the
#'     original effect estimates.
#' @param type Type of recalibration. Can be either "golden" (default),
#'     "nominal" (no recalibration), or "controlled". "golden" ensures that for
#'     an original study just significant at the specified \code{level},
#'     replication success is only possible for replication effect estimates
#'     larger than the original one. "controlled" ensures exact overall Type-I
#'     error control at level \code{level}^2.
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
#'     the internal function \code{.sampleSizeReplicationSuccess_}.
#'     \code{\link[base]{Vectorize}} is used to vectorize the function.
#' @references
#' Held, L. (2020). A new standard for the analysis and design of replication
#' studies (with discussion). \emph{Journal of the Royal Statistical Society:
#' Series A (Statistics in Society)}, \bold{183}, 431-448.
#' \doi{10.1111/rssa.12493}
#'
#' Held, L., Micheloud, C., Pawel, S. (2022). The assessment of replication
#'     success based on relative effect size. \emph{The Annals of Applied
#' Statistics}. 16:706-720. \doi{10.1214/21-AOAS1502}
#'
#' Micheloud, C., Balabdaoui, F., Held, L. (2023). Assessing replicability
#' with the sceptical p-value: Type-I error control and
#' sample size planning. \emph{Statistica Neerlandica}. \doi{10.1111/stan.12312}
#'
#' @author Leonhard Held, Charlotte Micheloud, Samuel Pawel, Florian Gerber
#' @seealso \code{\link{pSceptical}}, \code{\link{powerReplicationSuccess}},
#'     \code{\link{levelSceptical}}
#' @examples
#' ## based on power
#' sampleSizeReplicationSuccess(zo = p2z(0.0025), power = 0.8, level = 0.025,
#'                              type = "golden")
#' sampleSizeReplicationSuccess(zo = p2z(0.0025), power = 0.8, level = 0.025,
#'                              type = "golden", designPrior = "predictive")
#' @export
sampleSizeReplicationSuccess <- Vectorize(.sampleSizeReplicationSuccess_)
