.sampleSizeReplicationSuccessTarget <- function(zo, c, p, level, designPrior, alternative, type = type,
                   shrinkage){
    zr2 <- zr2quantile(zo = zo, c = c, p = p, designPrior = designPrior, 
                       shrinkage = shrinkage)
    pC <- pSceptical(zo = zo, zr = sqrt(zr2), c = c, 
                     alternative = alternative, type = type)
    return(pC - level)
}

.sampleSizeReplicationSuccess_ <- function(zo,
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
              0 <= shrinkage, shrinkage <= 1)
        
    mylower <- 0
    myupper <- 1000
    
    ## sample size calculation based on power
    if (is.na(d)){
        target.l <- .sampleSizeReplicationSuccessTarget(c = mylower, 
                                                        zo = zo, 
                                                        p = 1 - power, 
                                                        level = level,
                                                        designPrior = designPrior,
                                                        alternative = alternative,
                                                        type = type,
                                                        shrinkage = shrinkage)
        target.u <- .sampleSizeReplicationSuccessTarget(c = myupper, 
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
            c <- uniroot(f = .sampleSizeReplicationSuccessTarget, 
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

#' Computes the required relative sample size to achieve replication success based
#' on power or on the minimum relative effect size
#'
#' The relative sample size to achieve replication success is computed based on the
#' z-value of the original study, the replication success level, the type of
#' recalibration and either the power or the minimum relative effect size.
#' When the approach based on power is used, the design prior also has to be specified.
#' @param zo Numeric vector of z-values from original studies.
#' @param power The power to achieve replication success.
#' @param d The minimum relative effect size (ratio of the effect estimate
#' from the replication study to the effect estimate
#' from the original study) to achieve replication success.
#' @param level Replication success level. Default is 0.025.
#' @param alternative Either "one.sided" (default) or "two.sided".
#' Specifies if the replication success level is one-sided or two-sided.
#' If one-sided, sample size calculations are based on a one-sided assessment of
#' replication success in the direction of the original effect estimate.
#' @param type Type of recalibration. Can be either "golden" (default),
#' "nominal" (no recalibration), "liberal", "controlled".
#' "golden" ensures that for an original study just significant at the specified \code{level},
#' replication success is only possible if the replication effect estimate is at
#' least as large as the original one.
#' See \code{\link{levelSceptical}} for details about recalibration types.
#' @param designPrior Is only taken into account when \code{power} is specified.
#' Either "conditional" (default), "predictive", or "EB".
#' If "EB", the power is computed under a predictive distribution
#' where the contribution of the original study is shrunken towards zero based
#' on the evidence in the original study (with an empirical Bayes shrinkage estimator).
#' @param shrinkage Is only taken into account when \code{power} is specified.
#' A number in [0,1] with default 0.
#' Specifies the shrinkage of the original effect estimate towards zero
#' (e.g., the effect is shrunken by a factor of 25\% for \code{shrinkage = 0.25}).
#' Is only taken into account when the \code{designPrior} is "conditional" or "predictive".
#' @return The relative sample size for replication success.
#' If larger than 1000, \code{Inf} is returned.
#' @references
#' Held, L. (2020). A new standard for the analysis and design of replication studies (with discussion).
#' \emph{Journal of the Royal Statistical Society: Series A (Statistics in Society)}.
#' 183(2):431 - 448. \url{https://doi.org/10.1111/rssa.12493}
#'
#' Held, L., Micheloud, C. & Pawel, S. (2020). The assessment of replication success
#' based on relative effect size. \url{https://arxiv.org/abs/2009.07782}
#' @author Leonhard Held, Charlotte Micheloud
#' seealso \code{\link{pSceptical}}, \code{\link{powerReplicationSuccess}}, \code{\link{levelSceptical}}
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
