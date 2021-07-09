#' @export
.effectSizeReplicationSuccess_ <- function(zo, 
                                           c = 1, 
                                           level = 0.025, 
                                           alternative = c("one.sided", "two.sided"),
                                           type = c("golden", "nominal", "liberal", "controlled")){
    
    stopifnot(is.numeric(zo),
              length(zo) == 1,
              is.finite(zo),
                            
              is.numeric(c),
              length(c) == 1,
              !is.na(c), !is.nan(c),
              0 <= c, 
              
              is.numeric(level),
              length(level) == 1,
              is.finite(level),
              0 < level, level < 1,
              
              !is.null(alternative))
    alternative <- match.arg(alternative)
    
    stopifnot(!is.null(type))
    type <- match.arg(type)
    
    alphas <- levelSceptical(level = level, 
                             alternative = alternative, 
                             type = type)
    zalphas <- p2z(p = alphas, alternative = alternative)
    K <- zo^2 / zalphas^2
    
    if (zalphas > zo) {
      warning(paste("Replication success is not achievable at this level as |zo| =",
                    abs(round(zo, 2)), " < ", round(p2z(levelSceptical(level = level,
                                                        alternative = alternative,
                                                        type = type)), 3)))
      d <- NA
    } else {
        d <- if(c < Inf) sqrt(1 + c/(K - 1))/(sqrt(K * c)) else 1/sqrt(K * (K - 1))
    }
    
    return(d)
}

#' Computes the minimum relative effect size to achieve replication success
#'
#' The minimum relative effect size (replication to original) to achieve replication success
#' is computed based on the result of the original study and the corresponding variance ratio.
#' 
#' @param zo Numeric vector of z-values from original studies. 
#' @param c Numeric vector of variance ratios of the original and replication effect estimates.
#' This is usually the ratio of the sample size of the replication study to the sample
#' size of the original study.
#' @param level Replication success level. Default is 0.025.
#' @param alternative Specifies if the replication success level is "one.sided" (default) or "two.sided".
#' If the replication success level is one-sided, then effect size calculations are based on a one-sided assessment of
#' replication success in the direction of the original effect estimate.
#' @param type Type of recalibration. Can be either "golden" (default), "nominal" (no recalibration),
#' "liberal", or "controlled". "golden" ensures that for an original study just significant at
#' the specified \code{level}, replication success is only possible for replication effect estimates larger than the original one.
#' See \code{\link{levelSceptical}} for details about recalibration types.
#' @return The minimum relative effect size to achieve replication success.
#' @details \code{effectSizeReplicationSuccess} is the vectorized version of \code{.effectSizeReplicationSuccess_}.
#' \code{\link[base]{Vectorize}} is used to vectorize the function.
#' @references
#' Held, L., Micheloud, C., Pawel, S. (2021). The assessment of replication
#' success based on relative effect size. \url{https://arxiv.org/abs/2009.07782}
#' @author Leonhard Held, Charlotte Micheloud, Samuel Pawel, Florian Gerber
#' @seealso \code{\link{sampleSizeReplicationSuccess}}, \code{\link{levelSceptical}}
#' @examples
#' po <- c(0.001, 0.002, 0.01, 0.02, 0.025)
#' zo <- p2z(po, alternative = "one.sided")
#'
#' effectSizeReplicationSuccess(zo = zo, c = 1, level = 0.025,
#'                              alternative = "one.sided", type = "golden")
#'                              
#' effectSizeReplicationSuccess(zo = zo, c = 10, level = 0.025,
#'                              alternative = "one.sided", type = "golden")
#'                              
#' effectSizeReplicationSuccess(zo = zo, c= 2, level = 0.025,
#'                              alternative = "one.sided", type = "nominal")
#'                              
#' effectSizeReplicationSuccess(zo = zo, c = 2, level = 0.05,
#'                              alternative = "two.sided", type = "nominal")
#' @export
effectSizeReplicationSuccess <- Vectorize(.effectSizeReplicationSuccess_)
