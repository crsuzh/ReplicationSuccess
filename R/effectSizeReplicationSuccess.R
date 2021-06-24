#' Computes the minimum relative effect size to achieve replication success
#'
#' The minimum relative effect size (replication to original) to achieve replication success
#' is computed based on the result of the original study and the corresponding variance ratio (relative sample size).
#' 
#' @param zo  Numeric vector of z-values from original studies. 
#' @param c Numeric vector of variance ratios of the original and replication effect estimates.
#' This is usually the ratio of the sample size of the replication study to the sample
#' size of the original study.
#' @param level Replication success significance level. Default is 0.025.
#' @param alternative Specifies if the replication success level is "one.sided" (default) or "two.sided".
#' If the significance level is one-sided, then effect size calculations are based on a one-sided assessment of
#' replication success in the direction of the original effect estimate.
#' @param type  Type of recalibration. Can be either "golden" (default), "nominal" (no recalibration),
#' "liberal", or "controlled". "golden" ensures that for an original study just significant at
#' the specified \code{level}, replication success is only possible for replication effect estimates larger than the original one.
#' See \code{\link{levelSceptical}} for details about recalibration types.
#' @return The minimum relative effect size to achieve replication success.
#' @references Held, L., Micheloud, C. & Pawel, S. (2020).
#' The assessment of replication success based on relative effect size. \url{https://arxiv.org/abs/2009.07782}
#' @author Leonhard Held, Charlotte Micheloud
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
#' effectSizeReplicationSuccess(zo = zo, c= 2, level = 0.025,
#'                              alternative = "one.sided", type = "nominal")
#'
#' effectSizeReplicationSuccess(zo = zo, c = 2, level = 0.05,
#'                              alternative = "two.sided", type = "nominal")
#' @export
effectSizeReplicationSuccess <- function(zo, 
                                         c = 1, 
                                         level = 0.025, 
                                         alternative = "one.sided",
                                         type = "golden"){
  
  mRV <- mapply(FUN = function(zo, c, level){
    if (!is.numeric(level) || (level <= 0 || level >= 1)) 
      stop("level must be numeric and in (0,1)!")
    
    alphas <- levelSceptical(level = level, 
                             alternative = alternative, 
                             type = type)
    zalphas <- p2z(alphas, alternative = alternative)
    K <- zo^2/zalphas^2
    
    if (zalphas > zo) {
      warning(paste("Replication success is not achievable at this level as",
                    zo, " < ", round(p2z(levelSceptical(level = level,
                                                            alternative = alternative,
                                                            type = type)),
                                     3)))
      d <- NA
    } else {
    d <- ifelse(c < Inf, sqrt(1 + c/(K - 1))/(sqrt(K * c)), 1/sqrt(K * (K - 1)))
    }
    return(d)
  }, zo, c, level)
  
  return(mRV)
}
