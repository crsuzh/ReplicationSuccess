#' @export
.effectSizeSignificance_ <- function(zo, 
                                     c = 1, 
                                     level = 0.025, 
                                     alternative = c("one.sided", "two.sided")){
    stopifnot(is.numeric(zo),
              length(zo) == 1,
              is.finite(zo),
                            
              is.numeric(c),
              length(c) == 1,
              is.finite(c),
              
              is.numeric(level),
              length(level) == 1,
              is.finite(level),
              0 < level, level < 1,
              
              !is.null(alternative))
    alternative <- match.arg(alternative)

    zalpha <- z2p(level, alternative = alternative)
    d <- zalpha / (zo * sqrt(c))
    return(d)
}

#' Computes the minimum relative effect size to achieve significance of the replication study
#'
#' The minimum relative effect size (replication to original) to achieve significance
#' of the replication study is computed based on the result of the original study and
#' the corresponding variance ratio.
#'
#' @param zo Numeric vector of z-values from original studies. 
#' @param c Numeric vector of variance ratios of the original and replication effect estimates.
#' This is usually the ratio of the sample size of the replication study to the sample
#' size of the original study.
#' @param level Significance level. Default is 0.025.
#' @param alternative Specifies if the replication success level is "one.sided" (default) or "two.sided".
#' @return The minimum relative effect size to achieve significance of the replication study.
#' @details \code{effectSizeSignificance} is the vectorized version of \code{.effectSizeSignificance_}.
#' \code{\link[base]{Vectorize}} is used to vectorize the function.
#' @references Held, L., Micheloud, C. & Pawel, S. (2020).
#' The assessment of replication success based on relative effect size. \url{https://arxiv.org/abs/2009.07782}
#' @author Charlotte Micheloud
#' @seealso \code{\link{effectSizeReplicationSuccess}}
#' @examples
#' po <- c(0.001, 0.002, 0.01, 0.02, 0.025)
#' zo <- p2z(po, alternative = "one.sided")
#'
#' effectSizeSignificance(zo = zo, c = 1, level = 0.025,
#'                        alternative = "one.sided")
#'
#' effectSizeSignificance(zo = zo, c = 1, level = 0.05,
#'                        alternative = "two.sided")
#'
#' effectSizeSignificance(zo = zo, c = 50, level = 0.025,
#'                        alternative = "one.sided")
#' @export
effectSizeSignificance <- Vectorize(.effectSizeSignificance_)
