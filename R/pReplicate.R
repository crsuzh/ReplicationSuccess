#' Probability of replicating an effect of Killeen (2005)
#'
#' Computes the probability that a replication study yields an effect
#' estimate in the same direction as in the original study. 
#'
#' @param po Numeric vector of p-values from the original study, default is \code{NULL}.
#' @param zo Numeric vector of z-values from the original study.
#' Is calculated from \code{po}, if necessary. 
#' @param c The ratio of the variances of the original and replication effect estimates.
#' This is usually the ratio of the sample size of the replication study to the sample
#' size of the original study.
#' @param alternative Either "two.sided" (default) or "one.sided".
#' Specifies whether the p-value is two-sided or one-sided.
#' @return The probability that a replication study yields an effect
#' estimate in the same direction as in the original study.
#' @details This extends the statistic p_rep ("the probability of replicating an effect")
#' by Killeen (2005) to the case of possibly unequal sample sizes, see also Senn (2002).
#' @references
#' Killeen, P. R. (2005). An alternative to null-hypothesis significance
#' tests. \emph{Psychological Science}, \bold{16}, 345--353. \doi{10.1111/j.0956-7976.2005.01538.x}
#'
#' Senn, S. (2002). Letter to the Editor, \emph{Statistics in Medicine}, \bold{21}, 2437--2444.
#'
#' Held, L. (2019). The assessment of intrinsic credibility and a new argument
#' for \emph{p < 0.005}. \emph{Royal Society Open Science}, \bold{6}, 181534.
#' \doi{10.1098/rsos.181534}
#' @author Leonhard Held
#' @examples
#' pReplicate(po = c(0.05, 0.01, 0.001), c = 1)
#' pReplicate(po = c(0.05, 0.01, 0.001), c = 2)
#' pReplicate(po = c(0.05, 0.01, 0.001), c = 2, alternative = "one.sided")
#' pReplicate(zo = c(2, 3, 4), c = 1)
#' @export
pReplicate <- function(po = NULL, 
                       zo = p2z(p = po, alternative = alternative),
                       c,
                       alternative = "two.sided"){
    ## 'po' and 'alternative' are checked in p2z()

    stopifnot(is.numeric(zo),
              length(zo) > 0,
              is.finite(zo),
              
              is.numeric(c),
              length(c) > 0,
              is.finite(c),
              0 <= c)
    
    pRep <- pnorm(q = zo / sqrt(1 + 1/c))
    return(pRep)
}

