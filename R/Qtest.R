#' Q-test to assess compatibility between original and replication effect estimate
#'
#' Computes p-value from meta-analytic Q-test to assess compatibility between
#' original and replication effect estimate.
#' @param thetao Numeric vector of effect estimates from original studies.
#' @param thetar Numeric vector of effect estimates from replication studies.
#' @param seo Numeric vector of standard errors of the original effect estimates.
#' @param ser Numeric vector of standard errors of the replication effect estimates.
#' @return p-value from Q-test.
#' @details This function computes the p-value from a meta-analytic Q-test assessing
#' compatibility between original and replication effect estimate. Rejecting
#' compatibility when the p-value is smaller than alpha is equivalent with
#' rejecting compatibility based on a (1 - alpha) prediction interval.
#' @references
#' Hedges, L. V., Schauer, J. M. (2019). More Than One Replication Study Is
#' Needed for Unambiguous Tests of Replication. \emph{Journal of Educational and
#' Behavioral Statistics}, \bold{44}, 543-570.
#' \doi{10.3102/1076998619852953}
#' @seealso \code{\link{predictionInterval}}
#' @author Samuel Pawel
#' @examples
#' Qtest(thetao = 2, thetar = 0.5, seo = 1, ser = 0.5)
#' @import stats
#' @export
Qtest <- function(thetao, thetar, seo, ser) {
    stopifnot(is.numeric(thetao),
              length(thetao) > 0,
              is.finite(thetao),

              is.numeric(thetar),
              length(thetar) > 0,
              is.finite(thetar),
              
              is.numeric(seo),
              length(seo) > 0,
              is.finite(seo),
              0 < seo,  
              
              is.numeric(ser),
              length(ser) > 0,
              is.finite(ser),
              0 < ser)

    Q <- (thetao - thetar)^2/(seo^2 + ser^2)
    stats::pchisq(q = Q, df = 1, ncp = 0, lower.tail = FALSE)
}
