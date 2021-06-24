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
#' Needed for Unambiguous Tests of Replication. Journal of Educational and
#' Behavioral Statistics, 44(5), 543-570.
#' \url{https://doi.org/10.3102/1076998619852953}
#' @seealso \code{\link{predictionInterval}}
#' @author Samuel Pawel
#' @examples
#' Qtest(thetao = 2, thetar = 0.5, seo = 1, ser = 0.5)
#' @export
Qtest <- function(thetao, thetar, seo, ser) {
    # vectorize function in all arguments
    pvalQvec <- mapply(FUN = function(thetao, thetar, seo, ser) {
        # sanity checks
        if (!is.numeric(thetao)) 
            stop("thetao must be numeric")
        if (!is.numeric(thetar)) 
            stop("thetar must be numeric")
        if (!is.numeric(seo) || seo <= 0)
            stop("seo must be numeric and larger than 0")
        if (!is.numeric(ser) || ser <= 0)
            stop("ser must be numeric and larger than 0")
        
        # compute Q-test statistic and p-value
        Q <- (thetao - thetar)^2/(seo^2 + ser^2)
        pvalQ <- stats::pchisq(q = Q, df = 1, ncp = 0, lower.tail = FALSE)
        return(pvalQ)
    }, thetao, thetar, seo, ser)
    return(pvalQvec)
}
