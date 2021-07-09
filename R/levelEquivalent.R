#' Computes the level for the sceptical p-value
#'
#' The equivalent level for the sceptical p-value is computed based on
#' the specified limiting relative effect size, the replication success level, and
#' the alternative. 
#' @param dinf The limiting relative effect size bound below which replication success is
#' deemed to be impossible for borderline significant original studies
#' with p-values equal to \code{level}.
#' @param level Numeric vector of required replication success levels. Defaults to 0.025. 
#' @param alternative Either "one.sided" (default) or "two.sided".
#' Specifies if the replication success level is one-sided or two-sided.
#' If the replication success level is one-sided,
#' then a one-sided level for the sceptical p-value is computed.
#' @return Equivalent levels for the sceptical p-value corresponding to \code{dmin}=1.
#' Thus, at the equivalent level, replication success is impossible for borderline significant original
#' studies and shrinkage of the replication effect estimate. 
#' @references Held, L. (2020). A new standard for the analysis and design of replication studies (with discussion).
#' \emph{Journal of the Royal Statistical Society: Series A (Statistics in Society)}, \bold{183}, 431-448.
#' \doi{10.1111/rssa.12493}
#'
#' Held, L., Micheloud, C., Pawel, S. (2021). The assessment of replication
#' success based on relative effect size. \url{https://arxiv.org/abs/2009.07782}
#' @seealso \code{\link{pSceptical}}, \code{\link{levelSceptical}}
#' @author Leonhard Held
#' @examples
#' levelEquivalent(dinf = 0.8, level = 0.025)
#' levelEquivalent(dinf = 0.8, level = 0.05, alternative="two.sided")
#' @export
levelEquivalent <- function(dinf, level=0.025,
                            alternative=c("one.sided", "two.sided")){
    stopifnot(is.numeric(dinf),
              length(dinf) >= 1,
              is.finite(dinf),
              dinf > 0,
              
              is.numeric(level),
              length(level) >= 1,
              is.finite(level),
              0 < level, level < 1,
              
              !is.null(alternative))
    alternative <- match.arg(alternative)

    zalpha <- p2z(p=level, alternative=alternative)
    K <- 0.5 + sqrt(1/dinf^2 + 1/4)
    phi <- (sqrt(5)+1)/2
    zalphaNew <- zalpha*sqrt(phi/K)
    alphaNew <- z2p(z=zalphaNew, alternative=alternative)
    return(alphaNew)
}
