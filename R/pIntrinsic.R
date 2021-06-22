#' Computes the p-value for intrinsic credibility
#'
#' @param p numeric vector of p-values.
#' @param z numeric vector of z-values. Default is \code{NULL}.
#' @param alternative  Either "two.sided" (default) or "one.sided".
#' Specifies if the p-value is two-sided or one-sided.
#' If the p-value is one-sided, then a one-sided p-value for
#' intrinsic credibility is computed.
#' @param type   Type of intrinsic p-value. Default is \code{"Held"} as in
#' Held (2019). The other option is \code{"Matthews"} as in Matthews (2018).
#' @return p-values for intrinsic credibility.
#' @references Matthews, R. A. J. (2018).  Beyond 'significance': principles and practice of the analysis of credibility.
#' \emph{Royal Society Open Science}, 5:171047.
#' \url{https://doi.org/10.1098/rsos.171047}
#'
#' Held, L. (2019). The assessment of intrinsic credibility and a new argument for p < 0.005.
#' \emph{Royal Society Open Science}, 6:181534. \url{https://dx.doi.org/10.1098/rsos.181534}
#' @author Leonhard Held
#' @examples
#' p <- c(0.005, 0.01, 0.05)
#' pIntrinsic(p = p)
#' pIntrinsic(p = p, type = "Matthews")
#' pIntrinsic(p = p, alternative = "one.sided")
#' pIntrinsic(p = p, alternative = "one.sided", type = "Matthews")
#'
#' pIntrinsic(z = 2)
#' @export
pIntrinsic <- function(p = z2p(z, alternative = alternative), z = NULL,
                       alternative = "two.sided", type = "Held"){
    if(type == "Held"){
        iz <- p2z(p, alternative = alternative)/sqrt(2)
        iP <- z2p(z = iz, alternative = alternative)
    }
    if(type == "Matthews"){
        iz <- p2z(p, alternative = alternative)/sqrt(2)*sqrt(sqrt(5) - 1)
        iP <- z2p(z = iz, alternative = alternative)
    }
    return(iP)
}
