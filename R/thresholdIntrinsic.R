#' Computes the p-value threshold for intrinsic credibility
#'
#' @param alpha Numeric vector of intrinsic credibility levels.
#' @param alternative Either "two.sided" (default) or "one.sided".
#' Specifies if the threshold is for one-sided or two-sided p-values.
#' @param type Either "Held" (default) or "Matthews".
#' Type of intrinsic p-value threshold, see Held (2019) and Matthews (2018) for more information.
#' @return The threshold for intrinsic credibility. 
#' @references
#' Matthews, R. A. J. (2018). Beyond 'significance': principles and
#' practice of the analysis of credibility. \emph{Royal Society Open
#' Science}, \bold{5}, 171047. \doi{10.1098/rsos.171047}
#'
#' Held, L. (2019). The assessment of intrinsic credibility and a new argument
#' for \emph{p < 0.005}. \emph{Royal Society Open Science}, \bold{6}, 181534.
#' \doi{10.1098/rsos.181534}
#' @author Leonhard Held
#' @examples
#' thresholdIntrinsic(alpha = c(0.005, 0.01, 0.05))
#' thresholdIntrinsic(alpha = c(0.005, 0.01, 0.05), alternative = "one.sided")
#' @export
thresholdIntrinsic <- function(alpha, 
                               alternative = c("two.sided", "one.sided"), 
                               type = c("Held", "Matthews")){

    stopifnot(is.numeric(alpha),
              length(alpha) > 0,
              is.finite(alpha),
              0 < alpha, alpha <= 1,

              !is.null(alternative))
    alternative <- match.arg(alternative)

    stopifnot(!is.null(type))
    type <- match.arg(type)

    z <- p2z(p = alpha, alternative = alternative)
    if(type == "Held"){
        result <- z2p(z = sqrt(2)*z, alternative = alternative)
    } else{ ## type == "Matthews"
        result <- z2p(z = sqrt(2)*z/sqrt(sqrt(5) - 1), alternative = alternative)
    }
    return(result)
}
