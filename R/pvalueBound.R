#' Bound for the p-values entering the harmonic mean chi-squared test
#'
#' Necessary or sufficient bounds for significance of the harmonic mean
#' chi-squared test are computed for n one-sided p-values.
#' @param alpha Numeric vector specifying the significance level.
#' @param n The number of p-values.
#' @param type Either "necessary" (default) or "sufficient".
#' If "necessary", the necessary bounds are computed.
#' If "sufficient", the sufficient bounds are computed.
#' @return The bound for the p-values.
#' @references
#' Held, L. (2020). The harmonic mean chi-squared test to substantiate
#' scientific findings. \emph{Journal of the Royal Statistical Society: Series C
#' (Applied Statistics)}, \bold{69}, 697-708. \doi{10.1111/rssc.12410}
#' @author Leonhard Held
#' @seealso \code{\link{hMeanChiSq}}
#' @examples
#' pvalueBound(alpha = 0.025^2, n = 2, type = "necessary")
#' pvalueBound(alpha = 0.025^2, n = 2, type = "sufficient")
#' @export
pvalueBound <- function(alpha, n, type=c("necessary", "sufficient")){

    stopifnot(is.numeric(alpha),
              length(alpha) > 0,
              is.finite(alpha),

              is.numeric(n),
              length(n) > 0,
              is.finite(n),
              
              !is.null(type))
    type <- match.arg(type)
    
    cH <- function(alpha, n){
        (qnorm(1-2^(n-1)*alpha))^2
    }
    
    if(type=="necessary")
       return(1-pnorm(sqrt(cH(alpha, n))/n))

    ## type=="sufficient"
    1-pnorm(sqrt(cH(alpha, n)/n))
}

