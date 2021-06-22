#' Bound for the p-values entering the harmonic mean chi-squared test.
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
#' Held, L. (2020). The harmonic mean chi-squared test to substantiate scientific findings.
#' \emph{Journal of the Royal Statistical Society, Series C (Applied Statistics)}, to appear.
#' \url{https://arxiv.org/abs/1911.10633}
#' @author Leonhard Held
#' @seealso \code{\link{hMeanChiSq}}
#' @examples
#' pvalueBound(alpha = 0.025^2, n = 2, type = "necessary")
#' pvalueBound(alpha = 0.025^2, n = 2, type = "sufficient")
#' @export
pvalueBound <- function(alpha, n, type="necessary"){
  if(!(type %in% c("necessary", "sufficient")))
    stop("Incorrect type chosed")
  if(type=="necessary")
      bound <- 1-pnorm(sqrt(cH(alpha, n))/n)
  if(type=="sufficient")
      bound <- 1-pnorm(sqrt(cH(alpha, n)/n))
  return(bound)
}

cH <- function(alpha, n){
    res <- (qnorm(1-2^(n-1)*alpha))^2
    return(res)
}
