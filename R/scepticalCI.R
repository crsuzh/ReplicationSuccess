pLimit <- function(z1, z2){
  W <- abs(z1*z2)
  target <- function(t, W)
    exp(-W/sqrt(t))/sqrt(t*(1-t))
  res <- integrate(target, lower=0, upper=1, W=W)$value
  return(res/pi)
}
PLimit <- Vectorize(pLimit)

pValueMu <- function(thetahat, se, mu = 0, c = 1, 
                         alternative = c("two.sided", "greater", "less"), 
                         bound = FALSE) 
{
  stopifnot(is.numeric(thetahat), length(thetahat) > 0, is.finite(thetahat), 
            is.numeric(se), length(se) == 1 || length(se) == length(thetahat), 
            is.finite(se), min(se) > 0, c >= 0, is.numeric(mu), 
            length(mu) > 0, is.finite(mu), !is.null(alternative))
  alternative <- match.arg(alternative)
  stopifnot(is.logical(bound), length(bound) == 1, is.finite(bound))
  n <- length(thetahat)
  stopifnot((length(thetahat)==2) & (length(se)==2))
  m <- length(mu)
  z <- (thetahat - mu)/se
  if(c==Inf)
    res <- PLimit(z[1], z[2])
  else{
    zS <- zSceptical(zo=z[1], zr=z[2], c=c)
    res <- (1-FZ(zS^2, c=c))
  }
  check_greater <- min(z) >= 0
  check_less <- max(z) <= 0
  break_p <- 1/(2^n)
  if (alternative == "greater") {
    if (bound) 
      res <- if (check_greater) 
        res/(2^n)
    else paste(">", format(break_p, scientific = FALSE))
    else res <- if (check_greater) 
      res/(2^n)
    else 1 - 2 * sqrt(res/(2^n)) + res/(2^n) ## NaN
  }
  if (alternative == "less") {
    if (bound) 
      res <- if (check_less) 
        res/(2^n)
    else paste(">", format(break_p, scientific = FALSE))
    else res <- if (check_less) 
      res/(2^n)
    else 1 - 2 * sqrt(res/(2^n)) + res/(2^n) ## NaN
  }
  return(res)
}


#' Computes the sceptical confidence interval
#'
#' Computes the sceptical confidence interval based on the effect estimates 
#' and standard errors of the original and the replication study.
#' @rdname scepticalCI
#' @param thetao Effect estimate of the original study
#' @param thetar Effect estimate of the replication study
#' @param seo Standard error of thetao
#' @param ser Standard error of thetar
#' @param c Default is variance ratios of the original and replication
#' effect estimates. This is usually the ratio of the sample
#' size of the replication study to the sample size of the
#' original study. It is also possible to choose c as a free parameter,
#' not related to the variance ratio.
#' @param levelCI Level of the confidence interval.
#' @param alternative Either "two.sided" (default), "greater" or "less".
#' Specifies the type of the confidence interval and of the confidence level.
#' @return \code{scepticalCI} returns the sceptical confidence interval and 
#' the minimum p-value of the p-value function between the original 
#' and replication effect estimates. The confidence interval splits 
#' into two if the minimum p-value is smaller than 1 - \code{levelCI}.
#' @details If the two-sided levelCI-confidence interval does not include 0, 
#' the two-sided controlled sceptical p-value will be smaller than sqrt(1 - levelCI).
#' If their is conflict between the original and the replication effect 
#' estimates, the confidence region splits into two disjoint intervals.
#' @author Leonhard Held, Florian Gerber, Charlotte Micheloud
#' @examples
#' scepticalCI(thetao = 2, thetar = 1.9, seo = 1, ser = 0.5, alternative = "two.sided", levelCI = 0.95)
#' @export
#' 
scepticalCI <- function(thetao, thetar, seo, ser, 
                              c = NA, levelCI = 0.95,
                              alternative = c("two.sided", "greater", "less"))
  
{
  thetahat <-  c(thetao, thetar)
  se <- c(seo, ser)
  c <- ifelse(is.na(c), seo^2/ser^2, c)
  stopifnot(is.numeric(thetahat), length(thetahat) > 0, is.finite(thetahat))
  stopifnot(is.numeric(se), length(se) == 1 || length(se) == 
              length(thetahat), is.finite(se), min(se) > 0, 
            !is.null(alternative))
  alternative <- match.arg(alternative)
  stopifnot(is.numeric(levelCI), length(levelCI) == 1, is.finite(levelCI), 
            0 < levelCI, levelCI < 1)
  target <- function(limit) {
    pValueMu(thetahat = thetahat, se = se, c = c, mu = limit, 
                 alternative = alternative, bound = FALSE) - alpha
  }
  indOrd <- order(thetahat)
  thetahat <- thetahat[indOrd]
  se <- se[indOrd]
  thetahatUnique <- unique(thetahat)
  nThetahatUnique <- length(thetahatUnique)
  mini <- which.min(thetahat)
  maxi <- which.max(thetahat)
  mint <- thetahat[mini]
  maxt <- thetahat[maxi]
  minse <- se[mini]
  maxse <- se[maxi]
  alpha <- 1 - levelCI
  z1 <- max(-qnorm(alpha), 1)
  eps <- 1e-06
  factor <- 5
  if (alternative == "two.sided") {
    lower <- mint - z1 * minse
    while (target(lower) > 0) lower <- lower - minse
    CIlower <- uniroot(f = target, lower = lower, upper = thetahat[1])$root
    CImiddle <- matrix(NA, nrow = 2, ncol = nThetahatUnique - 
                         1)
    gam <- matrix(NA, nrow = nThetahatUnique - 1, ncol = 2)
    colnames(gam) <- c("minimum", "pvalue_fun/gamma")
    if(nThetahatUnique != 1){
      for (i in 1:(nThetahatUnique - 1)) {
        opt <- optimize(f = target, lower = thetahatUnique[i], 
                        upper = thetahatUnique[i + 1])
        gam[i, ] <- c(opt$minimum, opt$objective + alpha)
        if (opt$objective <= 0) {
          CImiddle[1, i] <- uniroot(f = target, lower = thetahatUnique[i], 
                                    upper = opt$minimum)$root
          CImiddle[2, i] <- uniroot(f = target, lower = opt$minimum, 
                                    upper = thetahatUnique[i + 1])$root
        }
      }
    }
    CImiddle <- CImiddle[!is.na(CImiddle)]
    upper <- maxt + maxse
    while (target(upper) > 0) upper <- upper + z1 * maxse
    CIupper <- uniroot(f = target, lower = thetahat[length(thetahat)], 
                       upper = upper)$root
    CI <- matrix(c(CIlower, CImiddle, CIupper), ncol = 2, 
                 byrow = TRUE)
    colnames(CI) <- c("lower", "upper")
    
      return(list(CI = CI, minP = gam))
  }
  if (alternative == "greater") {
    lower <- uniroot(f = target, lower = mint - factor * 
                       z1 * minse, upper = mint - eps * minse)$root
    upper <- Inf
    return(list(CI = cbind(lower, upper)))
  }
  if (alternative == "less") {
    lower <- -Inf
    upper <- uniroot(f = target, lower = maxt + eps * maxse, 
                     upper = maxt + factor * z1 * maxse)$root
    return(list(CI = cbind(lower, upper)))
  }
  stop("function not get here.")
}


