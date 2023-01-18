FZ <- function(z, c) {
  if (z <= 0) {
    return(0)
  } else if (c == 0) {
    return(1 - 4*(1 - pnorm(sqrt(z)))^2)
  } else if (c == 1) {
    return(pgamma(z, 1/2, 2))
  } else {
    f <- function(t, c, z) {
      t1 <- exp(-(c - 1)*z/(sqrt(1 + (c - 1)*t) - 1))
      t2 <- 1 / sqrt(t*(1 - t))
      return(t1*t2)
    }
    myint <- integrate(
      f,
      lower = 0,
      upper = 1,
      z = z,
      c = c
    )$value
    result <- 1 - myint/pi
    return(result)
  }
}

#' Computes the cumulative distribution function of z = z_S^2
#'
#' @rdname FZ
#' @param z z_S^2
#' @param c variance ratios of the original and replication effect estimates
#' @return \code{FZ} returns the cdf for a particular c and z.