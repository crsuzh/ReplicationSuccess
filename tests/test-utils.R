library(ReplicationSuccess)

## Functions to check for errors
## ------------------------------------------------------------------
checkError <- function(expr) {
  inherits(try(expr = expr, silent = TRUE), "try-error")
}

checkNum <- function(x, y) {
  stopifnot(isTRUE(all.equal.numeric(x, y)))
}

checkNumTol <- function(x, y, tol = .Machine$double.eps) {
  stopifnot(isTRUE(all.equal.numeric(x, y, 
                                     tolerance = tol,
                                     check.attributes = FALSE)))
}

## Check that wrong inputs throw errors
## ------------------------------------------------------------------

## p2z
checkError(p2z(p = 0))
checkError(p2z(p = 1.1))
checkError(p2z(p = 0.05, alternative = "wrong"))

## z2p 
checkError(z2p(z = "wrong"))
checkError(z2p(z = 1, alternative = "wrong"))

## ci2se
checkError(ci2se(lower = c(1, 2), upper = c(1, 2, 3)))
checkError(ci2se(lower = c(3, 3), upper = c(2, 1)))
checkError(ci2se(lower = c(3, 3), upper = c(2, 1), conf.level = 2))
checkError(ci2se(lower = 0, upper = 1, ratio = TRUE))

## ci2estimate
checkError(ci2estimate(lower = c(1, 2), upper = c(1, 2, 3)))
checkError(ci2estimate(lower = c(3, 3), upper = c(2, 1)))
checkError(ci2estimate(lower = 0, upper = 1, ratio = TRUE))

## ci2z
checkError(ci2z(lower = c(1, 2), upper = c(1, 2, 3)))
checkError(ci2z(lower = c(3, 3), upper = c(2, 1)))
checkError(ci2z(lower = c(3, 3), upper = c(2, 1), conf.level = 2))
checkError(ci2z(lower = 0, upper = 1, ratio = TRUE))

## ci2p
checkError(ci2p(lower = c(1, 2), upper = c(1, 2, 3)))
checkError(ci2p(lower = c(3, 3), upper = c(2, 1)))
checkError(ci2p(lower = c(3, 3), upper = c(2, 1), conf.level = 2))
checkError(ci2p(lower = 0, upper = 1, ratio = TRUE))
checkError(ci2p(lower = 0, upper = 1, alternative = "wrong"))
