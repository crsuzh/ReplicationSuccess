library(ReplicationSuccess)

## Functions to check for errors and (approximate) numerical equality
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
checkError(pSceptical(zo = 1, zr = 1, c = 1, alternative = "wrong"))
checkError(pSceptical(zo = 1, zr = 1, c = -1))
checkError(pSceptical(zo = 1, zr = 1, alternative = "wrong"))


## Check numerically some results from the paper
## ------------------------------------------------------------------
checkNumTol(pSceptical(zo = sqrt(12.19), zr = sqrt(3.99), c = 1, 
                       alternative = "two.sided", type = "nominal"),
            0.083, tol = 0.01)
checkNumTol(pSceptical(zo = 2.33, zr = 2.33, c = 1, alternative = "one.sided",
                       type = "nominal"),
            0.05, tol = 0.01)


## Apply over a grid of values
## ------------------------------------------------------------------
zo <- seq(-4, 4, 2)
apply_grid <- expand.grid(zr = seq(-4, 4, 2),
                          c = c(0.5, 2),
                          alt = c("one.sided", "two.sided"),
                          stringsAsFactors = FALSE)
for (i in seq(1, nrow(apply_grid))) {
  print(apply_grid[i,])
  c <- pSceptical(zo = zo, 
                  zr = apply_grid$zr[i], 
                  c = 0.05,
                  alternative = apply_grid$alt[i],
                  type = "nominal")
  print(round(c, digits = 5))
}
