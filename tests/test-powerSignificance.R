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
checkError(powerSignificance(zo = 1, c = 1, designPrior = "wrong"))
checkError(powerSignificance(zo = 1, c = -1))
checkError(powerSignificance(zo = 1, c = 1, h = -1))
checkError(powerSignificance(zo = 1, c = 1, shrinkage = -1))
checkError(powerSignificance(zo = 1, c = 1, shrinkage = 2))
checkError(powerSignificance(zo = 1, c = 1, level = -1))
checkError(powerSignificance(zo = 1, c = 1, level = 2))
checkError(powerSignificance(zo = 1, c = 1, alternative = "wrong"))


## Check numerically some results 
## ------------------------------------------------------------------
checkNumTol(powerSignificance(zo = qnorm(p = 1 - 0.05/2),
                              c = 1, level = 0.05,
                              alternative = "two.sided"), 
            0.5, tol = 0.001)
checkNumTol(powerSignificance(zo = qnorm(p = 1 - 0.0056/2),
                              c = 1, level = 0.05,
                              alternative = "two.sided"), 
            0.791, tol = 0.001)

## Apply over a grid of values
## ------------------------------------------------------------------
zo <- seq(-1, 1, 1)
apply_grid <- expand.grid(priors = c("conditional", "predictive", "EB"),
                          c = c(0.5, 2),
                          h = c(0, 1),
                          alt = c("two.sided", "one.sided", "greater"),
                          shrinkage = c(0, 0.5),
                          stringsAsFactors = FALSE)

for (i in seq(1, nrow(apply_grid))) {
  print(apply_grid[i,])
  p <- powerSignificance(zo = zo,
                         c = apply_grid$c[i],
                         level = 0.05,
                         designPrior = apply_grid$priors[i],
                         alternative = apply_grid$alt[i],
                         h = apply_grid$h[i],
                         shrinkage = apply_grid$shrinkage[i])
  print(round(p, digits = 5))
}
