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
checkError(sampleSizeReplicationSuccess(zo = 1, power = 0.8, 
                                        designPrior = "wrong"))
checkError(sampleSizeReplicationSuccess(zo = 1, power = -1))
checkError(sampleSizeReplicationSuccess(zo = 1, power = 2))
checkError(sampleSizeReplicationSuccess(zo = 1, power = 0.8, level = -1))
checkError(sampleSizeReplicationSuccess(zo = 1, power = 0.8, level = 2))
checkError(sampleSizeReplicationSuccess(zo = 1, power = 0.8, 
                                        alternative = "wrong"))

## Check numerically some results 
## ------------------------------------------------------------------
za <- qnorm(p = 0.001/2, lower.tail = FALSE)
checkNumTol(sampleSizeReplicationSuccess(zo = za, designPrior = "conditional",
                                         power = 0.8, level = 0.05,
                                         alternative = "two.sided",
                                         type = "nominal"),
            1, tol = 0.01)

## Apply over a grid of values
## ------------------------------------------------------------------
zo <- seq(-4, 4, 2)
apply_grid <- expand.grid(priors = c("conditional", "predictive"),
                          alt = c("one.sided", "two.sided"),
                          stringsAsFactors = FALSE)
for (i in seq(1, nrow(apply_grid))) {
  print(apply_grid[i,])
  c <- sampleSizeReplicationSuccess(zo = zo, 
                                    power = 0.8, 
                                    level = 0.05,
                                    designPrior = apply_grid$priors[i],
                                    alternative = apply_grid$alt[i],
                                    type = "nominal")
  print(round(c, digits = 5))
}