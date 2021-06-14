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
checkError(sampleSizeSignificance(zo = 1, power = 0.8, designPrior = "wrong"))
checkError(sampleSizeSignificance(zo = 1, power = -1))
checkError(sampleSizeSignificance(zo = 1, power = 2))
checkError(sampleSizeSignificance(zo = 1, power = 0.8, level = -1))
checkError(sampleSizeSignificance(zo = 1, power = 0.8, level = 2))
checkError(sampleSizeSignificance(zo = 1, power = 0.8, h = -1))
checkError(sampleSizeSignificance(zo = 1, power = 0.8, shrinkage = -1))
checkError(sampleSizeSignificance(zo = 1, power = 0.8, shrinkage = 2))
checkError(sampleSizeSignificance(zo = 1, power = 0.8, alternative = "wrong"))

## Check numerically some results 
## ------------------------------------------------------------------
za <- qnorm(p = 0.025, lower.tail = FALSE)
checkNumTol(sampleSizeSignificance(zo = za, designPrior = "conditional",
                                   power = 0.8, alternative = "one.sided"),
            2.04, tol = 0.01)

## Apply over a grid of values
## ------------------------------------------------------------------
zo <- seq(-4, 4, 2)
apply_grid <- expand.grid(priors = c("conditional", "predictive", "EB"),
                          h = c(0, 0.1),
                          shrinkage = c(0, 0.75),
                          alt = c("greater", "two.sided"),
                          stringsAsFactors = FALSE)
for (i in seq(1, nrow(apply_grid))) {
  print(apply_grid[i,])
  c <- sampleSizeSignificance(zo = zo, 
                              power = 0.8, 
                              level = 0.05,
                              designPrior = apply_grid$priors[i],
                              alternative = apply_grid$alt[i],
                              h = apply_grid$h[i],
                              shrinkage = apply_grid$shrinkage[i])
  print(round(c, digits = 5))
}
