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
checkError(powerReplicationSuccess(zo = 1, c = 1,  
                                   designPrior = "wrong"))
checkError(powerReplicationSuccess(zo = 1, c = -1))
checkError(powerReplicationSuccess(zo = 1, c = 1, level = -1))
checkError(powerReplicationSuccess(zo = 1, c = 1, level = 2))
checkError(powerReplicationSuccess(zo = 1, c = 1, alternative = "wrong"))

## Check numerically results from paper
## ------------------------------------------------------------------
checkNumTol(powerReplicationSuccess(zo = qnorm(p = 1 - 0.05/2),
                                    c = 1, level = 0.05,
                                    alternative = "two.sided",
                                    type = "nominal"), 
            0)
checkNumTol(powerReplicationSuccess(zo = qnorm(p = 1 - 0.0056/2),
                                    c = 1, level = 0.05,
                                    alternative = "two.sided",
                                    type = "nominal"), 
            0.5, tol = 0.01)

## Apply over a grid of values
## ------------------------------------------------------------------
zo <- seq(-3, 3, 1)
apply_grid <- expand.grid(priors = c("conditional", "predictive"),
                          c = c(0.5, 2),
                          alt = c("two.sided", "one.sided"),
                          stringsAsFactors = FALSE)

for (i in seq(1, nrow(apply_grid))) {
  print(apply_grid[i,])
  p <- powerReplicationSuccess(zo = zo,
                               c = apply_grid$c[i],
                               level = 0.05,
                               designPrior = apply_grid$priors[i],
                               alternative = apply_grid$alt[i],
                               type = "nominal")
  print(round(p, digits = 5))
}
