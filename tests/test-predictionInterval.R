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
checkError(predictionInterval(thetao = "wrong", seo = -1, ser = 1))
checkError(predictionInterval(thetao = 1, seo = 1, ser = 1, designPrior = "wrong"))
checkError(predictionInterval(thetao = 1, seo = -1, ser = 1))
checkError(predictionInterval(thetao = 1, seo = 1, ser = -1))
checkError(predictionInterval(thetao = 1, seo = 1, ser = 1, tau = -1))
checkError(predictionInterval(thetao = 1, seo = 1, ser = 1, conf.level = -1))
checkError(predictionInterval(thetao = 1, seo = 1, ser = 1, conf.level = 2))

## Check numerically some results 
## ------------------------------------------------------------------
za <- qnorm(p = 0.025, lower.tail = FALSE)
checkNumTol(predictionInterval(thetao = za, seo = 1, ser = 1, 
                               designPrior = "conditional"),
            data.frame(lower = 0, mean = za, upper = 2*za))

## Apply over a grid of values
## ------------------------------------------------------------------
thetao <- seq(-2, 2, 2)
apply_grid <- expand.grid(priors = c("conditional", "predictive", "EB"),
                          tau = c(0, 0.5),
                          seo = 1,
                          ser = c(0.5, 2),
                          stringsAsFactors = FALSE)
for (i in seq(1, nrow(apply_grid))) {
  print(apply_grid[i,])
  pis <- predictionInterval(thetao = thetao,
                            seo = apply_grid$seo[i],
                            ser = apply_grid$ser[i],
                            tau = apply_grid$tau[i],
                            designPrior = apply_grid$priors[i])
  print(round(pis, digits = 5))
}
