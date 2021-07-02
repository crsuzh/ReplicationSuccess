## library(testthat)
## sapply(list.files("../../R", pattern='\\.R$', full.names = TRUE), source)

context("sampleSizeReplicationSuccess")
source("helpers.R")


test_that("numeric test for sampleSizeReplicationSuccess(): 1", {
    za <- qnorm(p = 0.001/2, lower.tail = FALSE)
    expect_equal_tol(object = sampleSizeReplicationSuccess(zo = za, designPrior = "conditional",
                                                           power = 0.8, level = 0.05,
                                                           alternative = "two.sided",
                                                           type = "nominal"),
                     expected = 1, tol = 0.01)
})


test_that("numeric test for sampleSizeReplicationSuccess(): 2", {
    zo <- seq(-4, 4, 2)
    apply_grid <- expand.grid(priors = c("conditional", "predictive"),
                              alt = c("one.sided", "two.sided"),
                              stringsAsFactors = FALSE)
    
    out <- lapply(X=seq_len(nrow(apply_grid)), FUN=function(i){
        sampleSizeReplicationSuccess(zo = zo, 
                                     power = 0.8, 
                                     level = 0.05,
                                     designPrior = apply_grid$priors[i],
                                     alternative = apply_grid$alt[i],
                                     type = "nominal")
    })
    
    expect_equal_tol(out,
                     list(c(Inf, Inf, Inf, Inf, 0.407426313316046),
                          c(Inf, Inf, Inf, Inf, 0.469740710568011),
                          c(0.549414467208847, Inf, Inf, Inf, 0.549414467208847),
                          c(0.654250464630736, Inf, Inf, Inf, 0.654250464630736)))
})
