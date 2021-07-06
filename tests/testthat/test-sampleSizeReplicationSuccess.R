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
        suppressWarnings({
        sampleSizeReplicationSuccess(zo = zo, 
                                     power = 0.8, 
                                     level = 0.05,
                                     designPrior = apply_grid$priors[i],
                                     alternative = apply_grid$alt[i],
                                     type = "nominal")
        })
    })
    
    expect_equal_tol(out,
                     list(c(0.407463446750989, NaN, NaN, NaN, 0.407463446750989),
                          c(0.469946779033606, NaN, NaN, NaN, 0.469946779033606),
                          c(0.549411550469594, NaN, NaN, NaN, 0.549411550469594),
                          c(0.654287906282666, NaN, NaN, NaN, 0.654287906282666)))
    })
