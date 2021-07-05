## library(testthat)
## sapply(list.files("../../R", pattern='\\.R$', full.names = TRUE), source)

context("powerReplicationSuccess")
source("helpers.R")


test_that("numeric test for powerReplicationSuccess(): 1", {
    expect_equal_tol(object = powerReplicationSuccess(zo = qnorm(p = 1 - 0.05/2),
                                    c = 1, level = 0.05,
                                    alternative = "two.sided",
                                    type = "nominal"),
                     expected = 0)

    expect_equal_tol(object = powerReplicationSuccess(zo = qnorm(p = 1 - 0.0056/2),
                                    c = 1, level = 0.05,
                                    alternative = "two.sided",
                                    type = "nominal"), 
                     expected = 0.5,
                     tol = 0.01)
    
})


test_that("numeric test for powerReplicationSuccess(): 2", {
    zo <- seq(-3, 3, 1)
    apply_grid <- expand.grid(priors = c("conditional", "predictive"),
                          c = c(0.5, 2),
                          alt = c("two.sided", "one.sided"),
                          stringsAsFactors = FALSE)
    out <- lapply(X=seq_len(nrow(apply_grid)), FUN=function(i){
        powerReplicationSuccess(zo = zo,
                               c = apply_grid$c[i],
                               level = 0.05,
                               designPrior = apply_grid$priors[i],
                               alternative = apply_grid$alt[i],
                               type = "nominal")
    })
    
    expect_equal_tol(out,
                     list(c(0.430650458202026, 0, 0, 0, 0, 0, 0.430650458202026), 
    c(0.443413695396971, 0, 0, 0, 0, 0, 0.443413695396971), c(0.8749833000594, 
    0, 0, 0, 0, 0, 0.8749833000594), c(0.746702330550861, 0, 
    0, 0, 0, 0, 0.746702330550861), c(0.621148468047692, 0.174180811538108, 
    0, 0, 0, 0.174180811538108, 0.621148468047692), c(0.600021385924778, 
    0.222918901859534, 0, 0, 0, 0.222918901859534, 0.600021385924778
    ), c(0.977236547904878, 0.18003467902742, 0, 0, 0, 0.18003467902742, 
    0.977236547904878), c(0.875936454290268, 0.29868431840938, 
    0, 0, 0, 0.29868431840938, 0.875936454290268)))
})
