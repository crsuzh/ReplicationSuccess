## library(testthat)
## sapply(list.files("../../R", pattern='\\.R$', full.names = TRUE), source)

context("powerReplicationSuccess")


test_that("numeric test for powerReplicationSuccess(): 1", {
    expect_equal(object = powerReplicationSuccess(zo = qnorm(p = 1 - 0.05/2),
                                                  c = 1, level = 0.05,
                                                  alternative = "two.sided",
                                                  type = "nominal"),
                 expected = 0)
    expect_equal(object = powerReplicationSuccess(zo = qnorm(p = 1 - 0.0056/2),
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
    expect_equal(out,
                 list(c(4.30651110224e-01, 6.58940633183e-09, 0, 0, 0, 6.58940633183e-09, 4.30651110224e-01),
                      c(4.43281086648e-01, 1.73576581216e-06, 0, 0, 0, 1.73576581216e-06, 4.43281086648e-01),
                      c(8.74983256919e-01, 3.12385006455e-28, 0, 0, 0, 3.12385006455e-28, 8.74983256919e-01),
                      c(7.4668926659280443481e-01, 1.2644672285096127461e-10, 0, 0, 0, 1.2644672285096127461e-10, 7.4668926659280443481e-01),
                      c(0.621076958271, 0.174118774024, 0, 0, 0, 0.174118774024, 0.621076958271),
                      c(0.599376841233, 0.221872456556, 0, 0, 0, 0.221872456556, 0.599376841233),
                      c(0.977226371859, 0.180025396625, 0, 0, 0, 0.180025396625, 0.977226371859),
                      c(0.875842012298, 0.298600320185, 0, 0, 0,0.298600320185, 0.875842012298)))
})


test_that("powerReplicationSuccess() vs .powerReplicationSuccessNum_", {
    cvec <- c(0.001, 0.5, 1, 2, 100)
    vec01bound <- c(0, 0.0386, 0.5031, 0.99)
    vec55 <- c(-5, -2.6288, 0, 0.0427, 4)
    ## only compute the same for alternative="two.sided" with strict=TRUE
    alternative <- c("two.sided")
    designPrior <- c("conditional", "predictive", "EB")
    ## usually level not larger than 0.5
    levelvec <- c(0.001, 0.025, 0.2, 0.49)
    type <- c("golden", "nominal")
    pars_grid <- expand.grid(zo = vec55,
                             c = cvec,
                             level = levelvec,
                             alternative = alternative,
                             designPrior = designPrior,
                             type = type,
                             shrinkage = vec01bound,
                             h = 0, ## heterogeneity not supported in legacy version
                             strict = TRUE,
                             stringsAsFactors = FALSE)
    ## test all configurations separately
    pars_grid <- cbind(pars_grid, new = NA, legacy = NA)
    powerReplicationSuccess <- ReplicationSuccess::powerReplicationSuccess
    f_num <- ReplicationSuccess:::powerReplicationSuccessNum
    for (i in seq_len(nrow(pars_grid))) {
        pars_grid[i,10] <- try(do.call("powerReplicationSuccess", args = pars_grid[i,1:9]),
                               silent = TRUE)
        pars_grid[i,11] <- try(do.call("f_num", args = pars_grid[i,1:7]),
                               silent = TRUE)
    }
    ## 1e-05 the approximation error of the numerical implementation
    expect_equal(object = pars_grid[,10], expected = pars_grid[,11], tolerance = 1e-04)
})
