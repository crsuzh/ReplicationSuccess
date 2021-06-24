## library(testthat)
## sapply(list.files("../../R", pattern='\\.R$', full.names = TRUE), source)

## context("sampleSizeSignificance")


## expect_equal_tol <- function(object, expected, tol=1e-3, msg=""){
##     message(msg)
##     if(!is.numeric(object) || !is.finite(object)){
##         expect_equal(object=object, expected=expected)
##     } else {
##         expect_lt(object=abs(object - expected), expected=tol)
##     }
## }

## test_that("sampleSizeSignificance() agrees with .sampleSizeSignificance_uniroot()", {
##     vec01 <- c(0.001, 0.2532, 0.5, 0.6609, 0.8286, 0.99)
##     vec01bound <- c(0, 0.0386, 0.2833, 0.5, 0.5031, 0.6508, 0.9592, 1)
##     vec55 <- c(-5, -3, -2.6288, -2, -1.341, -1, 0, 0.0427, 0.6115, 1, 2.9401, 4, 4.9643, 5)
##     alternative <- c("two.sided", "one.sided", "less", "greater")
##     designPrior <- c("conditional", "predictive", "EB")
##     pars_grid_power <- expand.grid(zo=vec55,
##                                    power=vec01,
##                                    d=NA,
##                                    level=vec01,
##                                    alternative=alternative,
##                                    designPrior=designPrior,
##                                    h=abs(vec55),
##                                    shrinkage=vec01bound)
##     pars_grid_d <- expand.grid(zo=c(-3, -2.1, 0, 1.22 , 2),
##                                power=NA,
##                                d=c(.001, .2, .5, .7, .8, .99),
##                                level=.025,
##                                alternative="one.sided",
##                                designPrior="conditional",
##                                h=0,
##                                shrinkage=0)
##     pars_grid <- rbind(pars_grid_power, pars_grid_d)

##     ## test all configurations separately
##     for(i in seq_len(nrow(pars_grid))){
##         expect_equal_tol(object=do.call("sampleSizeSignificance",
##                                         args = pars_grid[i,]),
##                          expected=do.call(".sampleSizeSignificance_uniroot",
##                                           args = pars_grid[i,]),
##                          msg=paste(i))
##     }

##     ## test vecotrized input
##     expect_equal(object=do.call("sampleSizeSignificance", args = pars_grid),
##                  expected=do.call(".sampleSizeSignificance_uniroot", args = pars_grid))
  
## })
