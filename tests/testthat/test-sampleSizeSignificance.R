context("sampleSizeSignificance")

test_that("sampleSizeSignificance() agrees with .sampleSizeSignificance_uniroot()", {
    pars_grid_power <- expand.grid(zo=c(1,2),
                                   power=c(.5,.7,.9),
                                   d=NA,
                                   level=c(.025,.05),
                                   alternative="one.sided",
                                   designPrior="conditional",
                                   h=0,
                                   shrinkage=0)
    pars_grid_d <- expand.grid(zo=c(1,2),
                                   power=NA,
                                   d=.5,
                                   level=.025,
                                   alternative="one.sided",
                                   designPrior="conditional",
                                   h=0,
                                   shrinkage=0)
    pars_grid <- rbind(pars_grid_power, pars_grid_d)

    ## test all configurations separately
    for(i in seq_len(nrow(pars_grid))){
        expect_equal(object=do.call("sampleSizeSignificance",
                                    args = pars_grid[i,]),
                     expected=do.call(".sampleSizeSignificance_uniroot",
                                      args = pars_grid[i,]))
    }

    ## test vecotrized input
    expect_equal(object=do.call("sampleSizeSignificance", args = pars_grid),
                 expected=do.call(".sampleSizeSignificance_uniroot", args = pars_grid))
  
})
