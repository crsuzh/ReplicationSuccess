## library(testthat)
## sapply(list.files("../../R", pattern='\\.R$', full.names = TRUE), source)

context("sampleSizeSignificance")

test_that("numeric test for sampleSizeSignificance(): 1", {
    za <- qnorm(p = 0.025, lower.tail = FALSE)
    expect_equal(object = sampleSizeSignificance(zo = za, designPrior = "conditional",
                                                 power = 0.8, alternative = "one.sided"),
                 expected = 2.04, tol = 0.01)
})


test_that("numeric test for sampleSizeSignificance(): 2", {
    zo <- seq(-4, 4, 2)
    apply_grid <- expand.grid(priors = c("conditional", "predictive", "EB"),
                          h = c(0, 0.1),
                          shrinkage = c(0, 0.75),
                          alt = c("one.sided", "two.sided"),
                          stringsAsFactors = FALSE)
    out <- lapply(X=seq_len(nrow(apply_grid)), FUN=function(i){
        sampleSizeSignificance(zo = zo, 
                              power = 0.8, 
                              level = 0.05,
                              designPrior = apply_grid$priors[i],
                              alternative = apply_grid$alt[i],
                              h = apply_grid$h[i],
                              shrinkage = apply_grid$shrinkage[i])
    })
    expect_equal(out, list(
                          c(0.386409827001236,1.54563930800494,Inf,1.54563930800494,0.386409827001236),
                          c(0.440562894751761,2.64240442300288,NA,2.64240442300288,0.440562894751761),
                          c(0.505683996618901,5.68097835352188,NA,5.68097835352188,0.505683996618901),
                          c(0.386409827001236,1.54563930800494,Inf,1.54563930800494,0.386409827001236),
                          c(0.452332524054221,2.95871584108967,NA,2.95871584108967,0.452332524054221),
                          c(0.52827525176549,7.60908343838383,NA,7.60908343838383,0.52827525176549),
                          c(6.18255723201977,24.7302289280791,Inf,24.7302289280791,6.18255723201977),
                          c(113.100184785495,NA,NA,NA,113.100184785495),
                          c(0.505683996618901,5.68097835352188,NA,5.68097835352188,0.505683996618901),
                          c(6.18255723201977,24.7302289280791,Inf,24.7302289280791,6.18255723201977),
                          c(453.912799705814,NA,NA,NA,453.912799705814),
                          c(0.52827525176549,7.60908343838383,NA,7.60908343838383,0.52827525176549),
                          c(0.490554983396818,1.96221993358727,Inf,1.96221993358727,0.490554983396818),
                          c(0.567658970548963,3.51088218947353,NA,3.51088218947353,0.567658970548963),
                          c(0.652140069510171,7.6215454064467,NA,7.6215454064467,0.652140069510171),
                          c(0.490554983396818,1.96221993358727,Inf,1.96221993358727,0.490554983396818),
                          c(0.584343691289888,3.95487118399945,NA,3.95487118399945,0.584343691289888),
                          c(0.683282959536562,10.3012684887906,NA,10.3012684887906,0.683282959536562),
                          c(7.84887973434909,31.3955189373964,Inf,31.3955189373964,7.84887973434909),
                          c(158.405996395716,NA,NA,NA,158.405996395716),
                          c(0.652140069510171,7.6215454064467,NA,7.6215454064467,0.652140069510171),
                          c(7.84887973434909,31.3955189373964,Inf,31.3955189373964,7.84887973434909),
                          c(640.395220372103,NA,NA,NA,640.395220372103),
                          c(0.683282959536562,10.3012684887906,NA,10.3012684887906,0.683282959536562)
                      ))
})


test_that("sampleSizeSignificance() vs sampleSizeSignificanceNum", {
    vec01 <- c(0.001, 0.2532, 0.99)
    vec01bound <- c(0, 0.0386, 0.5031, 0.99)
    vec55 <- c(-5, -2.6288, 0, 4)
    alternative <- c("two.sided", "one.sided")
    designPrior <- c("conditional", "predictive", "EB")
    ## power should only be larger than level
    powvec <- c(0.499, 0.8, 0.975)
    levelvec <- c(0.001, 0.025, 0.49)
    pars_grid_power <- expand.grid(zo=vec55,
                                   power=powvec,
                                   d=NA,
                                   level=levelvec,
                                   alternative=alternative,
                                   designPrior=designPrior,
                                   h=abs(vec55),
                                   shrinkage=vec01bound, stringsAsFactors = FALSE)
    pars_grid_d <- expand.grid(zo=vec55,
                               power=NA,
                               d=vec01,
                               level=.025,
                               alternative="one.sided",
                               designPrior="conditional",
                               h=0,
                               shrinkage=0, stringsAsFactors = FALSE)
    ## test all configurations separately
    pars_grid <- cbind(rbind(pars_grid_power, pars_grid_d), new=NA, legacy=NA)
    sampleSizeSignificanceNum <- ReplicationSuccess:::sampleSizeSignificanceNum
    for(i in seq_len(nrow(pars_grid))){
        pars_grid[i,9] <- do.call("sampleSizeSignificance", args = pars_grid[i,1:8])
        pars_grid[i,10] <- do.call("sampleSizeSignificanceNum", args = pars_grid[i,1:8])
    }

    ## exclude cases with large vales
    pars_grid <- pars_grid[pars_grid$new < 1000,]
    expect_equal(object=is.na(pars_grid[,9]), expected=is.na(pars_grid[,10]))
    pars_grid_nonNA <- pars_grid[!is.na(pars_grid[,9]) & !is.na(pars_grid[,10]),]
    expect_equal(object = pars_grid_nonNA[,9], expected = pars_grid_nonNA[,10], tol = 0.1)
})
