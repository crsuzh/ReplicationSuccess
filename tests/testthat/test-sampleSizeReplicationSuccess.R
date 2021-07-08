## library(testthat)
## sapply(list.files("../../R", pattern='\\.R$', full.names = TRUE), source)

context("sampleSizeReplicationSuccess")


test_that("numeric test for sampleSizeReplicationSuccess(): 1", {
    za <- qnorm(p = 0.001/2, lower.tail = FALSE)
    expect_equal(object = sampleSizeReplicationSuccess(zo = za, designPrior = "conditional",
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
    expect_equal(out,
                 list(c(0.407463446750989, NaN, NaN, NaN, 0.407463446750989),
                      c(0.469946779033606, NaN, NaN, NaN, 0.469946779033606),
                      c(0.549411550469594, NaN, NaN, NaN, 0.549411550469594),
                      c(0.654287906282666, NaN, NaN, NaN, 0.654287906282666)))
})

## checking that sampleSizeReplicationSuccess produces sample size which
## corresponds to input power if defined. When not defined should also be
## undefined in numerical implementation
test_that("sampleSizeReplicationSuccess(): recomputing the power", {
    vec01 <- c(0.001, 0.2532, 0.99)
    vec01bound <- c(0, 0.0386, 0.5031, 1)
    vec55 <- c(-5, -2.6288, 0, 0.0427, 4)
    alternative <- c("two.sided", "one.sided")
    designPrior <- c("conditional", "predictive", "EB")
    ## power should be larger than level
    powvec <- c(0.55, 0.8, 0.99)
    levelvec <- c(0.001, 0.025, 0.2, 0.4)
    shrinkvec <- c(0, 0.0386, 0.5031, 0.99)
    type <- c("golden", "nominal")
    pars_grid <- expand.grid(zo = vec55,
                             power = powvec,
                             d = NA,
                             level = levelvec,
                             type = type,
                             designPrior = designPrior,
                             alternative = alternative,
                             shrinkage = shrinkvec,
                             h = 0,
                             new = NA, legacy = NA,
                             pownew = NA, powlegacy = NA,
                             stringsAsFactors = FALSE)
    ## test all configurations separately
    f_num <- ReplicationSuccess:::sampleSizeReplicationSuccessNum
    suppressWarnings( {
        for (i in seq_len(nrow(pars_grid))){
            cnew <- try(do.call("sampleSizeReplicationSuccess", args = pars_grid[i,1:9]),
                        silent = TRUE)
            if (inherits(cnew, "try-error")){
                pars_grid[i,"new"] <- NA
            } else {
                pars_grid[i,"new"] <- cnew
                if (!is.na(cnew)) {
                    pownew <-  try(powerReplicationSuccess(zo = pars_grid[i,"zo"],
                                                           c = cnew,
                                                           level = pars_grid[i,"level"],
                                                           designPrior = pars_grid[i,"designPrior"],
                                                           alternative = pars_grid[i,"alternative"],
                                                           type = pars_grid[i,"type"],
                                                           shrinkage = pars_grid[i,"shrinkage"]),
                                   silent = TRUE)
                    if (class(pownew) == "try-error") {
                        pars_grid[i,"pownew"] <- NA
                    } else {
                        pars_grid[i,"pownew"] <- pownew
                    }
                }
            }
            clegacy <- try(do.call("f_num", args = pars_grid[i,1:8]), silent = TRUE)
            if (inherits(clegacy, "try-error")){
                pars_grid[i,"legacy"] <- NA
            } else {
                pars_grid[i,"legacy"] <- clegacy
                if (!is.na(clegacy)) {
                    powlegacy <-  try(powerReplicationSuccess(zo = pars_grid[i,"zo"],
                                                              c = clegacy,
                                                              level = pars_grid[i,"level"],
                                                              designPrior = pars_grid[i,"designPrior"],
                                                              alternative = pars_grid[i,"alternative"],
                                                              type = pars_grid[i,"type"],
                                                              shrinkage = pars_grid[i,"shrinkage"]),
                                      silent = TRUE)
                    if (class(powlegacy) == "try-error") {
                        pars_grid[i,"powlegacy"] <- NA
                    } else {
                        pars_grid[i,"powlegacy"] <- powlegacy
                    }
                }
            }
        }
    })
    ## check that difference to input power small where power can be computed
    pars_grid$powdiff <- abs(pars_grid$pownew - pars_grid$power)
    finiteNew <- is.finite(pars_grid$new)
    expect_equal(object = pars_grid$pownew[finiteNew],
                 expected = pars_grid$power[finiteNew])

    ## difference between legacy and analytical implementation (makes no sense
    ## to compare the two since legacy uses always two-sided power while new version
    ## always used one-sided power)
    ## finiteLegacy <- is.finite(pars_grid$legacy)
    ## View(pars_grid[which(finiteNew != finiteLegacy),c(1, 3:11, 2, 12:14)])
    ## cases where they disagree are when:
    ## 1) c > 1000 and legacy implementation can't find it
    ## 2) impossible to obtain a c with desired one-sided power, but
    ##    possible with two-sided power (usually when desired power just
    ##    below the power limit)
})
