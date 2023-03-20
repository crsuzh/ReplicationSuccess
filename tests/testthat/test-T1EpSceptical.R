test_that("Output of function 'T1EpSceptical' stay the same.", {
    # define a range of levels acceptable (Note: 0 and 1 are not allowed)
    grid <- expand.grid(
        level = c(1e-7, 0.01, 0.05, 0.1, 0.5, 1-1e-7),
        type = c("nominal", "liberal", "golden", "controlled")
    )
})
