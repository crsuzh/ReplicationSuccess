test_that("Output of function 'Qtest' stays the same.", {
    # set all possible parameters
    set.seed(42L)
    l <- 5L
    thetao <- stats::rnorm(n = l, mean = 2, sd = 1)
    thetar <- stats::rnorm(n = l, mean = 0, sd = 0.5)
    seo <- stats::rexp(n = l, rate = 1)
    ser <- stats::rexp(n = l, rate = 2)
    out <- Qtest(
        thetao = thetao,
        thetar = thetar,
        seo = seo,
        ser = ser
    )
    res <-c(
        7.22696994204967e-26, 0.277231562296862, 0.0572165017792304,
        0.50769728061928, 2.69451509179729e-05
    )
    expect_equal(out, res)
})