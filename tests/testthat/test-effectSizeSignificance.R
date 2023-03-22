test_that("Output of function 'effectSizeSignificance' stays the same.", {
    # set all possible parameters
    grid <- expand.grid(
        zo = seq(-4, 4, length.out = 3L),
        c = c(1e-4, 1, 10),
        level = seq(1e-4, 0.99999, length.out = 3L),
        alternative = c("one.sided", "two.sided"),
        stringsAsFactors = FALSE
    )

    out <- lapply(
        seq_len(nrow(grid)),
        function(i) {
            tryCatch({
                effectSizeSignificance(
                    zo = grid[i, "zo"],
                    c = grid[i, "c"],
                    level = grid[i, "level"],
                    alternative = grid[i, "alternative"]
                )
            },
            warning = function(w) "warning!",
            error = function(e) "error!"
            )
        }
    )
    res <- list(
        92.975412136392, Inf, 92.975412136392, 0.92975412136392,
        Inf, 0.92975412136392, 0.29401406874386, Inf, 0.29401406874386,
        -0.00281995681494437, -Inf, -0.00281995681494437, -2.81995681494437e-05,
        -Inf, -2.81995681494437e-05, -8.91748643853814e-06, -Inf,
        -8.91748643853814e-06, -106.622269848096, -Inf, -106.622269848096,
        -1.06622269848096, -Inf, -1.06622269848096, -0.337169222017079,
        -Inf, -0.337169222017079, 97.2647971603273, Inf, 97.2647971603273,
        0.972647971603274, Inf, 0.972647971603274, 0.307578295180912,
        Inf, 0.307578295180912, 16.8604736855589, Inf, 16.8604736855589,
        0.168604736855589, Inf, 0.168604736855589, 0.0533174992756998,
        Inf, 0.0533174992756998, 0.000313328534339131, Inf, 0.000313328534339131,
        3.13328534339131e-06, Inf, 3.13328534339131e-06, 9.90831824433934e-07,
        Inf, 9.90831824433934e-07
    )
    expect_equal(out, res)
})