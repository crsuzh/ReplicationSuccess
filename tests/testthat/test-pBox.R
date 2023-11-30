test_that("Output of function 'pBox' stays the same.", {
    # set all possible parameters
    grid <- expand.grid(
        zo = seq(-4, 4, length.out = 3L),
        zr = seq(-4, 4, length.out = 3L),
        c = c(0.001, 1, 10),
        level = c(1e-4, 0.05, 0.999999),
        alternative = c("one.sided", "two.sided"),
        stringsAsFactors = FALSE
    )
    out <- lapply(
        seq_len(nrow(grid)),
        function(i) {
            tryCatch({
                pBox(
                    zo = grid[i, "zo"],
                    zr = grid[i, "zr"],
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
        3.34137971947029e-05, "error!", 0.999966586202805, 0.5,
        "error!", 0.5, 0.999966586202805, "error!", 3.34137971947029e-05,
        0.0704127178588384, "error!", 0.929587282141162, 0.5, "error!",
        0.5, 0.929587282141162, "error!", 0.0704127178588384, 0.309587205963449,
        "error!", 0.690412794036551, 0.5, "error!", 0.5, 0.690412794036551,
        "error!", 0.309587205963449, 3.17257492201826e-05, "error!",
        0.99996827425078, 0.5, "error!", 0.5, 0.99996827425078, "error!",
        3.17257492201826e-05, 0.000133095996311856, "error!", 0.999866904003688,
        0.5, "error!", 0.5, 0.999866904003688, "error!", 0.000133095996311856,
        0.0108376053689272, "error!", 0.989162394631073, 0.5, "error!",
        0.5, 0.989162394631073, "error!", 0.0108376053689272, "error!",
        "error!", "error!", "error!", "error!", "error!", "error!",
        "error!", "error!", "error!", "error!", "error!", "error!",
        "error!", "error!", "error!", "error!", "error!", "error!",
        "error!", "error!", "error!", "error!", "error!", "error!",
        "error!", "error!", 7.32768948750901e-05, "error!", 7.32768948750901e-05,
        1, "error!", 1, 7.32768948750901e-05, "error!", 7.32768948750901e-05,
        0.352818312294978, "error!", 0.352818312294978, 1, "error!",
        1, 0.352818312294978, "error!", 0.352818312294978, 0.763244029877727,
        "error!", 0.763244029877727, 1, "error!", 1, 0.763244029877727,
        "error!", 0.763244029877727, 6.35117906339563e-05, "error!",
        6.35117906339563e-05, 1, "error!", 1, 6.35117906339563e-05,
        "error!", 6.35117906339563e-05, 0.000488635808825294, "error!",
        0.000488635808825294, 1, "error!", 1, 0.000488635808825294,
        "error!", 0.000488635808825294, 0.0498457305262133, "error!",
        0.0498457305262133, 1, "error!", 1, 0.0498457305262133, "error!",
        0.0498457305262133, 6.33424836662398e-05, "error!", 6.33424836662398e-05,
        1, "error!", 1, 6.33424836662398e-05, "error!", 6.33424836662398e-05,
        6.33424836662924e-05, "error!", 6.33424836662924e-05, 1,
        "error!", 1, 6.33424836662924e-05, "error!", 6.33424836662924e-05,
        6.33424836667653e-05, "error!", 6.33424836667653e-05, 1,
        "error!", 1, 6.33424836667653e-05, "error!", 6.33424836667653e-05
    )
    expect_equal(out, res)
})
