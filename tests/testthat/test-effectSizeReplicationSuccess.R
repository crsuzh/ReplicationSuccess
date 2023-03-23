test_that("Output of function 'effectSizeReplicationSuccess' stays the same.", {
    # set all possible parameters
    grid <- expand.grid(
        zo = seq(-4, 4, length.out = 3L),
        c = c(1e-4, 1, 10),
        level = seq(1e-4, 0.99999, length.out = 3L),
        alternative = c("one.sided", "two.sided"),
        type = c("golden", "nominal", "controlled"),
        stringsAsFactors = FALSE
    )

    out <- lapply(
        seq_len(nrow(grid)),
        function(i) {
            tryCatch({
                effectSizeReplicationSuccess(
                    zo = grid[i, "zo"],
                    c = grid[i, "c"],
                    level = grid[i, "level"],
                    alternative = grid[i, "alternative"],
                    type = grid[i, "type"]
                )
            },
            warning = function(w) "warning!",
            error = function(e) "error!"
            )
        }
    )
    res <- list(73.0969404541718, "warning!", 73.0969404541718, 1.07102642204892,
                "warning!", 1.07102642204892, 0.816252478846714, "warning!",
                0.816252478846714, 0.00221691293528586, Inf, 0.00221691293528586,
                2.21691293583058e-05, NaN, 2.21691293583058e-05, 7.01049426701986e-06,
                "warning!", 7.01049426701986e-06, 83.8311450395328, Inf,
                83.8311450395328, 1.53703478010626, NaN, 1.53703478010626,
                1.31534627570162, "warning!", 1.31534627570162, 76.4702365614467,
                "warning!", 76.4702365614467, 1.18651793895581, "warning!",
                1.18651793895581, 0.938938850327047, "warning!", 0.938938850327047,
                13.2548964696555, "warning!", 13.2548964696555, 0.133728805717944,
                "warning!", 0.133728805717944, 0.0455095211856697, "warning!",
                0.0455095211856697, 0.000246323658962554, "warning!", 0.000246323658962554,
                2.46323658963301e-06, "warning!", 2.46323658963301e-06, 7.78943803931849e-07,
                "warning!", 7.78943803931849e-07, 93.0050524158691, "warning!",
                93.0050524158691, 2.52526348581919, "warning!", 2.52526348581919,
                2.36621157533257, "warning!", 2.36621157533257, 0.00281995681494448,
                Inf, 0.00281995681494448, 2.8199568160656e-05, NaN, 2.8199568160656e-05,
                8.91748647399477e-06, "warning!", 8.91748647399477e-06, 106.577968187218,
                Inf, 106.577968187218, "warning!", NaN, "warning!", "warning!",
                "warning!", "warning!", 97.3500301432072, "warning!", 97.3500301432072,
                4.18731970601557, "warning!", 4.18731970601557, 4.0843857128568,
                "warning!", 4.0843857128568, 16.8604983518478, "warning!",
                16.8604983518478, 0.171053583792177, "warning!", 0.171053583792177,
                0.0606178766290874, "warning!", 0.0606178766290874, 0.000313328534339131,
                "warning!", 0.000313328534339131, 3.13328534340669e-06, "warning!",
                3.13328534340669e-06, 9.90831824482572e-07, "warning!", 9.90831824482572e-07,
                93.0050524158691, "warning!", 93.0050524158691, 0.945115549226922,
                "warning!", 0.945115549226922, 0.301778030165621, "warning!",
                0.301778030165621, "error!", "error!", "error!", "error!",
                "error!", "error!", "error!", "error!", "error!", "error!",
                "error!", "error!", "error!", "error!", "error!", "error!",
                "error!", "error!", 97.3500301432072, "warning!", 97.3500301432072,
                1.025931148083, "warning!", 1.025931148083, 0.328411938982688,
                "warning!", 0.328411938982688, 16.8580976659652, "warning!",
                16.8580976659652, 0.145294602912764, "warning!", 0.145294602912764,
                0.0357970532154254, "warning!", 0.0357970532154254, "error!",
                "error!", "error!", "error!", "error!", "error!", "error!",
                "error!", "error!")
    expect_equal(out, res)
})
