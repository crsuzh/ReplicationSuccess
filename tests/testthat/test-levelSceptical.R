test_that("Output of function 'levelSceptical' stays the same.", {
    # set all possible parameters
    grid_controlled <- expand.grid(
        alternative = c("two.sided", "one.sided"),
        type = "controlled",
        c = c(1e-5, 0.5, 1, 1.5, 5, 10),
        stringsAsFactors = FALSE
    )
    grid_rest <- expand.grid(
        alternative = c("two.sided", "one.sided"),
        type = c("nominal", "golden"),
        c = NA_real_,
        stringsAsFactors = FALSE
    )
    grid <- rbind(grid_rest, grid_controlled)
    # set level of replication success. Note: In the function definition
    # we do not allow 0 or 1 but anything in between
    level <- c(1e-6, 0.005, 0.01, 0.05, 0.1, 0.5, 0.999)
    out <- lapply(
        seq_len(nrow(grid)),
        function(i) {
            tryCatch({
                levelSceptical(
                    level = level,
                    alternative = grid[i, "alternative"],
                    type = grid[i, "type"],
                    c =  grid[i, "c"]
                )
            },
            warning = function(w) "warning!",
            error = function(e) "error!"
            )
        }
    )
    res <- list(
        c(1e-06, 0.005, 0.01, 0.05, 0.1, 0.5, 0.999),
        c(1e-06, 0.005, 0.01, 0.05, 0.1, 0.5, 0.999),
        c(0.000120273342410185, 0.0273312878015435, 0.0428682107732366,
          0.123358558704367, 0.195975110892562, 0.595937883003954, 0.999213848543629),
        c(9.31473875686863e-05, 0.0214341053866183, 0.0337101730585594,
          0.0979875554462809, 0.156848832934081, 0.5, 0.992437881245328),
        c(1e-06, 0.005, 0.01, 0.05, 0.1, 0.5, 0.999),
        "error!",
        c(0.0001230703125, 0.0231661522426465, 0.0362305332225521,
          0.104482884301164, 0.167564981493604, 0.544385476905582, 0.999),
        "error!",
        c(0.0003672109375, 0.0350797364800123, 0.0517578701184723,
          0.130619555416346, 0.197775154089893, 0.565160134707053, 0.999),
        "error!",
        c(0.000733421875000002, 0.0452158514769247, 0.0643804764006999,
          0.15008537065244, 0.219576651593275, 0.579869614260351, 0.999),
        "error!",
        c(0.00493656685712746, 0.0949127866342207, 0.12269147198411,
          0.228026183709154, 0.302377503301343, 0.633105985166959, 0.999),
        "error!",
        c(0.0130868106403446, 0.139897370649398, 0.172310755243015,
          0.285727684366346, 0.360418465338739, 0.668190405230152, 0.999),
        "error!"
    )
    expect_equal(out, res)
})

# Tests written by CM
test_that("alphaLevel and levelSceptical return the same.", {
    ## computes the required alphalevel to achieve a certain targetT1E for a given c
    alphaLevel <- function(c, alternative = "one.sided", targetT1E){
        mylower <- sqrt(targetT1E)
        if (alternative == "one.sided")
            myupper <- 0.5
        if (alternative == "two.sided")
            myupper <- 1 - .Machine$double.eps^0.25
        res <- uniroot(
            ReplicationSuccess:::target, lower = mylower, upper = myupper,
            alternative = alternative, c = c,targetT1E = targetT1E
        )
        return(res$root)
    }
    # parameter grid
    grid <- expand.grid(
        level = seq(1e-6, 1 - 1e-6, length.out = 3L),
        c = seq(1e-2, 5, length.out = 4L),
        alternative = c("one.sided", "two.sided"),
        stringsAsFactors = FALSE
    )
    out <- vapply(
        seq_len(nrow(grid)),
        function(i) {
            tryCatch({
                c(
                    "alphaLevel" = alphaLevel(
                        c = grid[i, "c"], alternative = grid[i, "alternative"],
                        targetT1E = grid[i, "level"]^2
                    ),
                    "levelSceptical" = levelSceptical(
                        level = grid[i, "level"], c = grid[i, "c"],
                        alternative = grid[i, "alternative"], type = "controlled"
                    )
                )
            },
            warning = function(w) rep(NA_real_, 2L),
            error = function(e) rep(NA_real_, 2L)
            )
        },
        double(2L)
    )
    # remove cases that throw errors
    out <- out[, !apply(out, 2L, function(x) all(is.na(x)))]
    expect_true(all(apply(out, 2L, diff) == 0))
})