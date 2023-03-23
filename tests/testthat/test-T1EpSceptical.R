test_that("Output of function 'T1EpSceptical' stay the same.", {
    # define a range of levels acceptable (Note: 0 and 1 are not allowed)
    grid <- expand.grid(
        level = c(1e-7, 0.01, 0.05, 0.5, 1-1e-7),
        c = c(1e-4, 1, 10),
        alternative = c("one.sided", "two.sided", "greater", "less"),
        type = c("nominal", "golden", "controlled"),
        stringsAsFactors = FALSE
    )
    out <- lapply(
        seq_len(nrow(grid)),
        function(i) {
            tryCatch({
                T1EpSceptical(
                    level = grid[i, "level"],
                    c = grid[i, "c"],
                    alternative = grid[i, "alternative"],
                    type = grid[i, "type"]
                )
            },
            warning = function(w) "warning!",
            error = function(e) "error!"
            )
        }
    )
    res <- list(1.8481478395111e-14, 0.000198974461086162, 0.00499010297667079,
                0.49999999989708, 1.99999959970701, 0, 1.6381173918445e-06,
                0.000501458332820448, 0.5, 1, 1.75783557439074e-95, 1.17784325504187e-11,
                1.26896316819391e-06, 0.49999999989708, "warning!", 9.1857117431564e-15,
                9.92758489167359e-05, 0.00249125903397981, 0.249957677864376,
                0.99999979979417, 0, 2.58192729640783e-07, 8.85754383213033e-05,
                0.177343550652352, 0.9999998, 5.62869116688093e-97, 1.32085180488759e-13,
                1.90786517856787e-08, 0.0612591963304767, 0.99999979979417,
                9.24073919755551e-15, 9.94872305430809e-05, 0.0024950514883354,
                0.24999999994854, 0.999999799853507, 0, 8.19058695922248e-07,
                0.000250729166410224, 0.25, 0.5, 8.78917787195368e-96, 5.88921627520935e-12,
                6.34481584096957e-07, 0.24999999994854, "warning!", 9.24073919903737e-15,
                9.94872305430807e-05, 0.0024950514883354, 0.24999999994854,
                0.999999799853507, 6.2851043157607e-26, 8.19058695927337e-07,
                0.000250729166410241, 0.25, 0.5, 3.45516222748448e-51, 5.88921627094996e-12,
                6.34481584097002e-07, 0.24999999994854, "warning!", 9.16199937893269e-10,
                0.00226641088412062, 0.0191839394645963, 0.49999999989708,
                1.99991273755881, 1.11022302462516e-16, 0.000127232832616619,
                0.00485191915823502, 0.5, 1, 6.332561124499e-34, 8.04156418013958e-08,
                0.000115871632941169, 0.49999999989708, "warning!", 7.63359299991196e-10,
                0.00183053424414904, 0.0151929064306378, 0.355119419090057,
                0.999999842563891, 0, 5.12212418299907e-05, 0.00205852045131949,
                0.288916238382482, 0.999999842769725, 6.67985672501231e-35,
                6.29338518993261e-09, 1.06838888896613e-05, 0.147594067329956,
                0.999999842563891, 4.58099968946634e-10, 0.00113320544206031,
                0.00959196973229814, 0.24999999994854, 0.999956368779407,
                5.55111512312578e-17, 6.36164163083097e-05, 0.00242595957911751,
                0.25, 0.5, 3.1662805622495e-34, 4.02078209006979e-08, 5.79358164705846e-05,
                0.24999999994854, "warning!", 4.58099968946854e-10, 0.00113320544206031,
                0.00959196973229814, 0.24999999994854, 0.999956368779407,
                7.40077008585932e-17, 6.36164163083132e-05, 0.00242595957911752,
                0.25, 0.5, 1.38079515027116e-32, 4.02078209005387e-08, 5.79358164705849e-05,
                0.24999999994854, "warning!", 1.8481478395111e-14, 0.000198974461086162,
                0.00500230476119075, "error!", "error!", 7.7715611723761e-15,
                0.000199936952585134, 0.00500371970716484, "error!", "error!",
                2.01836964776376e-14, 0.000200015456237629, 0.00500038523208522,
                "error!", "error!", 9.1857117431564e-15, 0.000100493444147997,
                0.00249735254469641, 0.250018723027083, "error!", 1.55431223447522e-14,
                0.000100126831303671, 0.00250012364444463, 0.249984142409984,
                "error!", 9.89374814891535e-15, 0.000100003454270831, 0.00250008063564273,
                0.249951571398768, "error!", 9.24073919755551e-15, 9.94872305430809e-05,
                0.00250115238059538, "error!", "error!", 3.88578058618805e-15,
                9.99684762925668e-05, 0.00250185985358242, "error!", "error!",
                1.00918482388188e-14, 0.000100007728118814, 0.00250019261604261,
                "error!", "error!", 9.24073919903737e-15, 9.94872305430807e-05,
                0.00250115238059538, "error!", "error!", 3.88338354811876e-15,
                9.99684762925604e-05, 0.00250185985358241, "error!", "error!",
                1.00918463826467e-14, 0.000100007728118814, 0.00250019261604261,
                "error!", "error!")
    expect_equal(out, res)
})

# This is an adapted version of a test written by CM
test_that("Function 'T1EpSceptical' works as intended.", {
    # required functions
    zrHowLarge <- function(zo, c, level) {
        z <- p2z(level, alternative = "one.sided")
        result <- z * sqrt(1 + c/(zo^2 / z^2 - 1))
        return(result)
    }

    f <- function(zo, level, c) {
        zrMin <- zrHowLarge(zo = zo, c = c, level = level)
        return((1 - pnorm(zrMin)) * dnorm(zo))
    }

    f2 <- function(zo, level, c, sig.level, origPower) {
        zrMin <- zrHowLarge(zo = zo, c = c, level = level)
        zalpha <- qnorm(1 - sig.level)
        mu <- zalpha + qnorm(origPower)
        return((1 - pnorm(zrMin, mean = sqrt(c) * mu)) * dnorm(zo, mean = mu))
    }

    alpha <- 0.025
    myc <- exp(seq(log(0.25), log(10), length.out = 10))
    type <- c("nominal", "golden", "controlled")

    T1E1 <- T1E1_formula <-
        POWER1 <- POWER1_formula <-
        matrix(NA, ncol = length(type), nrow = length(myc))

    myorigPower <- 0.9

    for (i in 1:length(type)) {
        for (j in 1:length(myc)) {
            T1E1[j, i] <- integrate(
                f,
                lower = qnorm(
                    levelSceptical(
                        type = type[i], c = myc[j],
                        level = alpha, alternative = "one.sided"
                    ),
                    lower.tail = FALSE
                ),
                upper = Inf,
                level = levelSceptical(
                    type = type[i], c = myc[j],
                    level = alpha, alternative = "one.sided"
                ),
                c = myc[j]
            )$value

            T1E1_formula[, i] <- T1EpSceptical(
                level = alpha,
                c = myc,
                alternative = "greater", # !not! "one.sided"
                type = type[i]
            )

            POWER1[j, i] <- integrate(
                f2,
                lower =  qnorm(
                    levelSceptical(
                        type = type[i], c = myc[j],
                        level = alpha, alternative = "one.sided"
                    ),
                    lower.tail = FALSE
                ),
                upper = Inf,
                level = levelSceptical(
                    type = type[i], c = myc[j],
                    level = alpha, alternative = "one.sided"
                ),
                c = myc[j],
                sig.level = alpha,
                origPower = myorigPower
            )$value
            POWER1_formula[, i] <-  PPpSceptical(
                level = 0.025,
                c = myc,
                alternative = "one.sided",
                type = type[i],
                alpha = alpha,
                power = myorigPower
            )
        }
    }

    expect_equal(POWER1, POWER1_formula)
    expect_equal(T1E1, T1E1_formula)
})
