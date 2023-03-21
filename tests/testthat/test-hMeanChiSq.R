test_that("Output of function 'hMeanChiSq' stays the same.", {
    # set all possible parameters
    set.seed(42L)
    z <- rnorm(5L, mean = 0, sd = 2)
    w <- diff(sort(runif(n = length(z))))
    w <- c(w, 1 - sum(w))
    grid <- expand.grid(
        alternative = c("greater", "less", "two.sided", "none"),
        bound = c(TRUE, FALSE),
        stringsAsFactors = FALSE
    )
    out <- lapply(
        seq_len(nrow(grid)),
        function(i) {
            tryCatch({
                hMeanChiSq(
                    z = z,
                    w = w,
                    alternative = grid[i, "alternative"],
                    bound = grid[i, "bound"]
                )
            },
            warning = function(w) "warning!",
            error = function(e) "error!"
            )
        }
    )
    res <- list("> 0.03125", "> 0.03125", "> 0.0625", 0.0545824477857667,
                NaN, NaN, NaN, 0.0545824477857667)
    expect_equal(out, res)
})

test_that("Output of function 'hMeanChiSqMu' stays the same.", {
    # set all possible parameters
    set.seed(42L)
    n <- 5L
    thetahat <- rnorm(n, mean = 0, sd = 2)
    se <- abs(rnorm(n, mean = 0, sd = 0.5))
    w <- diff(sort(runif(n = length(thetahat))))
    w <- c(w, 1 - sum(w))
    grid <- expand.grid(
        mu = seq(-4, 4, length.out = n),
        alternative = c("greater", "less", "two.sided", "none"),
        bound = c(TRUE, FALSE),
        stringsAsFactors = FALSE
    )
    out <- lapply(
        seq_len(nrow(grid)),
        function(i) {
            tryCatch({
                hMeanChiSqMu(
                    thetahat = thetahat,
                    se = se,
                    w = w,
                    mu = grid[i, "mu"],
                    alternative = grid[i, "alternative"],
                    bound = grid[i, "bound"]
                )
            },
            warning = function(w) "warning!",
            error = function(e) "error!"
            )
        }
    )
    res <- list(
        1.52012950343536e-16, 0.000510817026605097, "> 0.03125",
        "> 0.03125", "> 0.03125", "> 0.03125", "> 0.03125", "> 0.03125",
        "> 0.03125", 9.32413128951708e-36, 3.04025900687072e-16,
        0.00102163405321019, "> 0.0625", "> 0.0625", 1.86482625790342e-35,
        4.86441441099315e-15, 0.0163461448513631, 0.00262344261357555,
        2.2240791037915e-07, 2.98372201264547e-34, 1.52012950343536e-16,
        0.000510817026605097, NaN, NaN, 9.32413128951708e-36, 1.52012950343536e-16,
        0.000510817026605097, NaN, NaN, 9.32413128951708e-36, 3.04025900687072e-16,
        0.00102163405321019, NaN, NaN, 1.86482625790342e-35, 4.86441441099315e-15,
        0.0163461448513631, 0.00262344261357555, 2.2240791037915e-07,
        2.98372201264547e-34
    )
    expect_equal(out, res)
})

test_that("Output of function 'hMeanChiSqCI' stays the same.", {
    # set all possible parameters
    set.seed(42L)
    n <- 5L
    thetahat <- rnorm(n, mean = 0, sd = 2)
    se <- abs(rnorm(n, mean = 0, sd = 0.5))
    w <- diff(sort(runif(n = length(thetahat))))
    w <- c(w, 1 - sum(w))
    wGamma <- rep(1L, length(unique(thetahat)) - 1L)
    grid <- expand.grid(
        alternative = c("greater", "less", "two.sided", "none"),
        level = seq(1e-4, 0.99999, length.out = 3L),
        stringsAsFactors = FALSE
    )
    out <- lapply(
        seq_len(nrow(grid)),
        function(i) {
            tryCatch({
                hMeanChiSqCI(
                    thetahat = thetahat,
                    se = se,
                    w = w,
                    level = grid[i, "level"],
                    alternative = grid[i, "alternative"],
                    wGamma = wGamma
                )
            },
            warning = function(w) "warning!",
            error = function(e) "error!"
            )
        }
    )
    res <- list(
        "error!", "error!", "error!",
        list(CI = structure(
            c(-1.12947482273488, 0.726256822674678,
              0.808536646281998, 1.26572520992208,
              2.74191689429334, -1.12933530763593,
              0.726256822674678, 0.808536646281998,
              1.26572520992208, 2.74191689429334),
            dim = c(5L, 2L), dimnames = list(NULL, c("lower", "upper"))),
            gamma = structure(
                c(0.49104940828295, 0.766213563663337,
                0.863109314098832, 2.56562031022734,
                0.00019226765301783, 0.00404570139122806,
                0.00433382604234844, 2.62678767626312e-13),
                dim = c(4L, 2L), dimnames = list(NULL, c("minimum", "pvalue_fun/gamma"))),
            gammaMean = 0.00214294877171425, gammaHMean = 1.05071506893784e-12),
        "error!", "error!", "error!",
        list(CI = structure(
            c(-1.37322842110802, 0.722580669258802, 0.804910218000078,
              1.1888288547464, 2.7372509370574, -0.885511976838297,
              0.729932048714472, 0.812157206496657, 1.34256073642529,
              2.74657617456907),
            dim = c(5L, 2L), dimnames = list(NULL, c("lower", "upper"))),
            gamma = structure(
                c(0.491049408270793, 0.766213563663337, 0.863109314098832,
                  2.56486895629396, 0.00019226765301783, 0.00404570139122812,
                  0.00433382604234844, 2.62623256475081e-13),
                dim = c(4L, 2L), dimnames = list(NULL, c("minimum", "pvalue_fun/gamma"))),
            gammaMean = 0.00214294877171425, gammaHMean = 1.05049302433358e-12),
        list(CI = structure(
            c(-2.43763307622691, Inf), dim = 1:2, dimnames = list(NULL, c("lower", "upper")))),
        list(CI = structure(
            c(-Inf, 2.76933301613503), dim = 1:2, dimnames = list(NULL, c("lower", "upper")))),
        list(CI = structure(c(
            -2.50230084038932, 2.77102063017006),
            dim = 1:2, dimnames = list(NULL, c("lower", "upper")))),
        list(CI = structure(
            c(-2.73810412117517, 2.70575276470872, 1.85872550562387, 2.77765886818584),
            dim = c(2L, 2L), dimnames = list(NULL, c("lower", "upper"))),
            gamma = structure(
                c(0.491049408272113, 0.766213563663337, 0.863109314098832,
                  2.56442192247056, 0.000192267653017805, 0.00404570139122811,
                  0.00433382604234846, 2.62627986307058e-13),
                dim = c(4L, 2L), dimnames = list(NULL, c("minimum", "pvalue_fun/gamma"))),
            gammaMean = 0.00214294877171425, gammaHMean = 1.05051194366143e-12))
    expect_equal(out, res)
})