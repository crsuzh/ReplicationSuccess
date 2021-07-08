## library(testthat)
## sapply(list.files("../../R", pattern='\\.R$', full.names = TRUE), source)

context("predictionInterval")

test_that("numeric test for predictionInterval(): 1", {
    za <- qnorm(p = 0.025, lower.tail = FALSE)
    expect_equal(object = predictionInterval(thetao = za, seo = 1, ser = 1,
                                             designPrior = "conditional"),
                 expected = data.frame(lower = 0, mean = za, upper = 2*za),
                 tol = 0.0001)
})


test_that("numeric test for predictionInterval(): 2", {
    thetao <- seq(-2, 2, 2)
    apply_grid <- expand.grid(priors = c("conditional", "predictive", "EB"),
                              tau = c(0, 0.5),
                              seo = 1,
                              ser = c(0.5, 2),
                              stringsAsFactors = FALSE)
    out <- lapply(X=seq_len(nrow(apply_grid)), FUN = function(i){
        predictionInterval(thetao = thetao,
                            seo = apply_grid$seo[i],
                            ser = apply_grid$ser[i],
                            tau = apply_grid$tau[i],
                            designPrior = apply_grid$priors[i])
    })
    expect_equal(out,
                     list(structure(list(lower = c(-2.97998199227003, -0.979981992270027, 
1.02001800772997), mean = c(-2, 0, 2), upper = c(-1.02001800772997, 
0.979981992270027, 2.97998199227003)), class = "data.frame", row.names = c(NA, 
-3L)), structure(list(lower = c(-4.19130635144145, -2.19130635144145, 
-0.191306351441454), mean = c(-2, 0, 2), upper = c(0.191306351441454, 
2.19130635144145, 4.19130635144145)), class = "data.frame", row.names = c(NA, 
-3L)), structure(list(lower = c(-3.45996398454005, -0.979981992270027, 
-0.459963984540054), mean = c(-1.5, 0, 1.5), upper = c(0.459963984540054, 
0.979981992270027, 3.45996398454005)), class = "data.frame", row.names = c(NA, 
-3L)), structure(list(lower = c(-2.97998199227003, -0.979981992270027, 
1.02001800772997), mean = c(-2, 0, 2), upper = c(-1.02001800772997, 
0.979981992270027, 2.97998199227003)), class = "data.frame", row.names = c(NA, 
-3L)), structure(list(lower = c(-4.59278864086811, -2.59278864086811, 
-0.592788640868113), mean = c(-2, 0, 2), upper = c(0.592788640868113, 
2.59278864086811, 4.59278864086811)), class = "data.frame", row.names = c(NA, 
-3L)), structure(list(lower = c(-3.66016587677592, -1.38590382434968, 
-0.910165876775924), mean = c(-1.375, 0, 1.375), upper = c(0.910165876775924, 
1.38590382434968, 3.66016587677592)), class = "data.frame", row.names = c(NA, 
-3L)), structure(list(lower = c(-5.91992796908011, -3.91992796908011, 
-1.91992796908011), mean = c(-2, 0, 2), upper = c(1.91992796908011, 
3.91992796908011, 5.91992796908011)), class = "data.frame", row.names = c(NA, 
-3L)), structure(list(lower = c(-6.38261270288291, -4.38261270288291, 
-2.38261270288291), mean = c(-2, 0, 2), upper = c(2.38261270288291, 
4.38261270288291, 6.38261270288291)), class = "data.frame", row.names = c(NA, 
-3L)), structure(list(lower = c(-5.7716424707947, -3.91992796908011, 
-2.7716424707947), mean = c(-1.5, 0, 1.5), upper = c(2.7716424707947, 
3.91992796908011, 5.7716424707947)), class = "data.frame", row.names = c(NA, 
-3L)), structure(list(lower = c(-5.91992796908011, -3.91992796908011, 
-1.91992796908011), mean = c(-2, 0, 2), upper = c(1.91992796908011, 
3.91992796908011, 5.91992796908011)), class = "data.frame", row.names = c(NA, 
-3L)), structure(list(lower = c(-6.5965229808865, -4.5965229808865, 
-2.5965229808865), mean = c(-2, 0, 2), upper = c(2.5965229808865, 
4.5965229808865, 6.5965229808865)), class = "data.frame", row.names = c(NA, 
-3L)), structure(list(lower = c(-5.80528821432467, -4.04056926533255, 
-3.05528821432467), mean = c(-1.375, 0, 1.375), upper = c(3.05528821432467, 
4.04056926533255, 5.80528821432467)), class = "data.frame", row.names = c(NA, 
-3L))))
})
