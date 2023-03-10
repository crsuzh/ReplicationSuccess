test_that("Output of function 'thresholdIntrinsic' stays the same.", {
    # set all possible parameters
    grid <- expand.grid(
        alternative = c("two.sided", "one.sided"),
        type = c("Held", "Matthews"),
        stringsAsFactors = FALSE
    )
    # set alpha
    alpha <- c(0.000005, 0.005, 0.01, 0.05, 0.1, 0.5, 1)
    out <- lapply(
        seq_len(nrow(grid)),
        function(i) {
            type <- grid[i, "type"]
            alternative <- grid[i, "alternative"]
            thresholdIntrinsic(
                alpha = alpha,
                alternative = alternative,
                type = type
            )
        }
    )
    res <- list(
        c(7.19495236320815e-05, 0.000269716956631484, 0.00557459668078442),
        c(0.000134858478315742, 0.000501021101850841, 0.010004626858059),
        c(0.000356161575901172, 0.00105100270170506, 0.0126628646365154),
        c(0.000525501350852532, 0.00154239342690154, 0.0182064636662782)
    )
    expect_equal(out, res)
})