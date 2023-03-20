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
            tryCatch({
                thresholdIntrinsic(
                    alpha = alpha,
                    alternative = alternative,
                    type = type
                )
            },
            warning = function(w) "warning!",
            error = function(e) "error!"
            )
        }
    )
    res <- list(
        c(1.07801853562574e-10, 7.19495236320815e-05, 0.000269716956631484,
          0.00557459668078442, 0.020009253716118, 0.340148158600666, 1),
          "error!",
        c(6.37923911569433e-09, 0.000356161575901172, 0.00105100270170506,
          0.0126628646365154, 0.0364129273325564, 0.390912223290277, 1),
        "error!"
    )
    expect_equal(out, res)
})