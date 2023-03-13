test_that("Output of function 'pvalueBound' stays the same.", {
    # set all possible parameters
    # set alpha
    alpha <- c(0, 0.000005, 0.005, 0.01, 0.05, 0.1, 0.5, 1)
    grid <- expand.grid(
        n = c(1, 5, 10),
        type = c("necessary", "sufficient"),
        stringsAsFactors = FALSE
    )
    out <- lapply(
        seq_len(nrow(grid)),
        function(i) {
            suppressWarnings({
                pvalueBound(
                    alpha = alpha,
                    n = grid[i, "n"],
                    type = grid[i, "type"]
                )
            })
        }
    )
    res <- list(
        c(0, 5.00000000003276e-06, 0.005, 0.01, 0.0500000000000002, 0.1, 0.5, 0),
        c(0, 0.225123774733589, 0.389349711269652, 0.421173779375272, 0.433164099939562, NaN, NaN, NaN),
        c(0, 0.38976233221047, NaN, NaN, NaN, NaN, NaN, NaN),
        c(0, 5.00000000003276e-06, 0.005, 0.01, 0.0500000000000002, 0.1, 0.5, 0),
        c(0, 0.0456829040333974, 0.264881740723192, 0.328255605455661, 0.353315549379915, NaN, NaN, NaN),
        c(0, 0.188012694018994, NaN, NaN, NaN, NaN, NaN, NaN)
    )
    expect_equal(out, res)
})
