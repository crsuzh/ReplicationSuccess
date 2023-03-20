test_that("Output of function 'pReplicate' stays the same.", {
    # set all possible parameters
    grid <- expand.grid(
        po = seq(1e-6, 1, 4),
        c = c("Held", "Matthews"),
        alternative = c("two.sided", "one.sided"),
        stringsAsFactors = FALSE
    )
    out <- lapply(
        seq_len(nrow(grid)),
        function(i) {
            pReplicate(
                po = grid[i, "po"],
                c = grid[i, "c"],
                alternative = grid[i, "alternative"],
            )
        }
    )
    res <- 
    expect_equal(out, res)
})