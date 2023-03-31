test_that("Output of function 'levelEquivalent' stays the same.", {
    grid <- expand.grid(
        dinf = seq(1e-6, 4, length.out = 4),
        level = seq(1e-6, 1 - 1e-6, length.out = 4),
        alternative = c("one.sided", "two.sided"),
        stringsAsFactors = FALSE
    )
    out <- lapply(
        seq_len(nrow(grid)),
        function(i) {
            ReplicationSuccess:::levelEquivalent(
                dinf = grid[i, "dinf"],
                level = grid[i, "level"],
                alternative = grid[i, "alternative"]
            )
        }
    )

    expect_equal(
        out,
        list(0.497587831099434, 1.63093518989714e-07, 5.96767888868161e-09,
             2.10713779359606e-09, 0.499781422613272, 0.321745148225167,
             0.302732392898475, 0.297222038366052, 0.500218577386727,
             0.678254851774833, 0.697267607101525, 0.702777961633948,
             0.502412168900563, 0.999999836906481, 0.999999994032321,
             0.999999997892862, 0.995035387882307, 1.4708973730017e-07,
             4.45349784685276e-09, 1.48120228256636e-09, 0.9990181409927,
             0.29856594867123, 0.245967203931534, 0.231775398916138, 0.999562843830884,
             0.64348923722917, 0.605463634302254, 0.59444289973172, 0.999999998727981,
             0.999998925479515, 0.999998800728358, 0.999998763932022)
    )
})
