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
        type = c("nominal", "liberal", "golden"),
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
            print(i)
            
            levelSceptical(
                level = level,
                alternative = grid[i, "alternative"],
                type = grid[i, "type"],
                c =  grid[i, "c"]
            )
        }
    )
    res <- 
    expect_equal(out, res)
})

# Tests written by CM
test_that("", {
    ## computes the required alphalevel to achieve a certain targetT1E for a given c
    alphaLevel <- function(c, alternative="one.sided", targetT1E){
        mylower <- sqrt(targetT1E)
        if(alternative=="one.sided")
            myupper <- 0.5
        if(alternative=="two.sided")
            myupper <- 1-.Machine$double.eps^0.25
        res <- uniroot(ReplicationSuccess:::target, lower=mylower, upper=myupper,
                       alternative=alternative, c=c,targetT1E=targetT1E)
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
        \(i) {
            print(i)
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
        double(2L)
    )
    levelSceptical(level = 0.05, c = 2.3, alternative = "two.sided",
                   type = "controlled")
    
    alphaLevel(c = 2.3, alternative = "two.sided", targetT1E = 0.05^2)
})
