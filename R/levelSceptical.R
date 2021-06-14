
levelSceptical <- function(level, 
                           alternative = "one.sided", 
                           type = "golden"){
    if (!(type %in% c("nominal", "liberal", "controlled", "golden")))
        stop('type must be either "nominal", "liberal", "controlled", or "golden"')
    if (!(alternative %in% c("one.sided", "two.sided", "greater", "less")))
        stop('alternative must be either "one.sided", "two.sided", "greater" or "less"')
    
    if(type == "nominal")
        res <- level
    
    if(type == "liberal")
        res <- pIntrinsic(p = level, alternative = alternative, type = "Held")
    
    if(type == "controlled"){
        if (alternative == "two.sided") {
            t1 <- level^2 ## level is two-sided significance level
            ## t1 <- (2*level)^2 ## level is a one-sided significance level
            res <- 2*(1 - pnorm(q = qnorm(p = 1 - t1/2)/2))
        } 
        if (alternative %in% c("one.sided", "greater", "less")) {
            ## t1 <- level*(level/2) ## level is a two-sided significance level
            t1 <- 2*level^2 ## level is a one-sided significance level
            res <- 1 - pnorm(q = qnorm(p = 1 - t1)/2)
        }
    }
    if(type == "golden"){
        res <- pIntrinsic(p = level, alternative = alternative, type = "Matthews")
    }
    return(res)
}
