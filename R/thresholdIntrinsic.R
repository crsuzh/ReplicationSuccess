thresholdIntrinsic <- function(alpha, 
                               alternative = "two.sided", 
                               type = "Held"){
    z <- p2z(p = alpha, alternative = alternative)
    if(type == "Held")
        result <- z2p(z = sqrt(2)*z, alternative = alternative)
    if(type == "Matthews")
        result <- z2p(z = sqrt(2)*z/sqrt(sqrt(5) - 1), alternative = alternative)
    return(result)
}
