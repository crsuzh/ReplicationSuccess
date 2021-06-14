pIntrinsic <- function(p = z2p(z, alternative = alternative),
                       z = NULL,
                       alternative = "two.sided", 
                       type = "Held"){
    if(type == "Held"){
        iz <- p2z(p, alternative = alternative)/sqrt(2)
        iP <- z2p(z = iz, alternative = alternative)
    }
    if(type == "Matthews"){
        iz <- p2z(p, alternative = alternative)/sqrt(2)*sqrt(sqrt(5) - 1)
        iP <- z2p(z = iz, alternative = alternative)
    }
    return(iP)
}
