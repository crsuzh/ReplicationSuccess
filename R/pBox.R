zBox <- function(zo, 
                 zr, 
                 c, 
                 level,
                 alternative=alternative){
    z <- p2z(level, alternative=alternative)
    den <- ifelse((zo^2 > z^2), c/(zo^2/z^2 - 1) + 1, NA)
    res <- zr^2/den
    return(sqrt(res))
}

pBox <- function(zo, 
                 zr,
                 c, 
                 level = 0.05,
                 alternative = "two.sided"){
    z <- zBox(zo = zo, zr = zr, c = c, level = level, alternative = alternative)
    res <- z2p(z)
    if(alternative == "one.sided")
        res <- ifelse(sign(zo) == sign(zr), res/2, 1 - res/2)
    return(res)
}
