zSceptical <- function(zo, 
                       zr, 
                       c){
    
    ## arithmetic mean
    arit.mean <- function(x, y)
        return((x + y)/2)
    ## harmonic mean
    harm.mean <- function(x, y)
        return(2/(1/x + 1/y))
    ## vectorize function in all arguments
    z2V <- mapply(FUN = function(zo, zr, c) {
        z2H <- harm.mean(zo^2, zr^2)
        z2A <- arit.mean(zo^2, zr^2)
        if (c == 1)
            z2 <- z2H/2
        else
            z2 <- (sqrt(z2A*(z2A + (c - 1)*z2H)) - z2A)/(c - 1)

        return(z2)
    }, zo, zr, c)
    return(sqrt(z2V))
}
