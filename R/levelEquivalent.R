
levelEquivalent <- function(dinf, level=0.025, alternative="one.sided"){
    if (!is.numeric(dinf) || dinf <= 0)
        stop("dinf must be numeric and larger than 0")
    if (!is.numeric(level) || (level <= 0 || level >= 1))
        stop("level must be numeric and in (0, 1)!")
    if (!(alternative %in% c("one.sided", "two.sided", "greater", "less")))
        stop('alternative must be either "one.sided", "two.sided", "greater" or "less"')

    zalpha <- p2z(level, alternative=alternative)
    K <- 0.5 + sqrt(1/dinf^2 + 1/4)
    phi <- (sqrt(5)+1)/2
    zalphaNew <- zalpha*sqrt(phi/K)
    alphaNew <- z2p(zalphaNew, alternative=alternative)
    return(alphaNew)
}
