p2z <- function(p, 
                alternative = "two.sided"){
  
  # vectorize function in both arguments
  zVec <- mapply(FUN = function(p, alternative) {
    
    # sanity checks
    if (!is.numeric(p) || (p <= 0 || p > 1))
      stop("p must be numeric and in (0,1]!")
    if (!(alternative %in% c("less", "greater", "two.sided", "one.sided")))
      stop('alternative must be either "less", "greater", "two.sided", or "one.sided"')
    
    if (alternative == "two.sided")
      z <- qnorm(p = p/2, lower.tail = FALSE)
    if (alternative == "less")
      z <- qnorm(p = p, lower.tail = TRUE)
    if (alternative == "greater" || alternative == "one.sided")
      z <- qnorm(p = p, lower.tail = FALSE)
    
    return(z)
    
  }, p, alternative)
  
  return(zVec)
}
