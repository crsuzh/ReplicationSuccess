effectSizeSignificance <- function(zo, 
                                   c = 1, 
                                   level = 0.025, 
                                   alternative = "one.sided"){
  
  mV <- mapply(FUN = function(zo, c, level) {
    if (!is.numeric(c)) 
      stop("d must be numeric")
    if (!is.numeric(level) || (level <= 0 || level >= 1)) 
      stop("level must be numeric and in (0,1)!")
    zalpha <- z2p(level, alternative = alternative)
    d <- zalpha/(zo*sqrt(c))
    return(d)
  }, zo, c, level)
  return(mV)
}
  