
cH <- function(alpha, n){
    res <- (qnorm(1-2^(n-1)*alpha))^2
    return(res)
}

pvalueBound <- function(alpha, n, type="necessary"){
  if(!(type %in% c("necessary", "sufficient")))
    stop("Incorrect type chosed")
    if(type=="necessary")
        bound <- 1-pnorm(sqrt(cH(alpha, n))/n)
    if(type=="sufficient")
        bound <- 1-pnorm(sqrt(cH(alpha, n)/n))
    return(bound)
}
