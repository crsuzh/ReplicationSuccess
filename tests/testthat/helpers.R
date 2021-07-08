expect_equal_tol <- function(object, expected, tol=1e-6, ...){
    if(!is.numeric(object) || !is.finite(object)){
        return(expect_equal(object=object, expected=expected, ...))
    }
    expect_lt(object=max(abs(object - expected)), expected=tol, ...)
}
