#' @export
.pSceptical_ <- function(zo,
                         zr,
                         c, 
                         alternative = c("one.sided", "two.sided"),
                         type = c("golden", "nominal", "liberal", "controlled")){
    
    stopifnot(is.numeric(zo),
              length(zo) == 1,
              is.finite(zo),

              is.numeric(zr),
              length(zr) == 1,
              is.finite(zr),

              is.numeric(c),
              length(c) == 1,
              is.finite(c),
              0 <= c,
              
              !is.null(alternative))
    alternative <- match.arg(alternative)

    stopifnot(!is.null(type))
    type <- match.arg(type)
    
    z <- zSceptical(zo = zo, zr = zr, c = c)
    if(type == "nominal")
        result <- z
    if(type == "liberal"){
        result <- z*sqrt(2)
    }
    if(type == "controlled"){
        result <- p2z(p = sqrt((1 - pnorm(2*z))/2), alternative = "greater")
    }
    if(type == "golden"){
            ## golden ratio 
        phi <- (sqrt(5) + 1)/2  
        result <- z*sqrt(phi)
    }
    res <- z2p(z = result, alternative = "two.sided")
    if(alternative == "one.sided") {
        if(sign(zo) == sign(zr)) 
            res <- res/2
        else 
            res <- 1 - res/2
    }
    return(res)
}


#' Computes the sceptical p-value and z-value
#'
#' Computes sceptical p-values and z-values based on the z-values of the
#' original and the replication study and the corresponding variance ratio.
#' If specified, the p-values are recalibrated.
#' @rdname pSceptical
#' @param zo Numeric vector of z-values from original studies.
#' @param zr Numeric vector of z-values from replication studies.
#' @param c Numeric vector of variance ratios of the original and replication
#' effect estimates. This is usually the ratio of the sample
#' size of the replication study to the sample size of the
#' original study.
#' @param alternative Either "one.sided" (default) or "two.sided".
#' If "one.sided", the sceptical p-value is based on a one-sided
#' assessment of replication success in the direction of the original effect estimate.
#' If "two.sided", the sceptical p-value is based on a two-sided
#' assessment of replication success regardless of the direction of the
#' original and replication effect estimate.
#' @param type Type of recalibration. Can be either "golden" (default), "nominal",
#' "liberal", or "controlled". Setting \code{type} to "nominal" corresponds to no
#' recalibration as in Held et al. (2020). A recalibration is applied if
#' \code{type} is "liberal", "controlled", or "golden", and the sceptical p-value
#' can then be interpreted on the same scale as an ordinary p-value (e.g., a one-sided
#' sceptical p-value can be thresholded at the conventional 0.025 level).
#' See \code{\link{levelSceptical}} for details about recalibration types. 
#' @return \code{pSceptical} returns the sceptical p-value.
#' @details \code{pSceptical} is the vectorized version of \code{.pSceptical_}.
#' \code{\link[base]{Vectorize}} is used to vectorize the function.
#' @references
#' Held, L. (2020). A new standard for the analysis and design of replication
#' studies (with discussion). \emph{Journal of the Royal Statistical Society:
#' Series A (Statistics in Society)}, \bold{183}, 431-448.
#' \doi{10.1111/rssa.12493}
#'
#' Held, L., Micheloud, C., Pawel, S. (2021). The assessment of replication success
#' based on relative effect size. \url{https://arxiv.org/abs/2009.07782}
#' @author Leonhard Held
#' @seealso \code{\link{sampleSizeReplicationSuccess}}, \code{\link{powerReplicationSuccess}}, \code{\link{levelSceptical}}
#' @examples
#' ## no recalibration (type = "nominal") as in Held (2020)
#' pSceptical(zo = p2z(0.01), zr = p2z(0.02), c = 2, alternative = "one.sided",
#'            type = "nominal")
#'
#' ## recalibration with golden level as in Held, Micheloud, Pawel (2020)
#' pSceptical(zo = p2z(0.01), zr = p2z(0.02), c = 2, alternative = "one.sided",
#'            type = "golden")
#'
#' ## two-sided p-values 0.01 and 0.02, relative sample size 2
#' pSceptical(zo = p2z(0.01), zr = p2z(0.02), c = 2, alternative = "one.sided")
#' ## reverse the studies
#' pSceptical(zo = p2z(0.02), zr = p2z(0.01), c = 1/2, alternative = "one.sided")
#' ## both p-values 0.01, relative sample size 2
#' pSceptical(zo = p2z(0.01), zr = p2z(0.01), c = 2, alternative = "two.sided")
#' @export
pSceptical <- Vectorize(.pSceptical_)



#' @rdname pSceptical
#' @return \code{zSceptical} returns the z-value of the sceptical p-value.
#' @examples
#'
#' zSceptical(zo = 2, zr = 3, c = 2)
#' zSceptical(zo = 3, zr = 2, c = 2)
#' @export
zSceptical <- function(zo, 
                       zr, 
                       c){
    stopifnot(is.numeric(zo),
              length(zo) > 0,
              is.finite(zo),
              
              is.numeric(zr),
              length(zr) > 0,
              is.finite(zr),
              
              is.numeric(c),
              length(c) > 0,
              is.finite(c),
              0 <= c)

    ## arithmetic mean
    aritMean <- function(x, y)
        return((x + y)/2)
    ## harmonic mean
    harmMean <- function(x, y)
        return(2/(1/x + 1/y))

    ## vectorize function in all arguments
    z2H <- harmMean(zo^2, zr^2)
    z2A <- aritMean(zo^2, zr^2)

    if(length(c)==1){
        z2 <- if(c == 1) z2H/2 else (sqrt(z2A*(z2A + (c - 1)*z2H)) - z2A)/(c - 1)
    } else {
        z2 <- ifelse(c == 1, 
                     z2H/2,
                     (sqrt(z2A*(z2A + (c - 1)*z2H)) - z2A)/(c - 1))
    }
     
    return(sqrt(z2))
}
