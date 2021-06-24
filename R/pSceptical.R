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
#' @references Held, L. (2020). A new standard for the analysis and design of replication
#' studies (with discussion). \emph{Journal of the Royal Statistical Society: Series A
#' (Statistics in Society)}. 183(2):431 - 448. \url{https://doi.org/10.1111/rssa.12493}
#'
#' Held, L., Micheloud, C. & Pawel, S. (2020). The assessment of replication success
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
pSceptical <- function(zo,
                       zr,
                       c, 
                       alternative = "one.sided",
                       type = "golden"){
    
    ## vectorize function in all arguments
    resV <- mapply(FUN = function(zo, zr, c, alternative, type) {
        ## sanity checks
        # if (!(alternative %in% c("one.sided", "two.sided", "greater", "less")))
        #     stop('alternative must be either "one.sided", "two.sided", "greater" or "less"')
        if (!(alternative %in% c("one.sided", "two.sided")))
            stop('alternative must be either "one.sided" or "two.sided"')
        if (!is.numeric(c) || c < 0)
            stop("c must be numeric and larger than 0")
        if (!(type %in% c("nominal", "liberal", "controlled", "golden")))
            stop('type must be either "nominal", "liberal", "controlled", or "golden"')
        
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
        # if(alternative == "greater") {
        #     if(sign(zo) == 1) {
        #         if(sign(zr) == 1) res <- res/2
        #         if(sign(zr) == -1) res <- 1 - res/2
        #     }
        #     if(sign(zo) != 1)
        #         res <- NA
        # }
        # if(alternative == "less") {
        #     if(sign(zo) == -1) {
        #         if(sign(zr) == -1) res <- res/2
        #         if(sign(zr) == 1) res <- 1 - res/2
        #     }
        #     if(sign(zo) != -1)
        #         res <- NA
        # }
        return(res)
    }, zo, zr, c, alternative, type)
    return(resV)
}

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
