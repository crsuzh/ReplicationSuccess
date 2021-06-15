.sampleSizeSignificance_one <- function(zo,
                                        power = NA,
                                        d = NA,
                                        level = 0.025,
                                        alternative = "one.sided",
                                        designPrior = "conditional",
                                        h = 0,
                                        shrinkage = 0) {

    ## check that the length of all aguments is <= 1
    if(any(length(zo) > 1, length(power) > 1, length(d) > 1, length(level) > 1,
           length(alternative) > 1, length(designPrior) > 1, length(h) > 1, length(shrinkage) >1))
        stop("all arguments have to be of length one")
    
    ## checks that only one of 'power' and 'd' has to be specified
    if (is.na(d) && is.na(power)) stop("either 'power' or 'd' has to be specified")
    if (!is.na(d) && !is.na(power)) stop("only one of 'power' or 'd' has to be specified")

    ## sample size calculation based on power
    if (is.na(d)) {
        ## input checks
        if (!(designPrior %in% c("conditional", "predictive", "EB")))
            stop('designPrior must be either "conditional", "predictive", or "EB"')
        if (!is.numeric(power) || (power <= 0 || power >= 1))
            stop("power must be numeric and in (0, 1)")
        if (!is.numeric(level) || (level <= 0 || level >= 1))
            stop("level must be numeric and in (0, 1)")
        if (!is.numeric(h) || h < 0)
            stop("h must be numeric and cannot be negative")
        if (!is.numeric(shrinkage) || (shrinkage < 0 || shrinkage > 1))
            stop("shrinkage must be numeric and in [0, 1]")
        u <- qnorm(p = power)
        v <- p2z(p = level, alternative = alternative)
        ## for conditional designPrior use analytical solution
        if (designPrior == "conditional") {
            c <- (u + v)^2*(1/((1 - shrinkage)*zo))^2
        }
        if (designPrior %in% c("predictive", "EB")) {
            if (designPrior == "EB") {
                shrinkage <- pmin((1 + h)/zo^2, 1)
                H <- 1 - shrinkage + 2*h - shrinkage*h
            } else {
                H <- 1 + 2*h
            }
            if (shrinkage >= 1 - u*sqrt(H)/zo) {
                c <- NaN
            } else {
                zos <- (1 - shrinkage)*zo
                num <- zos*v + u*sqrt(zos^2 + H*(v^2 - u^2))
                denom <- zos^2 - u^2*H
                sqrtc <- num/denom
                c <- sqrtc^2
            }
        }
    } else { # sample size calculation based on relative effect size
        ## input checks
        if (!is.numeric(d))
            stop("d must be numeric")
        if (!is.numeric(level) || (level <= 0 || level >= 1))
            stop("level must be numeric and in (0, 1)!")
        zalpha <- qnorm(1 - level)
        zalpha <- p2z(p = level, alternative = alternative)
        c <- zalpha^2/(d^2*zo^2)
    }
    return(c)
}


#' @name sampleSizeSignificance
#' @rdname sampleSizeSignificance
#' @author Leonhard Held, Samuel Pawel, Charlotte Micheloud
#' @title Computes the required relative sample size to achieve significance
#' based on power or on the minimum relative effect size.
#' @description
#' The relative sample size to achieve significance of the replication study is 
#' computed based on the z-value of the original study, the significance level 
#' and either the power or the minimum relative effect size.
#' When the approach based on power is used, the arguments design prior,
#' shrinkage, and relative heterogeneity also have to be specified.
#' @param zo A vector of z-values from original studies.
#' @param power The power to achieve replication success.
#' @param d The minimum relative effect size (ratio of the effect estimate
#' from the replication study to the effect estimate from the original study).
#' @param level Significance level. Default is 0.025.
#' @param alternative Either \code{"two.sided"}, \code{"one.sided"}, \code{"less"},
#' or \code{"greater"}.  Specifies direction of the alternative.
#' Defaults to \code{"one.sided"}, the same direction as the original estimate.
#' @param designPrior  Is only taken into account when \code{"power"} is specified.
#' Either \code{"conditional"}, \code{"predictive"}, or \code{"EB"}.  
#' Defaults to \code{"conditional"}. If \code{"EB"}, the power is computed under
#' a predictive distribution where the contribution of the original study is 
#' shrunken towards zero based on the evidence in the original study 
#' (with an empirical Bayes shrinkage estimator).
#' @param h Is only taken into account when \code{"power"} is specified.
#' The relative between-study heterogeneity, i.e. the ratio of the heterogeneity
#' variance to the variance of the original effect estimate.
#' Default is \code{0} (no heterogeneity).
#' Is only taken into account when \code{designPrior = "predictive"} or
#' \code{designPrior = "EB"}.
#' @param  shrinkage Is only taken into account when \code{"power"} is specified.
#' A number in [0,1]. Defaults to \code{0}. Specifies how much the original effect
#' estimate is shrunken towards zero (e.g. the effect is shrunken by a factor of 25\%
#' for \code{shrinkage = 0.25}). Is only taken into account when \code{designPrior = "conditional"}
#' or\code{designPrior = "predictive"}.
#' @return   The relative sample size to achieve significance in the specified direction.
#' If larger than 1000 then NA is returned. 
#' @seealso \code{\link{powerSignificance}}
#' @references
#' Held, L. (2020). A new standard for the analysis and design of replication studies (with discussion).
#' \emph{Journal of the Royal Statistical Society: Series A (Statistics in Society)}, \bold{183}, 431-448.
#' \url{https://doi.org/10.1111/rssa.12493}
#'
#' Pawel, S., Held, L. (2020). Probabilistic forecasting of replication studies. \emph{PLoS ONE} 15(4):e0231416.
#' \url{https://doi.org/10.1371/journal.pone.0231416}
#'
#' Held, L., Micheloud, C. & Pawel, S. (2020). The assessment of replication
#' success based on relative effect size. \url{https://arxiv.org/abs/2009.07782}
#' @examples
#' par(mfrow = c(1,2))
#' sampleSizeSignificance(zo = p2z(0.005), power = 0.8)
#' sampleSizeSignificance(zo = p2z(0.005, alternative = "greater"), power = 0.8)
#' sampleSizeSignificance(zo = p2z(0.005), power = 0.8, designPrior = "predictive")
#' 
#' sampleSizeSignificance(zo = 3, power = 0.8, designPrior = "predictive", 
#'                        shrinkage = 0.5, h = 0.25)
#' sampleSizeSignificance(zo = 3, power = 0.8, designPrior = "EB", 
#'                        h = 0.5)
#'                        
#' # required relative sample size for 0.8 power as function of original p-value
#' zo <- p2z(seq(0.0001, 0.05, 0.0001))
#' plot(z2p(zo), sampleSizeSignificance(zo = zo, designPrior = "conditional", power = 0.8),
#'      type = "l", ylim = c(0.5, 10), log = "y", lwd = 1.5, ylab = "Relative sample size",
#'      xlab = expression(italic(p)[o]), las = 1)
#' lines(z2p(zo), sampleSizeSignificance(zo = zo, designPrior = "predictive", power = 0.8), 
#'       lwd = 2, lty = 2)
#' lines(z2p(zo), sampleSizeSignificance(zo = zo, designPrior = "EB", power = 0.8),
#'       lwd = 1.5, lty = 3)
#' legend("topleft", legend = c("conditional", "predictive", "EB"), 
#'        title = "Design prior", lty = c(1, 2, 3), lwd = 1.5, bty = "n")
#'        
#' sampleSizeSignificance(zo = p2z(0.005), d = 1)
#' sampleSizeSignificance(zo = p2z(0.005), d = 0.5)
#' # required relative effect size of 0.8 power as function of original p-value
#' zo <- p2z(seq(0.0001, 0.05, 0.0001))
#' plot(z2p(zo), sampleSizeSignificance(zo = zo, d = 0.8, level = 0.025),
#'      type = "l", ylim = c(0.5, 10), log = "y", lwd = 1.5, ylab = "Relative sample size",
#'     xlab = expression(italic(p)[o]), las = 1)
#'
#' @export
sampleSizeSignificance <- Vectorize(FUN=.sampleSizeSignificance_one)

.sampleSizeSignificance_uniroot <- function(zo,
                                   power = NA,
                                   d = NA,
                                   level = 0.025,
                                   alternative = "one.sided",
                                   designPrior = "conditional",
                                   h = 0,
                                   shrinkage = 0) {
    
                                        # Target function for calculating required sample size u`sing uniroot
    ClassicalTarget <- function(c, zo, level, power, alternative,
                                h, shrinkage, designPrior){
        term <- powerSignificance(zo = zo, 
                                  c = c, 
                                  level = level,
                                  designPrior = designPrior,
                                  alternative = alternative,
                                  h = h,
                                  shrinkage = shrinkage)
        return(term - power)
    }
    n.l <- 0
    n.u <- 1000
    
    # vectorize function in all arguments
    cV <- mapply(FUN = function(zo, power, d, level, alternative, designPrior,
                                h, shrinkage) {
        # checks that only one of 'power' and 'd' has to be specified
        if (is.na(d) & is.na(power))  stop("either 'power' or 'd' has to be specified")
        if (!is.na(d) & !is.na(power))  stop("only one of 'power' or 'd' has to be specified")
        
        # sample size calculation based on power
        if (is.na(d)) {
            
            # sanity checks
            if (!(designPrior %in% c("conditional", "predictive", "EB")))
                stop('designPrior must be either "conditional", "predictive", or "EB"')
            if (!is.numeric(power) || (power <= 0 || power >= 1)) 
                stop("power must be numeric and in (0, 1)")
            if (!is.numeric(level) || (level <= 0 || level >= 1)) 
                stop("level must be numeric and in (0, 1)")
            if (!is.numeric(h) || h < 0)
                stop("h must be numeric and cannot be negative")
            if (!is.numeric(shrinkage) || (shrinkage < 0 || shrinkage > 1)) 
                stop("shrinkage must be numeric and in [0, 1]")
            
            # s is 1 - shrinkage
            s <- 1 - shrinkage
            
            # for conditional designPrior use analytical solution
            if (designPrior == "conditional") {
                u <- qnorm(p = power)
                v <- p2z(p = level, alternative = alternative)
                c <- (u + v)^2*(1/(s*zo))^2
            }
            
            # for predictive and EB designPrior use uniroot
            if (designPrior %in% c("predictive", "EB")) {
                
                # compute upper bound of power
                # if (designPrior == "EB") s <- pmax(1 - (1 + h)/zo^2, 0)
                # power.limit <- pnorm(sqrt(1/(s*(1 + h) + h))*s*abs(zo))
                # if (power > power.limit) {
                #     power.limit.r <- floor(power.limit * 1000)/1000
                #     warning(paste("power too large, power should not exceed",
                #                    power.limit.r,
                #                   "for a zo of",
                #                   zo,
                #                   "\n"))
                #     c <- NaN
                # } else {
                
                # check whether desired power can be achieved for max c = n.u
                target.l <- ClassicalTarget(c = n.l, 
                                            zo = zo,
                                            level = level,
                                            power = power,
                                            alternative = alternative,
                                            h = h,
                                            shrinkage = shrinkage,
                                            designPrior = designPrior)
                target.u <- ClassicalTarget(c = n.u, 
                                            zo = zo,
                                            level = level,
                                            power = power,
                                            alternative = alternative,
                                            h = h,
                                            shrinkage = shrinkage,
                                            designPrior = designPrior)
                if (sign(target.l) == sign(target.u)) 
                    c <- NaN
                # determine c to achieve desired power
                else c <- uniroot(f = ClassicalTarget, 
                                  lower = n.l, 
                                  upper = n.u,
                                  zo = zo,
                                  level = level,
                                  power = power, 
                                  alternative = alternative,
                                  h = h,
                                  shrinkage = shrinkage,
                                  designPrior = designPrior)$root
            }
            # }
        } else { # sample size calculation based on relative effect size
            # sanity checks
            if (!is.numeric(d)) 
                stop("d must be numeric")
            if (!is.numeric(level) || (level <= 0 || level >= 1)) 
                stop("level must be numeric and in (0, 1)!")
            
            zalpha <- qnorm(1- level)
            zalpha <- p2z(p = level, alternative = alternative)
            c <- zalpha^2/(d^2*zo^2)
        }
        return(c)
    }, zo, power, d, level, alternative, designPrior, h, shrinkage)
    
    return(cV)
}

