#' @export
.predictionInterval_ <- function(thetao,
                                 seo,
                                 ser,
                                 tau = 0,
                                 conf.level = 0.95,
                                 designPrior = c("predictive", "conditional", "EB")) {

    stopifnot(is.numeric(thetao),
              length(thetao) == 1,
              is.finite(thetao),

              is.numeric(seo),
              length(seo) == 1,
              is.finite(seo),
              0 < seo,  

              is.numeric(ser),
              length(ser) == 1,
              is.finite(ser),
              0 < ser,  

              is.numeric(tau),
              length(tau) == 1,
              is.finite(tau),
              0 <= tau,  

              is.numeric(conf.level),
              length(conf.level) == 1,
              is.finite(conf.level),
              0 <= conf.level, conf.level <= 1,

              !is.null(designPrior))
    designPrior <- match.arg(designPrior)

    ## determine parameters of predictive distribution of yr
    if(designPrior == "conditional"){
        mu <- thetao
        sigma <- ser
    } else if(designPrior == "predictive"){
        mu <- thetao
        sigma <- sqrt(seo^2 + ser^2 + 2*tau^2)
    } else{ ## designPrior == "EB"
        s <- pmax(1 - (seo^2 + tau^2)/thetao^2, 0)
        mu <- s*thetao
        sigma <- sqrt(s*(seo^2 + tau^2) + ser^2 + tau^2)
    }
    
    ## compute prediction interval
    lower <- qnorm(p = (1 - conf.level)/2, mean = mu, sd = sigma)
    upper <- qnorm(p = (1 + conf.level)/2, mean = mu, sd = sigma)
    result <- cbind(lower = lower, mean = mu, upper = upper)
    return(result)
}
.predictionInterval__ <- Vectorize(.predictionInterval_)
#' Prediction interval for effect estimate of replication study
#'
#' Computes a prediction interval for the effect estimate of the replication study.
#' @param thetao Numeric vector of effect estimates from original studies.
#' @param seo Numeric vector of standard errors of the original effect estimates.
#' @param ser Numeric vector of standard errors of the replication effect estimates.
#' @param tau Between-study heterogeneity standard error.
#' Default is \code{0} (no heterogeneity).
#' Is only taken into account when \code{designPrior} is "predictive" or "EB".
#' @param conf.level The confidence level of the prediction intervals. Default is 0.95.
#' @param designPrior Either "predictive" (default), "conditional", or "EB".
#' If "EB", the contribution of the original study to the predictive distribution is
#' shrunken towards zero based on the evidence in the original study (with empirical Bayes).
#' @details This function computes a prediction interval and a mean estimate under a
#' specified predictive distribution of the replication effect estimate. Setting
#' \code{designPrior = "conditional"} is not recommended since this ignores the
#' uncertainty of the original effect estimate. See Patil, Peng, and Leek (2016)
#' and Pawel and Held (2020) for details.
#' @return A data frame with the following columns
#' \item{lower}{Lower limit of prediction interval,}
#' \item{mean}{Mean of predictive distribution,}
#' \item{upper}{Upper limit of prediction interval.}
#' @details \code{predictionInterval} is the vectorized version of \code{.predictionInterval_}.
#' \code{\link[base]{Vectorize}} is used to vectorize the function.
#' @references
#' Patil, P., Peng, R. D., Leek, J. T. (2016).
#' What should researchers expect when they replicate studies? A statistical view of
#' replicability in psychological science. \emph{Perspectives on Psychological Science},
#' \bold{11}, 539-544.  \doi{10.1177/1745691616646366}
#'
#' Pawel, S., Held, L. (2020). Probabilistic forecasting of replication studies.
#' \emph{PLoS ONE}. \bold{15}, e0231416. \doi{10.1371/journal.pone.0231416}
#' @author Samuel Pawel
#' @examples
#' predictionInterval(thetao = c(1.5, 2, 5), seo = 1, ser = 0.5, designPrior = "EB")
#'   
#' # plot prediction intervals for different original effect estimates
#' thetao <- c(2, 2.5, 3)
#' pi_pred <- predictionInterval(thetao = thetao, seo = 1, ser = 1)
#' pi_cond <- predictionInterval(thetao = thetao, seo = 1, ser = 1, 
#'                               designPrior = "conditional")
#' pi_eb <- predictionInterval(thetao = thetao, seo = 1, ser = 1, designPrior = "EB")
#' plot(thetao - 0.03, pi_pred$mean, xlim = c(1, 3.5), ylim = c(-2, 7), pch = 20, xaxt = "n",
#'      xlab = expression(hat(theta)[o]), ylab = expression(hat(theta)[r]), las = 2)
#' axis(side = 1, at = thetao)
#' abline(h = 0, lty = 2, col = "gray70")
#' arrows(thetao - 0.03, pi_pred$lower, thetao - 0.03, pi_pred$upper, 
#'        length = 0.02, angle = 90, code = 3)
#' points(thetao, pi_cond$mean, pch = 20, col = "darkred")
#' arrows(thetao, pi_cond$lower, thetao, pi_cond$upper, length = 0.02, angle = 90, 
#'        code = 3, col = "darkred")
#' points(thetao + 0.03, pi_eb$mean, pch = 20, col = "darkblue")
#' arrows(thetao + 0.03, pi_eb$lower, thetao + 0.03, pi_eb$upper, length = 0.02, angle = 90, 
#'        code = 3, col = "darkblue")
#' legend("topleft", c("predictive", "conditional", "EB"), title = "designPrior", 
#'        pch = 20, col = c("black", "darkred", "darkblue"), bty = "n")
#'        
#' # compute prediction intervals for replication projects
#' data("RProjects", package = "ReplicationSuccess")
#' parOld <- par(mfrow = c(2, 2))
#' for (p in unique(RProjects$project)) {
#'   data_project <- subset(RProjects, project == p)
#'   PI <- predictionInterval(thetao = data_project$fiso, seo = data_project$se_fiso, 
#'                            ser = data_project$se_fisr)
#'   PI <- tanh(PI) # transforming back to correlation scale
#'   within <- (data_project$rr < PI$upper) & (data_project$rr > PI$lower)
#'   coverage <- mean(within)
#'   color <- ifelse(within == TRUE, "#333333B3", "#8B0000B3")
#'   study <- seq(1, nrow(data_project))
#'   plot(data_project$rr, study, col = color, pch = 20, 
#'        xlim = c(-0.5, 1), xlab = expression(italic(r)[r]), 
#'        main = paste0(p, ": ", round(coverage*100, 1), "% coverage"))
#'   arrows(PI$lower, study, PI$upper, study, length = 0.02, angle = 90, 
#'          code = 3, col = color)
#'   abline(v = 0, lty = 3)
#' }
#' par(parOld)
#' @export
predictionInterval <- function(thetao,
                               seo,
                               ser,
                               tau = 0,
                               conf.level = 0.95,
                               designPrior = "predictive") {
    res <- .predictionInterval__(thetao=thetao, seo=seo, ser=ser, tau=tau,
                                 conf.level=conf.level, designPrior=designPrior)
    res <- matrix(res, ncol=3, byrow=TRUE)
    res <- data.frame(res)
    colnames(res) <- c("lower", "mean", "upper")
    res
}
