#' Computes the level for the sceptical p-value
#'
#' The level for the sceptical p-value is computed based on the specified
#' alternative and calibration type.
#' @param level Numeric vector of required replication success levels.
#' @param alternative Either "one.sided" (default) or "two.sided".
#' Specifies if the replication success level is one-sided or two-sided. If the replication success level is one-sided,
#' then a one-sided level for the sceptical p-value is computed.
#' @param type The calibration type can be either "golden" (default), "nominal", "liberal", or "controlled".
#' \code{type} = "golden" ensures that for an original study just significant at the specified \code{level},
#' replication success is only possible if the replication effect estimate is larger than the original one.
#' If \code{type =} "controlled", the type-I error rate is equal to \code{level}^2 (for \code{alternative} ="two.sided") or
#' 2 \eqn{\times}{*}\code{level}^2 (for \code{alternative} = "one.sided") if the variance ratio is equal to 1.
#' The type \code{"nominal"} ensures that the type-I error rate is always smaller
#' than \code{level}^2. Significance of both the original and replication study
#' at \code{level} is then a necessary but not sufficient requirement for replication success.
#' If \code{type} is "liberal" then significance of both studies is a
#' sufficient requirement for replication success if the variance ratio is equal to 1.
#' @return Levels for the sceptical p-value.
#' @references Held, L. (2020). A new standard for the analysis and design of replication studies (with discussion).
#' \emph{Journal of the Royal Statistical Society: Series A (Statistics in Society)}, \bold{183}, 431-448.
#' \doi{10.1111/rssa.12493}
#'
#' Held, L. (2020). The harmonic mean chi-squared test to substantiate scientific findings.
#' \emph{Journal of the Royal Statistical Society: Series C (Applied Statistics)}, \bold{69}, 697-708.
#' \doi{10.1111/rssc.12410}
#'
#' Held, L., Micheloud, C., Pawel, S. (2021). The assessment of replication
#' success based on relative effect size. \url{https://arxiv.org/abs/2009.07782}
#' @author Leonhard Held
#' @examples
#' levelSceptical(level = 0.025, alternative = "one.sided", type = "nominal")
#' levelSceptical(level = 0.025, alternative = "one.sided", type = "liberal")
#' levelSceptical(level = 0.025, alternative = "one.sided", type = "controlled")
#' levelSceptical(level = 0.025, alternative = "one.sided", type = "golden")
#' @export
levelSceptical <- function(level, 
                           alternative = c("one.sided", "two.sided"), 
                           type = c("golden", "nominal", "liberal", "controlled")){

    stopifnot(is.numeric(level),
              length(level) >= 1,
              is.finite(level),
              0 < level, level < 1,
              
              !is.null(alternative))
    alternative <- match.arg(alternative)

    stopifnot(!is.null(type))
    type <- match.arg(type)
        
    if(type == "nominal")
        res <- level
    
    if(type == "liberal")
        res <- pIntrinsic(p = level, alternative = alternative, type = "Held")
    
    if(type == "controlled"){
        if (alternative == "two.sided") {
            t1 <- level^2 ## level is two-sided significance level
            ## t1 <- (2*level)^2 ## level is a one-sided significance level
            res <- 2*(1 - pnorm(q = qnorm(p = 1 - t1/2)/2))
        } 
        if (alternative == "one.sided") {
            ## t1 <- level*(level/2) ## level is a two-sided significance level
            t1 <- 2*level^2 ## level is a one-sided significance level
            res <- 1 - pnorm(q = qnorm(p = 1 - t1)/2)
        }
    }
    if(type == "golden"){
        res <- pIntrinsic(p = level, alternative = alternative, type = "Matthews")
    }
    return(res)
}
