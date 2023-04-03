# target calculates the T1E rate for a certain alternative, level and
# variance ratio and substracts the target T1E rate.
# Used in levelSceptical to calculate the controlled level for
# replication success


target <- function(alphalevel, alternative = alternative, c = c, targetT1E){
  myT1E <- T1EpSceptical(alternative = alternative, level = alphalevel, c = c,
                         type = "nominal")
  myT1E - targetT1E
}


.levelSceptical_ <- function(level,
                           c = NA,
                           alternative = c("one.sided", "two.sided"), 
                           type = c("golden", "nominal", "controlled")){


  stopifnot(is.numeric(level),
            length(level) >= 1,
            is.finite(level),
            0 < level, level < 1,

            !is.null(alternative))

  targetT1E <- level^2 # because we only consider one and two sided
  alternative <- match.arg(alternative)

  stopifnot(!is.null(type))
  type <- match.arg(type)

  if (type == "nominal") {
    res <- level
  # } else if (type == "liberal") {
  #   res <- pIntrinsic(p = level, alternative = alternative, type = "Held")
  } else if (type == "controlled") {
    mylower <- sqrt(targetT1E)
    if (alternative=="one.sided") {
      myupper <- 0.5
    } else if (alternative=="two.sided") {
      myupper <- 1-.Machine$double.eps^0.25
  }
    result <- stats::uniroot(target, lower = mylower, upper = myupper,
                      alternative = alternative, c = c,
                      targetT1E = targetT1E)
    res <- result$root
  } else if (type == "golden") {
    res <- pIntrinsic(p = level, alternative = alternative, type = "Matthews")
  }
  return(res)
}
#' Computes the replication success level
#'
#' The replication success level is computed based on the specified
#' alternative and recalibration type.
#' @param level Threshold for the calibrated sceptical p-value.
#'  Default is 0.025.
#' @param c The variance ratio. Only required when \code{type = } "controlled".
#' @param alternative Specifies if \code{level} is "one.sided" (default) or
#'  "two.sided". If "one-sided",
#' then a one-sided replication success level is computed.
#' @param type Type of recalibration. Can be either "golden" (default), "nominal" (no recalibration),
#'  or "controlled". "golden" ensures that for an original study just significant at
#' the specified \code{level}, replication success is only possible for 
#' replication effect estimates larger than the original one.
#' "controlled" ensures exact overall Type-I error control at level \code{level}^2.
#' @return Replication success levels
#' @details \code{levelSceptical} is the vectorized version of
#' the internal function \code{.levelSceptical_}.
#' \code{\link[base]{Vectorize}} is used to vectorize the function.
#' @references Held, L. (2020). A new standard for the analysis and design of replication studies (with discussion).
#' \emph{Journal of the Royal Statistical Society: Series A (Statistics in Society)}, \bold{183}, 431-448.
#' \doi{10.1111/rssa.12493}
#'
#' Held, L. (2020). The harmonic mean chi-squared test to substantiate scientific findings.
#' \emph{Journal of the Royal Statistical Society: Series C (Applied Statistics)}, \bold{69}, 697-708.
#' \doi{10.1111/rssc.12410}
#'
#' Held, L., Micheloud, C., Pawel, S. (2022). The assessment of replication
#' success based on relative effect size.
#' \emph{The Annals of Applied Statistics}, \bold{16}, 706-720.
#' \doi{10.1214/21-AOAS1502}
#'
#' Micheloud, C., Balabdaoui, F., Held, L. (2023).
#' Beyond the two-trials rule: Type-I error control and sample size planning
#' with the sceptical p-value. \url{https://arxiv.org/abs/2207.00464}
#' @author Leonhard Held
#' @examples
#' levelSceptical(level = 0.025, alternative = "one.sided", type = "nominal")
#' levelSceptical(level = 0.025, alternative = "one.sided", type = "controlled", c = 1)
#' levelSceptical(level = 0.025, alternative = "one.sided", type = "golden")
#' @export
levelSceptical <- Vectorize(.levelSceptical_)

