.powerEdgington_ <- function(zo = NULL,
                             po = NULL,
                             r = 1,
                             c = 1,
                             level = 0.025,
                             designPrior = "conditional",
                             shrinkage = 0) {

  stopifnot(length(r) == 1,
            is.numeric(r),
            is.finite(r))
  if (r < 1) {
    stop("Error: r needs to be larger or equal to 1")
  }

  if (is.null(zo) && is.null(po)) {
    stop("Error: One of 'zo' or 'po' must be provided.")
  }


  if (is.null(po)) {
    stopifnot(length(zo) == 1,
              is.numeric(zo),
              is.finite(zo))
    po <- z2p(zo, alternative = ifelse(sign(zo) == 1, "greater", "less"))
  }

  if (is.null(zo)) {
    stopifnot(length(po) == 1,
              is.numeric(po),
              is.finite(po),
              0 < po, po <= 1
              )
    zo <- p2z(po, alternative = "one.sided")
  }

  if (po >= sqrt(2*r)*level) {
    power <- 0
  } else {
    levelEdg <- sqrt(2/r)*level - po/r

    power <- powerSignificance(zo = zo, c = c, level = levelEdg,
                               designPrior = designPrior,
                               alternative = "one.sided", h = 0,
                               shrinkage = shrinkage)
  }
  return(power)
}

#' Computes the power for replication success with Edgington's method
#'
#' The power with Edgington's method is computed based on the result of
#' the original study (z-value or one-sided p-value),
#' the corresponding variance ratio, and the ratio of the
#' weight of the replication study over the weight of the
#' original study
#' @name powerEdgington
#' @rdname powerEdgington
#' @author Charlotte Micheloud, Leonhard Held, Samuel Pawel
#' @param zo Numeric vector of z-values from original studies.
#' @param po Numeric vector of original one-sided p-values
#' @param c Numeric vector of variance ratios of the original and replication
#' effect estimates. This is usually the ratio of the sample
#' size of the replication study to the sample size of the original study.
#' @param level One-sided significance level. Default is 0.025.
#' @param designPrior Either "conditional" (default) or "predictive".
#' @param r Numeric vector of ratios of replication to original weight.
#' @param shrinkage Numeric vector with values in [0,1). Defaults to 0.
#' Specifies the shrinkage of the original effect estimate towards zero, e.g.,
#' the effect is shrunken by a factor of 25\% for \code{shrinkage = 0.25}.
#' @return The power with Edgington's method
#' @details Either \code{zo} or \code{po} must be specified.
#' @references Held, L., Pawel, S., Micheloud, C. (2024). The assessment of
#'     replicability using the sum of p-values. \emph{Royal Society Open
#'     Science}. 11(8):11240149. \doi{10.1098/rsos.240149}
#' @examples
#' powerEdgington(po = 0.025, level = 0.025, c = 1.4)
#' @export
powerEdgington <- Vectorize(.powerEdgington_)
