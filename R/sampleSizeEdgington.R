.sampleSizeEdgington_ <- function(zo = NULL,
                                  po = NULL,
                                  r = 1,
                                  power,
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
    warning(paste("Replication success impossible for po >= ", round(sqrt(2*r)*level, 3)))
    c <- NaN
  } else {
    levelEdg <- sqrt(2/r)*level - po/r
    c <- sampleSizeSignificance(zo = zo, power = power, level = levelEdg,
                                designPrior = designPrior, shrinkage = shrinkage,
                                h = 0, alternative = "one.sided")
  }

  return(c)
}

#' Computes the required relative sample size to achieve replication success
#' with Edgington's method based on power
#'
#' The relative sample size to achieve replication success with Edgington's
#' method is computed based on the z-value (or one-sided p-value) of the
#' original study, the significance level, the ratio of the weight of the
#' replication study over the weight of the original study, the design prior and
#' the power.
#' @name sampleSizeEdgington
#' @rdname sampleSizeEdgington
#' @author Charlotte Micheloud, Leonhard Held, Samuel Pawel
#' @param zo Numeric vector of z-values from original studies.
#' @param po Numeric vector of original one-sided p-values
#' @param power Power to achieve replication success.
#' @param level One-sided significance level. Default is 0.025.
#' @param designPrior Either "conditional" (default) or "predictive".
#' @param r Numeric vector of ratios of replication to original weight.
#' @param shrinkage Numeric vector with values in [0,1). Defaults to 0.
#'     Specifies the shrinkage of the original effect estimate towards zero,
#'     e.g., the effect is shrunken by a factor of 25\% for \code{shrinkage =
#'     0.25}. Is only taken into account if the \code{designPrior} is
#'     "conditional" or "predictive".
#' @return The relative sample size to achieve replication success with
#'     Edgington's method. If impossible to achieve the desired power for
#'     specified inputs \code{NaN} is returned.
#' @details Either \code{zo} or \code{po} must be specified.
#' @references Held, L., Pawel, S., Micheloud, C. (2024). The assessment of
#'     replicability using the sum of p-values. \emph{Royal Society Open
#'     Science}. 11(8):11240149. \doi{10.1098/rsos.240149}
#' @examples
#' ## partially recreate Figure 5 from paper
#' poseq <- exp(seq(log(0.00001), log(0.025), length.out = 100))
#' cseq <- sampleSizeEdgington(po = poseq, power = 0.8)
#' cseqSig <- sampleSizeSignificance(zo = p2z(p = poseq, alternative = "one.sided"),
#'                                   power = 0.8)
#' plot(poseq, cseq/cseqSig, log = "x", xlim = c(0.00001, 0.035), ylim = c(0.9, 1.3),
#'      type = "l", las = 1, xlab = "Original p-value", ylab = "Sample size ratio")
#' @export
sampleSizeEdgington <- Vectorize(.sampleSizeEdgington_)
