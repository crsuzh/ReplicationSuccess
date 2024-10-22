.pEdgington_ <- function(zo = NULL,
                         zr = NULL,
                         po = NULL,
                         pr = NULL,
                         r = 1) {

  stopifnot(length(r) == 1,
            is.numeric(r),
            is.finite(r))
  if (r < 1) {
    stop("Error: r needs to be larger or equal to 1")
  }

  if (!is.null(zo) && !is.null(po)) {
    stop("Error: Only one of 'zo' or 'po' should be provided, not both.")
  }

  if (!is.null(zr) && !is.null(pr)) {
    stop("Error: Only one of 'zr' or 'pr' should be provided, not both.")
  }

  if (is.null(zo) && is.null(po)) {
    stop("Error: One of 'zo' or 'po' must be provided.")
  }

  if (is.null(zr) && is.null(pr)) {
    stop("Error: One of 'zr' or 'pr' must be provided.")
  }

  if (is.null(po)) {
    stopifnot(length(zo) == 1,
              is.numeric(zo),
              is.finite(zo))
    po <- z2p(zo, alternative = ifelse(sign(zo) == 1, "greater", "less"))
  }

  if (is.null(pr)) {
    stopifnot(length(zr) == 1,
              is.numeric(zr),
              is.finite(zr))
    pr <- z2p(zr, alternative = ifelse(sign(zo) == 1, "greater", "less"))
  }

  stopifnot(length(po) == 1,
            length(pr) == 1,
            is.numeric(po),
            is.numeric(pr),
            is.finite(po),
            is.finite(pr),
            0 < po, po <= 1,
            0 < pr, pr <= 1
            )

  wo <- 1
  wr <- wo*r
  E <- wo*po + wr*pr

  if (0 < E && E <= wo) {
    pE <- E^2/(2*wo*wr)
  } else if (wo < E && E <= wr) {
    pE <- 1/wr*(E - wo/2)
  } else { #if (wr < E && E <= wo + wr) {
    pE <- 1 + 1/(wo*wr)*(E*(wo + wr) - (wo + wr)^2/2 - E^2/2)
  }

  return(pE)
}

#' Computes Edgington's p-value
#'
#' The combined p-value with Edgington's method is
#' computed based on the one-sided p-values (or the corresponding the z-values)
#' of the original and replication study, and the ratio of the
#' weight of the replication study over the weight of the
#' original study
#' @name pEdgington
#' @rdname pEdgington
#' @author Charlotte Micheloud, Leonhard Held, Samuel Pawel
#' @param zo A vector of z-values from original studies.
#' @param zr A vector of z-values from replication studies.
#' @param po A vector of one-sided original p-values.
#' @param pr A vector of one-sided replication p-values.
#' @param r Numeric vector of ratios of replication to original weight
#'
#' @details Either \code{zo} and \code{zr}, or \code{po} and \code{pr}, must be
#'     specified.
#' @return Edgington's p-value
#' @references Held, L., Pawel, S., Micheloud, C. (2024). The assessment of
#'     replicability using the sum of p-values. \emph{Royal Society Open
#'     Science}. 11(8):11240149. \doi{10.1098/rsos.240149}
#'
#' @examples
#' ## examples from paper
#' pEdgington(po = 0.026, pr = 0.001)
#' pEdgington(po = 0.024, pr = 0.024)
#'
#' ## using z-values
#' pEdgington(zo = 1.91, zr = 1.95)
#' ## using combination of z-value and p-value
#' pEdgington(zo = 1.91, pr = 0.024)
#' @export
pEdgington <- Vectorize(.pEdgington_)
