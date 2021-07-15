#' Data from the Social Sciences Replication Project
#'
#' @description Data from the \emph{Social Sciences Replication Project} (SSRP)
#'     including the details of the interim analysis. The variables are as follows:
#' \describe{
#' \item{\code{study}}{Study identifier, usually names of authors
#' from original study}
#' \item{\code{ro}}{Effect estimate of original study on correlation scale}
#' \item{\code{ri}}{Effect estimate of replication study at the interim analysis
#' on correlation scale}
#' \item{\code{rr}}{Effect estimate of replication study at the final analysis
#' on correlation scale}
#' \item{\code{fiso}}{Effect estimate of original study transformed to
#' Fisher-z scale}
#' \item{\code{fisi}}{Effect estimate of replication study at
#' the interim analysis transformed to Fisher-z scale}
#' \item{\code{fisr}}{Effect estimate of replication study at the final analysis
#' transformed to Fisher-z scale}
#' \item{\code{se_fiso}}{Standard error of Fisher-z transformed effect estimate
#' of original study}
#' \item{\code{se_fisi}}{Standard error of Fisher-z transformed effect estimate
#' of replication study at the interim analysis}
#' \item{\code{se_fisr}}{Standard error of Fisher-z transformed effect estimate
#' of replication study at the final analysis}
#' \item{\code{no}}{ Sample size in original study}
#' \item{\code{ni}}{Sample size in replication study at the interim analysis}
#' \item{\code{nr}}{Sample size in replication study at the final analysis}
#' \item{\code{po}}{Two-sided p-value from significance test of effect estimate
#' from original study}
#' \item{\code{pi}}{Two-sided p-value from significance test of effect
#' estimate from replication study at the interim analysis}
#' \item{\code{pr}}{Two-sided p-value from significance test of effect estimate
#' from replication study at the final analysis}
#' \item{\code{n75}}{Sample size calculated to have 90\% power in replication study
#' to detect 75\% of the original effect size (expressed as the correlation
#' coefficient r)}
#' \item{\code{n50}}{Sample size calculated to have 90\% power in replication
#' study to detect 50\% of the original effect size (expressed as the correlation
#' coefficient r)}
#' }
#'
#' @details Two-sided p-values were calculated assuming normality of Fisher-z
#'     transformed effect estimates.A two-stage procedure was used for the
#'     replications. In stage 1, the authors had 90\% power to detect 75\% of
#'     the original effect size at the 5\% significance level in a two-sided
#'     test. If the original result replicated in stage 1 (two-sided P-value <
#'     0.05 and effect in the same direction as in the original study), the data
#'     collection was stopped. If not, a second data collection was carried out
#'     in stage 2 to have 90\% power to detect 50\% of the original effect size
#'     for the first and the second data collections pooled. \code{n75} and
#'     \code{n50} are the planned sample sizes calculated to reach 90\% power in
#'     stage 1 and 2, respectively. They sometimes differ from the sample sizes
#'     that were actually collected (\code{ni} and \code{nr}, respectively). See
#'     supplementary information of Camerer et al. (2018) for details.
#' @name SSRP
#' @docType data
#' @usage data(SSRP)
#' @format A data frame with 21 rows and 18 variables
#' @source \url{https://osf.io/abu7k}
#' @references Camerer, C. F., Dreber, A., Holzmeister, F., Ho, T.-H., Huber,
#'     J., Johannesson, M., ... Wu, H. (2018). Evaluating the replicability of
#'     social science experiments in Nature and Science between 2010 and 2015.
#'     \emph{Nature Human Behaviour}, \bold{2}, 637-644.
#'     \doi{10.1038/s41562-018-0399-z}
#' @seealso \code{\link{RProjects}}
#' @examples
#' # plot of the sample sizes
#' plot(ni ~ no, data = SSRP, ylim = c(0, 2500), xlim = c(0, 400),
#'      xlab = expression(n[o]), ylab = expression(n[i]))
#' abline(a = 0, b = 1, col = "grey")
#'
#'
#' plot(nr ~ no, data = SSRP, ylim = c(0, 2500), xlim = c(0, 400),
#'      xlab = expression(n[o]), ylab = expression(n[r]))
#' abline(a = 0, b = 1, col = "grey")
#'
#'
#' @keywords data
"SSRP"

#' Data from four large-scale replication projects
#'
#' @description Data from \emph{Reproduciblity Project Psychology} (RPP),
#'     \emph{Experimental Economics Replication Project} (EERP), \emph{Social
#'     Sciences Replication Project} (SSRP), \emph{Experimental Philosophy
#'     Replicability Project} (EPRP). The variables are as follows:
#' \describe{
#' \item{\code{study}}{Study identifier, usually names of authors
#' from original study}
#' \item{\code{project}}{Name of replication project}
#' \item{\code{ro}}{Effect estimate of original study on correlation scale}
#' \item{\code{rr}}{Effect estimate of replication study on correlation scale}
#' \item{\code{fiso}}{Effect estimate of original study transformed to
#' Fisher-z scale}
#' \item{\code{fisr}}{Effect estimate of replication study transformed
#' to Fisher-z scale}
#' \item{\code{se_fiso}}{Standard error of Fisher-z transformed effect estimate
#' of original study}
#' \item{\code{se_fisr}}{Standard error of Fisher-z transformed effect estimate
#' of replication study}
#' \item{\code{po}}{Two-sided p-value from significance test of effect estimate
#' from original study}
#' \item{\code{pr}}{Two-sided p-value from significance test of effect estimate
#' from replication study}
#' \item{\code{no}}{Sample size in original study}
#' \item{\code{nr}}{Sample size in replication study}
#' }
#'
#' @details Two-sided p-values were calculated assuming normality of Fisher-z
#'     transformed effect estimates. From the RPP only the \emph{meta-analytic
#'     subset} is included, which consists of 73 out of 100 study pairs for
#'     which the standard error of the z-transformed correlation coeffient can
#'     be computed. For the RPP also sample sizes were recalculated from
#'     standard errors of Fisher z-transformed correlation coefficients. From
#'     the EPRP only 31 out of 40 study pairs are included where effective
#'     sample size for original and replication study are available
#'     simultaneously. For details about how the the data was preprocessed see
#'     supplement S1 of Pawel and Held (2020).
#' @name RProjects
#' @docType data
#' @usage data(RProjects)
#' @format A data frame with 143 rows and 13 variables
#' @source RPP: \url{https://github.com/CenterForOpenScience/rpp/}
#'
#' EERP: \url{https://osf.io/pnwuz/}
#'
#' SSRP: \url{https://osf.io/abu7k}
#'
#' EPRP: \url{https://osf.io/4ewkh/}
#' @references Camerer, C. F., Dreber, A., Forsell, E., Ho, T.-H., Huber, J.,
#'     Johannesson, M., ... Hang, W. (2016). Evaluating replicability of
#'     laboratory experiments in economics. \emph{Science}, \bold{351}, 1433-1436.
#'     \doi{10.1126/science.aaf0918}
#'
#'     Camerer, C. F., Dreber, A., Holzmeister, F., Ho, T.-H., Huber, J.,
#'     Johannesson, M., ... Wu, H. (2018). Evaluating the replicability of
#'     social science experiments in Nature and Science between 2010 and 2015.
#'     \emph{Nature Human Behaviour}, \bold{2}, 637-644.
#'     \doi{10.1038/s41562-018-0399-z}
#'
#' Cova, F., Strickland, B., Abatista, A., Allard, A., Andow, J., Attie, M., ...
#' Zhou, X. (2018). Estimating the reproducibility of experimental philosophy.
#' \emph{Review of Philosophy and Psychology}. \doi{10.1007/s13164-018-0400-9}
#'
#' Open Science Collaboration. (2015). Estimating the reproducibility of
#' psychological science. \emph{Science}, \bold{349}, aac4716.
#' \doi{10.1126/science.aac4716}
#'
#' Pawel, S., Held, L. (2020). Probabilistic forecasting of replication studies.
#' \emph{PLoS ONE}. \bold{15}, e0231416. \doi{10.1371/journal.pone.0231416}
#'
#' @seealso \code{\link{SSRP}}
#' @examples
#' data("RProjects", package = "ReplicationSuccess")
#'
#' ## Computing key quantities
#' RProjects$zo <- RProjects$fiso/RProjects$se_fiso
#' RProjects$zr <- RProjects$fisr/RProjects$se_fisr
#' RProjects$c <- RProjects$se_fiso^2/RProjects$se_fisr^2
#'
#' ## Computing one-sided p-values for alternative = "greater"
#' RProjects$po1 <- z2p(z = RProjects$zo, alternative = "greater")
#' RProjects$pr1 <- z2p(z = RProjects$zr, alternative = "greater")
#'
#' ## Plots of effect estimates
#' parOld <- par(mfrow = c(2, 2))
#' for (p in unique(RProjects$project)) {
#'   data_project <- subset(RProjects, project == p)
#'   plot(rr ~ ro, data = data_project, ylim = c(-0.5, 1),
#'        xlim = c(-0.5, 1), main = p, xlab = expression(italic(r)[o]),
#'        ylab = expression(italic(r)[r]))
#'   abline(h = 0, lty = 2)
#'   abline(a = 0, b = 1, col = "grey")
#' }
#' par(parOld)
#' 
#' ## Plots of peer beliefs
#' RProjects$significant <- factor(RProjects$pr < 0.05,
#'                                 levels = c(FALSE, TRUE),
#'                                 labels = c("no", "yes"))
#' parOld <- par(mfrow = c(1, 2))
#' for (p in c("Experimental Economics", "Social Sciences")) {
#'   data_project <- subset(RProjects, project == p)
#'   boxplot(pm_belief ~ significant, data = data_project, ylim = c(0, 1),
#'           main = p, xlab = "Replication effect significant", ylab = "Peer belief")
#'   stripchart(pm_belief ~ significant, data = data_project, vertical = TRUE,
#'              add = TRUE, pch = 1, method = "jitter")
#' }
#' par(parOld)
#'
#' ## Computing the sceptical p-value
#' ps <- with(RProjects, pSceptical(zo = fiso/se_fiso,
#'                                  zr = fisr/se_fisr,
#'                                  c = se_fiso^2/se_fisr^2))
#' @keywords data
"RProjects"
