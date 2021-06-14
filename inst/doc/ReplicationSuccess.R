## ----"knitr-options", echo = FALSE--------------------------------------------
library(knitr)
opts_chunk$set(size = "small",
               fig.height = 4,
               fig.align = "center",
               cache = FALSE,
               message = FALSE,
               warning = FALSE)

## ----"data-loading"-----------------------------------------------------------
library(ReplicationSuccess)
data("RProjects")
str(RProjects)

## computing zo, zr, c
RProjects$zo <- with(RProjects, fiso/se_fiso)
RProjects$zr <- with(RProjects, fisr/se_fisr)
RProjects$c <- with(RProjects, se_fiso^2/se_fisr^2)

## computing one-sided p-values for alternative = "greater"
RProjects$po1 <- z2p(z = RProjects$zo, alternative = "greater")
RProjects$pr1 <- z2p(z = RProjects$zr, alternative = "greater")

## ----"plot-projects", fig.height = 5------------------------------------------
## plots of effect estimates
par(mfrow = c(2, 2), las = 1, mai = rep(0.65, 4))
for (p in unique(RProjects$project)) {
    data_project <- subset(RProjects, project == p)
    significant <- ifelse(data_project$pr < 0.05, "#DF536BCC", "#00000080")
    plot(rr ~ ro, data = data_project, ylim = c(-0.5, 1), col = significant,
         xlim = c(-0.1, 1), main = p, xlab = expression(italic(r)[o]), 
         cex = 0.7, pch = 19, ylab = expression(italic(r)[r]))
    legend("topleft", legend = "replication significant", cex = 0.8, pch = 20, 
           col = "#DF536BCC", bty = "n")
    abline(h = 0, lty = 2)
    abline(a = 0, b = 1, col = "grey")
}

## -----------------------------------------------------------------------------
for (p in unique(RProjects$project)) {
    data_project <- subset(RProjects, project == p)
    significant_O <- data_project$po1 < 0.025
    significant_R <- data_project$pr1 < 0.025
    success <- significant_O & significant_R
    cat(paste0(p, ": \n"))
    cat(paste0(round(mean(significant_O)*100, 1), "% original studies significant (", 
               sum(significant_O), "/", length(significant_O), ")\n"))
    cat(paste0(round(mean(significant_R)*100, 1), "% replications significant (", 
               sum(significant_R), "/", length(significant_R), ")\n"))
    cat(paste0(round(mean(success)*100, 1), 
               "% both studies significant in the same direction (", 
               sum(success), "/", length(success), ")\n \n"))
}

## -----------------------------------------------------------------------------
sampleSizeSignificance(zo = 2.5, power = 0.8, level = 0.05, designPrior = "conditional")
sampleSizeSignificance(zo = 2.5, power = 0.8, level = 0.05, designPrior = "predictive")
sampleSizeSignificance(zo = 2.5, power = 0.8, level = 0.05, designPrior = "conditional",
                       shrinkage = 0.25)

## ----"plot-powerSignificance", echo = FALSE, fig.height = 4-------------------
po <- seq(0.0001, 0.05, 0.0001)/2

## plot power
plot(po, powerSignificance(zo = p2z(po, alternative = "one.sided"), 
                           designPrior = "conditional")*100,
     type = "l", ylim = c(0, 100), lwd = 1.5, ylab = "Power (%)", 
     xlab = expression(italic(p)[o]), las = 1, yaxt = "n")
axis(side = 2, at = seq(0, 100, 25), las = 1)
axis(side = 3, at = seq(0.0, 0.025, by = 0.005), 
     labels = c("", round(p2z(p = seq(0.005, 0.025, by = 0.005), 
                              alternative = "one.sided"), 2)))
mtext(text = expression(paste(italic(z)[o])), side = 3, line = 2)
abline(h = seq(0, 100, 25), lty = 1, col = adjustcolor("lightgrey", alpha.f = 0.3))
# abline(h = 50, lty = 1, lwd = 1.5, col = adjustcolor("lightgrey", alpha.f = 0.4))
# abline(h = 50, col = "#333333B3", lty = 3)
lines(po, powerSignificance(zo = p2z(po, alternative = "one.sided"),
                            designPrior = "predictive")*100, lwd = 2, lty = 2)
legend("topright", legend = c("conditional", "predictive"), 
       title = "Design prior", lty = c(1, 2), lwd = 1.5, bty = "n")

## ----"plot-sampleSizeSignificance", echo = FALSE, fig.height = 4--------------
## plot sample size 
plot(po, sampleSizeSignificance(zo = p2z(po, alternative = "one.sided"), 
                                designPrior = "conditional", power = 0.8),
     type = "l", ylim = c(0.5, 8), log = "y", lwd = 1.5, 
     ylab = expression(paste("Relative sample size ", n[r]/n[o])),
     xlab = expression(italic(p)[o]), las = 1, yaxt = "n")
axis(side = 3, at = seq(0.0, 0.025, by = 0.005), 
     labels = c("", round(p2z(p = seq(0.005, 0.025, by = 0.005), 
                              alternative = "one.sided"), 2)))
axis(side = 2, las = 1, at = c(0.5, 1, 2, 4, 8, 16, 32), 
     labels = c("1/2", "1", "2", "4", "8", "16", "32"))
mtext(text = expression(paste(italic(z)[o])), side = 3, line = 2)
abline(h = c(0.5, 1, 2, 4, 8), lty = 1, col = adjustcolor("lightgrey", alpha.f = 0.3))
# abline(h = 1, col = "#333333B3", lty = 3)
abline(h = 1, lty = 1, lwd = 1.5, col = adjustcolor("lightgrey", alpha.f = 0.4))
lines(po, sampleSizeSignificance(zo = p2z(po, alternative = "one.sided"), 
                                 designPrior = "predictive", power = 0.8), 
      lwd = 2, lty = 2)
legend("topleft", legend = c("conditional", "predictive"), 
       title = "Design prior", lty = c(1, 2), lwd = 1.5, bty = "n")

## ----"plot-predictionInterval", fig.height = 6--------------------------------
## compute prediction intervals for replication projects
par(mfrow = c(2, 2), las = 1, mai = rep(0.65, 4))
for (p in unique(RProjects$project)) {
    data_project <- subset(RProjects, project == p)
    pQ <- Qtest(thetao = data_project$fiso, 
                thetar = data_project$fisr,
                seo = data_project$se_fiso, 
                ser = data_project$se_fisr)
    PI <- predictionInterval(thetao = data_project$fiso, 
                             seo = data_project$se_fiso, 
                             ser = data_project$se_fisr)
    ## transforming back to correlation scale 
    PI <- tanh(PI)
    incompatible <- pQ < 0.05
    color <- ifelse(incompatible == FALSE, "#00000099", "#DF536BCC")
    study <- seq(1, nrow(data_project))
    plot(data_project$rr, study, col = color, pch = 20, cex = 0.5,
         xlim = c(-0.5, 1), xlab = expression(italic(r)[r]), ylab = "Study",
         main = paste0(p, ": ", round(mean(incompatible)*100, 0), "% incompatible"))
    arrows(PI$lower, study, PI$upper, study, length = 0.02, angle = 90, code = 3, col = color)
    abline(v = 0, lty = 3)
}

## ----"plot-pSceptical", echo = FALSE, fig.height = 4--------------------------
## data from study
morewedge <- subset(RProjects, study == "Morewedge et al. (2010), Science")

## Functions
ssv <- function(zo, so, a) {
    tau2 <- so^2/(zo^2/qnorm(p = a/2, lower.tail = FALSE)^2 - 1)
    return(tau2)
}

## Parameters and computations
options(scipen = 5)
theta_o <- morewedge$fiso
theta_r <- morewedge$fisr
so <- morewedge$se_fiso
sr <- morewedge$se_fisr
c <- so^2/sr^2
zo <- theta_o/so
zr <- theta_r/sr
alpha <- 0.05
za <- qnorm(p = alpha/2, lower.tail = FALSE)
ps <- signif(pSceptical(zo = zo, zr = zr, c = c, alternative = "two.sided", type = "nominal"), 2)
ps1 <- signif(pSceptical(zo = zo, zr = zr, c = c,  alternative = "one.sided", type = "nominal"), 2)
ps1r <- signif(pSceptical(zo = zo, zr = zr, c = c,  alternative = "one.sided", type = "golden"), 2)
tau <- sqrt(ssv(zo, so, alpha)) # sufficiently sceptical prior sd at alpha = 0.05
s2_p <- 1/(1/so^2 + 1/tau^2) # posterior variance
mu_p <- s2_p*theta_o/so^2 # posterior mean


## Plot
df <- data.frame(estimate = factor(c("Original Study", 
                                     "Posterior", 
                                     "Sceptical Prior", 
                                     "Replication Study"),
                                   levels = c("Original Study", 
                                              "Posterior", 
                                              "Sceptical Prior", 
                                              "Replication Study")),
                 x = c(1, 2, 3, 4),
                 col = c(1, 3, 2, 4),
                 theta = c(theta_o, 
                           mu_p, 
                           0, 
                           theta_r),
                 lower = c(theta_o - za*so,
                           mu_p - za*sqrt(s2_p), 
                           0 - za*tau, 
                           theta_r - za*sr),
                 upper = c(theta_o + za*so, 
                           mu_p + za*sqrt(s2_p), 
                           0 + za*tau,
                           theta_r + za*sr),
                 p = c(signif(2*pnorm(q = zo, lower.tail = FALSE), 1), 
                       NA, 
                       NA, 
                       signif(2*pnorm(q = zr, lower.tail = FALSE), 2)))

ylims <- c(-0.5, 1)
xlims <- c(0.5, 4.5)
cex <- 1
plot(x = df$x, y = df$theta, pch = " ",
     xlim = xlims, ylim = ylims, 
     xlab = "", ylab = "Effect Size", xaxt = "n", las = 1)
axis(side = 1, at =  df$x, labels =  df$estimate, 
     cex.axis = 0.9, mgp = c(3, 2, 0))
abline(h = seq(-2, 2, 0.25), lty = 1, col = adjustcolor("lightgrey", alpha.f = 0.4))
abline(h = 0, lty = 2)

## one-sided CIs
arrows(x0 = df$x[-3], x1 = df$x[-3], y0 = df$theta[-3] + 0.05, y1 = df$upper[-3],
       col = df$col[-3], code = 2, angle = 90, length = 0, lwd = 1.5)
points(x = df$x[-3], y = df$upper[-3], pch = 17, cex = 1.3*cex, col = df$col[-3]) 

## two-sided CI
arrows(x0 = df$x[3], x1 = df$x[3], y0 = df$theta[3] + 0.05, y1 = df$upper[3],
       col = df$col[3], code = 2, angle = 90, length = 0.1, lwd = 1.5)
arrows(x0 = df$x, x1 = df$x, y0 = df$theta - 0.05, y1 = df$lower,
       col = df$col, code = 2, angle = 90, length = 0.1, lwd = 1.5)

## arrows and text
points(x = df$x, y = df$theta, pch = 20, cex = cex*1.5, col = df$col)
arrows(x0 = 2.1, x1 = 2.9, y0 = 0.9, y1 = 0.9, length = 0.05, lwd = 1.2, col = "#000000B2")
text(x = 2.5, y = 0.98, labels = "reverse-Bayes", cex = 0.75, col = "#000000B2")
arrows(x0 = 3.15, x1 = 4.5, y0 = 0.9, y1 = 0.9, length = 0.05, lwd = 1.2, col = "#000000B2")
text(x = 3.8, y = 0.98, labels = "prior-data conflict assessment", cex = 0.75, col = "#000000B2")
# points(x = 2, y = 0, pch = 1, cex = 2)
arrows(x0 = 1.85, x1 = 1.98, y0 = -0.2, y1 = -0.03, length = 0.05, col = "#000000B2")
text(x = 1.85, y = -0.25, labels = "fixed at zero", col = "#000000B2", cex = 0.75)
box()


## ----"threshold-p-sceptical"--------------------------------------------------
## computing nominal, controlled, liberal, and golden thresholds for one-sided 
## sceptical p-value and significance level 0.025
(thresh_gol <- levelSceptical(level = 0.025, alternative = "one.sided", 
                              type = "golden"))
(thresh_contr <- levelSceptical(level = 0.025, alternative = "one.sided", 
                                type = "controlled"))
(thresh_nom <- levelSceptical(level = 0.025, alternative = "one.sided", 
                              type = "nominal"))
(thresh_lib <- levelSceptical(level = 0.025, alternative = "one.sided", 
                              type = "liberal"))

## ----"plot-pSceptical-projects", fig.height = 4-------------------------------
## computing one-sided sceptical p-value for replication projects
RProjects$ps <- with(RProjects, 
                     pSceptical(zo = zo, zr = zr, c = c, 
                                alternative = "one.sided", type = "golden"))
boxplot(ps ~ project, data = RProjects, las = 1, cex.axis = 0.7, ylim = c(0, 1),
        xlab = "Project", ylab = expression(italic(tilde(p))[S]), outline = FALSE,
        col = "#0000000D")
abline(h = alpha/2, lty = 1, col = 4)
axis(side = 4, at = alpha/2, col.axis = 4, col = 4, las = 1, cex.axis = 0.5)
stripchart(ps ~ project, data = RProjects, vertical = TRUE, add = TRUE,
           pch = 20, method = "jitter", jitter = 0.3, cex = 1, col = "#00000099")

for (p in unique(RProjects$project)) {
    data_project <- subset(RProjects, project == p)
    cat(paste0(p, ": \n"))
    success_scept <- (data_project$ps < 0.025)
    cat(paste0(round(mean(success_scept)*100, 2), 
               "% smaller than 0.025 (one-sided sceptical p-value) \n"))
    success_tradit <- (data_project$po1 < 0.025) & (data_project$pr1 < 0.025)
    cat(paste0(round(mean(success_tradit)*100, 2), 
               "% smaller than 0.025 (both one-sided traditional p-values) \n"))
    if(sum(success_scept != success_tradit) > 0){
        discrep <- data_project[(success_scept != success_tradit), 
                                c("ro", "rr", "c", "po1", "pr1", "ps")]
        ## print effect estimates, 1sided p-values, and c of discrepant studies
        cat("Discrepant studies: \n")
        print(signif(discrep, 2), row.names = FALSE)
  }
  cat("\n \n")
}

## -----------------------------------------------------------------------------
sampleSizeReplicationSuccess(zo = 2.5, power = 0.8, level = 0.025,
                             alternative = "one.sided",
                             designPrior = "conditional", 
                             type = "golden")

sampleSizeReplicationSuccess(zo = 2.5, power = 0.8, level = 0.025,
                             alternative = "one.sided",
                             designPrior = "predictive", 
                             type = "golden")

## ----"plot-powerReplicationSuccess", echo = FALSE, fig.height = 4-------------
## plot power
po <- seq(0.001, 0.05, length.out = 100)/2
plot(po, powerReplicationSuccess(zo = p2z(po, alternative  = "one.sided"), 
                                 designPrior = "conditional",
                                 level = 0.025, 
                                 alternative = "one.sided", 
                                 type = "golden")*100,
     type = "l", ylim = c(0, 100), lwd = 1.5, ylab = "Power (%)", las = 1,
     xlab = expression(italic(p)[o]), yaxt = "n")
abline(h = seq(0, 100, 25), lty = 1, col = adjustcolor("lightgrey", alpha.f = 0.3))
# abline(h = 50, lty = 1, lwd = 1.5, col = adjustcolor("lightgrey", alpha.f = 0.4))
axis(side = 2, at = seq(0, 100, 25), las = 1)
axis(side = 3, at = seq(0.0, 0.025, by = 0.005), 
     labels = c("", round(p2z(p = seq(0.005, 0.025, by = 0.005), 
                              alternative = "one.sided"), 2)))
mtext(text = expression(paste( italic(z)[o])), side = 3, line = 2)
lines(po, powerReplicationSuccess(zo = p2z(po, alternative = "one.sided"), 
                                  designPrior = "predictive",
                                  level = 0.025, 
                                  alternative = "one.sided", 
                                  type = "golden")*100, 
      lwd = 2, lty = 2)
legend("topright", legend = c("conditional", "predictive"), 
       title = "Design prior", lty = c(1, 2), lwd = 1.5, bty = "n")
# abline(h = 50, lty = 3)

## ----"plot-sampleSizeReplicationSuccess", echo = FALSE, fig.height = 4--------
# po <- seq(0.0001, 0.05, 0.0001)
po <- seq(0.001, 0.05, length.out = 100)/2

## plot sample size 
plot(po, 
     sampleSizeReplicationSuccess(zo = p2z(po, alternative = "one.sided"), 
                                  power = 0.8,
                                  level = 0.025,
                                  designPrior = "conditional",
                                  alternative = "one.sided", 
                                  type = "golden"),
     type = "l", log = "y", lwd = 1.5, las = 1, ylim = c(0.5, 20),
     ylab = expression(paste("Relative sample size ", n[r]/n[o])),
     xlab = expression(italic(p)[o]), yaxt = "n")
axis(side = 2, las = 1, at = c(0.5, 1, 2, 4, 8, 16, 32), 
     labels = c("1/2", "1", "2", "4", "8", "16", "32"))
# abline(h = 1, lty = 3)
abline(h = c(0.5, 1, 2, 4, 8, 16, 32), lty = 1, col = adjustcolor("lightgrey", alpha.f = 0.3))
abline(h = 1, lty = 1, lwd = 1.5, col = adjustcolor("lightgrey", alpha.f = 0.4))
axis(side = 3, at = seq(0.0, 0.025, by = 0.005), 
     labels = c("", round(p2z(p = seq(0.005, 0.025, by = 0.005), 
                              alternative = "one.sided"), 2)))
mtext(text = expression(paste(italic(z)[o])), side = 3, line = 2)
lines(po, sampleSizeReplicationSuccess(zo = p2z(po, alternative = "one.sided"), 
                                  power = 0.8,
                                  level = 0.025,
                                  designPrior = "predictive",
                                  alternative = "one.sided", 
                                  type = "golden"), 
      lwd = 2, lty = 2)
legend("topleft", legend = c("conditional", "predictive"), 
       title = "Design prior", lty = c(1, 2), lwd = 1.5, bty = "n")

## ----echo = FALSE-------------------------------------------------------------
d <- morewedge$fisr/morewedge$fiso
dminrs <- effectSizeReplicationSuccess(zo = morewedge$zo, c = morewedge$c)
dminsign <- effectSizeSignificance(zo = morewedge$zo, c = morewedge$c)

## ----echo = FALSE, fig.height = 3.5, eval = FALSE-----------------------------
#  data("SSRP")
#  intpow_cond <- with(SSRP, powerSignificanceInterim(zo = fiso/se_fiso, zi = fisi/se_fisi, c = nr/no,
#                                                     f = ni/nr, designPrior = "conditional"))
#  intpow_pred <- with(SSRP, powerSignificanceInterim(zo = fiso/se_fiso, zi = fisi/se_fisi, c = nr/no,
#                                                     f = ni/nr, designPrior = "predictive"))
#  plot(intpow_cond*100, intpow_pred*100,
#       xlab = "Conditional power (in %)",
#       ylab = "Predictive power (in %)",
#       pch = 20,
#       cex = 1.2,
#       xlim = c(80, 100),
#       ylim = c(0, 100),
#       yaxt = "n", las = 1,
#       col = "#00000099")
#  axis(side = 2, at = seq(0, 100, 25), las = 1)
#  abline(a = 0, b = 1, col = "grey")

## ----"shrinkage", echo = FALSE, fig.height = 3.5------------------------------
zo <- seq(0, 4, 0.01)
s <- pmax(1 - 1/zo^2, 0)
plot(zo, s, type = "l", ylim = c(0, 1), las = 1,
     xlab = expression(paste(italic(z)[o])), ylab = "Shrinkage factor",
     yaxt = "n")
axis(side = 2, at = seq(0, 1, 0.25), las = 1)
axis(side = 3, at = seq(0, 4, by = 1), 
     labels = c(signif(z2p(seq(0, 3, by = 1), alternative = "one.sided"), 2), 
                signif(z2p(4, alternative = "one.sided"), 1)))
abline(h = seq(0, 1, 0.25), lty = 1, col = adjustcolor("lightgrey", alpha.f = 0.3))
mtext(text = expression(italic(p)[o]), side = 3, line = 2)

