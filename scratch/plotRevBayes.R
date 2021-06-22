plotRevBayes <- function(thetao, thetar, se_thetao, se_thetar, 
                         alpha = 0.025, alternative = "one.sided",
                         info = TRUE, cex.axis = 0.8, cex = 1.5) {
  ## computing key quantities
  direction <- sign(thetao)
  zo <- thetao/se_thetao
  zr <- thetar/se_thetar
  c <- se_thetao^2/se_thetar^2
  po <- z2p(z = zo, alternative = alternative)
  pr <- z2p(z = zr, alternative = alternative)
  zalpha <- p2z(p = alpha, alternative = alternative)
  tau2ss <- se_thetao^2/(zo^2/zalpha^2 - 1) # sufficiently sceptical prior variance
  sigma2post <- 1/(1/se_thetao^2 + 1/tau2ss) # posterior variance
  mupost <- sigma2post*thetao/se_thetao^2 # posterior mean
  
  
  ## creating data frame with every quantity for plotting
  zalphaprior <- qnorm(p = ifelse(alternative == "one.sided", alpha*2, alpha),
                       lower.tail = FALSE)
  uppersymbol <- lowersymbol <- 95 # dash symbol
  if (alternative == "one.sided") {
    if (direction == 1) uppersymbol <- 24 # uparrow symbol
    if (direction == -1) lowersymbol <- 25 # downarrow symbol
  }
  plotdf <- data.frame(x = c(1, 2, 3, 4),
                       labelx = c("Original Study", 
                                  "Posterior", 
                                  "Sufficiently Sceptical Prior",
                                  "Replication Study"),
                       lower = c(thetao - zalpha*se_thetao,
                                 mupost - zalpha*sqrt(sigma2post),
                                 0 - zalphaprior*sqrt(tau2ss),
                                 thetar - zalpha*se_thetar),
                       mu = c(thetao,
                              mupost,
                              0,
                              thetar),
                       upper = c(thetao + zalpha*se_thetao,
                                 mupost + zalpha*sqrt(sigma2post),
                                 0 + zalphaprior*sqrt(tau2ss),
                                 thetar + zalpha*se_thetar),
                       uppersymbol = c(uppersymbol, uppersymbol, 95, uppersymbol),
                       lowersymbol = c(lowersymbol, lowersymbol, 95, lowersymbol),
                       col = c("black", "#2297E6", "#DF536B", "#61D04F"))
 
  ## create plot
  ylims <- c(min(plotdf$lower), max(plotdf$upper))*1.1
  xlims <- c(0.5, 4.5)
  plot(x = plotdf$x, y = plotdf$mu, pch = "",
       xlim = xlims, ylim = ylims, 
       xlab = "", ylab = "Effect Size", xaxt = "n", las = 1)
  graphics::axis(side = 1, at = plotdf$x, labels = plotdf$labelx, 
                 cex.axis = cex.axis)
  graphics::abline(h = 0, lty = 2)
  graphics::arrows(x0 = plotdf$x, x1 = plotdf$x, y0 = plotdf$mu, 
                   y1 = plotdf$upper, length = 0, col = plotdf$col)
  graphics::points(x = plotdf$x, y = plotdf$upper, pch = plotdf$uppersymbol,
                   bg = plotdf$col, cex = cex, col = plotdf$col)
  graphics::arrows(x0 = plotdf$x, x1 = plotdf$x, y0 = plotdf$mu, 
                   y1 = plotdf$lower, length = 0, col = plotdf$col)
  graphics::points(x = plotdf$x, y = plotdf$lower, pch = plotdf$lowersymbol, 
                   bg = plotdf$col, cex = cex, col = plotdf$col)
  graphics::points(x = plotdf$x, y = plotdf$mu, pch = 20, cex = cex*1.5, 
                   col = plotdf$col)
  
  ## add p-values and effects if desired
  if (info == TRUE) {
    graphics::text(x = 0.6, y = ylims[1] + (diff(ylims))*0.9, 
                   labels = bquote(hat(theta)["o"] == .(round(thetao, 2))))
    graphics::text(x = 3.6, y = ylims[1] + (diff(ylims))*0.9, 
                   labels = bquote(hat(theta)["r"] == .(round(thetar, 2))))
    graphics::text(x = 0.65, y = ylims[1] + (diff(ylims))*0.8,
                   labels = bquote(italic(p)["o"] == .(round(po, 3))))
    graphics::text(x = 3.65, y = ylims[1] + (diff(ylims))*0.8,
                   labels = bquote(italic(p)["r"] == .(round(pr, 3))))
  }
  
}
