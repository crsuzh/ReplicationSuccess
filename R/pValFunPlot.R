formatPval <- function(x, break.eps = 1e-04, break.middle = 0.01, na.form = "NA", ...)
{
  format1Pval <- function (pv) {
    if (is.na(pv)) {
      na.form 
    } else if (pv < break.eps) {
      paste("<", format(break.eps, scientific=FALSE))
    } else {
      largep <- pv >= break.middle
      format(pv, digits=1+largep, nsmall=1+largep, scientific=FALSE, ...)
    }
  }
  vapply(X = x, FUN = format1Pval, FUN.VALUE = "", USE.NAMES = TRUE)
} # taken from biostatUZH

#' Plots the p-value function
#'
#' Plots the p-value function based on the effect estimates 
#' and standard errors of the original and the replication study.
#' @rdname pValFunPlot
#' @param thetao Effect estimate of the original study
#' @param thetar Effect estimate of the replication study
#' @param seo Standard error of thetao
#' @param ser Standard error of thetar
#' @param c Default is variance ratios of the original and replication
#' effect estimates. This is usually the ratio of the sample
#' size of the replication study to the sample size of the
#' original study. It is also possible to choose c as a free parameter,
#' not related to the variance ratio.
#' @param levelCI Level of the confidence interval to be added on the plot
#' @param xlim limits of the x-axis of the plot
#' @return \code{pValFunPlot} plots the p-value function
#' @details the function also returns the minimum p-value, the 
#' Q-test p-value and the two-sided p-value
#' @author Leonhard Held, Florian Gerber, Charlotte Micheloud
#' @examples
#' pValFunPlot(thetao = 2, thetar = 1.9, seo = 1, ser = 0.5, levelCI = 0.95)
#' @import ggplot2
#' @import meta
#' @export
#' 
pValFunPlot <- function(thetao, thetar,
                        seo, ser, 
                        c = NA, 
                        levelCI = 0.95,
                        xlim = c(min(0, c(thetao, thetar) - 2 * c(seo, ser)),
                                     max(0, c(thetao, thetar) + 2 * c(seo, ser)))){
  
  thetahat <-  c(thetao, thetar)
  se <- c(seo, ser)
  c <- ifelse(is.na(c), seo^2/ser^2, c)
  x <- y <- x_minP <- y_minP <- xmin <- xmax <- ymin <- ymax <- NULL
  stopifnot(
    is.numeric(levelCI),
    levelCI > 0 && levelCI < 1,
    c >= 0 || is.na(c)==TRUE, 
    is.numeric(thetahat),
    length(thetahat) > 0L,
    is.numeric(se),
    length(se) == length(thetahat) || length(se) == 1L,
    is.numeric(xlim),
    length(xlim) == 2L,
    xlim[1] < xlim[2]
  )
  
    alpha <- 1 - levelCI
    mg <- metagen(TE = thetahat, seTE = se)
    eps <- 0.0025 # for plotting error bars
    eb_height <- ifelse(alpha > 0.025, 0.025, alpha - 0.001)
    z1 <- thetahat[1]/se[1]
    z2 <- thetahat[2]/se[2]
    
    
    if(c == Inf)
      pS <- sqrt(pLimit(z1 = z1, z2 = z2))
    else
      pS <- pSceptical(zo = z1, zr = z2, c = c, alternative = "two.sided", 
                       type = "controlled")
    
    muSeq <- seq(xlim[1], xlim[2], length.out = 1000)
    createData <- function(){
      pval <- numeric()
      for(i in 1:length(muSeq))
        pval[i] <- pValueMu(thetahat = thetahat, se = se, 
                            c = c, mu = muSeq[i], 
                            alternative = "two.sided", bound = FALSE)
      
      CIs <- scepticalCI(thetao = thetao, thetar = thetar, seo = seo,
                         ser = ser, c = c,
                         alternative = "two.sided", levelCI = levelCI)
      idx <- which.min(CIs$minP[,2])
      minP_min <- CIs$minP[idx,2]
      x_minP_min <- CIs$minP[idx,1]
      df1 <- data.frame(x = muSeq,
                        y = pval,
                        x_minP = rep(x_minP_min, length(muSeq)),
                        y_minP = rep(minP_min, length(muSeq)),
                        stringsAsFactors = FALSE)
      df2 <- data.frame(xmin = unname(CIs$CI[, 1]),
                        xmax = unname(CIs$CI[, 2]),
                        y = rep(alpha, nrow(CIs$CI)), 
                        stringsAsFactors = FALSE)
      df2$ymax <- df2$y + eb_height
      df2$ymin <- df2$y - eb_height
      list(df1, df2)
    }
    data <-  createData()
    
    lines <- data[[1]] ##do.call(`rbind`, lapply(data, `[[`, i = 1L))
    errorbars <- data[[2]] ## do.call(`rbind`, lapply(data, `[[`, i = 2L))
    
    trans <- function(x) (1-x) * 100
    breaks_y1 <- sort(c(alpha, pretty(lines$y)))
    breaks_y2 <- sort(trans(c(breaks_y1)))
    transparency <- 1
    Q <- Qtest(thetahat[1], thetahat[2], se[1], se[2])
    
    if(pS^2 >= 0.0001)
      title <- paste("min p =", formatPval(lines$y_minP[1]), "Q-test: p =", formatPval(Q),
                     "c =", format(c, digits=2, nsmall=2), "two-sided p =", formatPval(pS^2))
    else
      title <- paste("min p =", formatPval(lines$y_minP[1]), "Q-test: p =", formatPval(Q),
                     "c =", format(c, digits=2, nsmall=2), "two-sided p", formatPval(pS^2))
    
    ggplot2::ggplot(data = lines, aes(x = x, y = y)) +
      geom_line(alpha = transparency) +
      geom_point(aes(x = x_minP, y = y_minP), alpha = transparency) +
      geom_point(aes(x = 0, y = pS^2), alpha = transparency) +
      geom_hline(yintercept = alpha, linetype = "dashed") +
      geom_vline(xintercept = thetahat, linetype = "dashed", col=c(5,6)) +
      geom_vline(xintercept = 0, linetype = "dashed", col="darkgrey") +
      scale_y_continuous(name = "p-value",
                         breaks = breaks_y1,
                         limits = c(0, 1),
                         sec.axis = sec_axis(trans = trans, 
                                             name = "Confidence level [%]",
                                             breaks = breaks_y2
                         )) +
      geom_segment(data = errorbars, aes(x = xmin, xend = xmax, y = y, yend = y)) +
      geom_segment(data = errorbars, aes(x = xmin, xend = xmin, y = ymin, yend = ymax)) +
      geom_segment(data = errorbars, aes(x = xmax, xend = xmax, y = ymin, yend = ymax)) +
      xlim(xlim) +
      labs(x = bquote(mu),
           color = "Heterogeneity") +
      theme_minimal() +
      theme(axis.title.y.right = element_text(angle = 90),
            legend.position = "bottom", 
            plot.title = element_text(size = 10, face = "bold", hjust=0.5), 
            # panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) + 
      ggtitle(title)
}


#' @rdname pValFunPlot

