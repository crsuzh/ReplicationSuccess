try(detach("package:ReplicationSuccess", unload=TRUE), silent=TRUE)
system("make -C .. lib")
library("ReplicationSuccess", lib.loc = "../lib/")
library("ReplicationSuccess", lib.loc = "../lib/v_old")
library("ReplicationSuccess")
library(devtools)

## example with confidence region consisting of disjunct intervals
thetahat2 <- c(-3.7, 2.1, 2.5) 
se2 <- c(1.5, 2.2, 3.1)
level <- 0.95; alpha <- 1 - level
muSeq <- seq(-7, 6, length.out = 1000)
pValueSeq <- hMeanChiSqMu(thetahat = thetahat2, se = se2,
                          alternative = "none", mu = muSeq)
(CIs <- hMeanChiSqCI(thetahat = thetahat2, se = se2, alternative = "none"))
(CIs <- hMeanChiSqCI(thetahat = thetahat2, se = se2, alternative = "none", useUnirootAll = TRUE))

plot(x = muSeq, y = pValueSeq, type = "l", panel.first = grid(lty = 1),
     xlab = expression(mu), ylab = "p-value")
abline(v = thetahat2, h = alpha, lty = 2)
arrows(x0 = CIs[, 1], x1 = CIs[, 2], y0 = alpha,
       y1 = alpha, col = "darkgreen", lwd = 3, angle = 90, code = 3)
     

