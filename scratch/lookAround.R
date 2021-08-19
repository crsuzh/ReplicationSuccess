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
(hm <- hMeanChiSqCI(thetahat = thetahat2, se = se2, alternative = "none"))

plot(x = muSeq, y = pValueSeq, type = "l", panel.first = grid(lty = 1),
     xlab = expression(mu), ylab = "p-value")
abline(v = thetahat2, h = alpha, lty = 2)
arrows(x0 = hm$CI[, 1], x1 = hm$CI[, 2], y0 = alpha,
       y1 = alpha, col = "darkgreen", lwd = 3, angle = 90, code = 3)
     

hMeanChiSqCI(thetahat = c(1,1,0.0755451095149995, 0, 0.0464405898859865, 0.852346228390377, 
                          0.159597617461607),
             se = c(1,1,0.137100202938993, 0.0626965224781561, 0.060411102631366, 0.467078529078658, 
                    0.0627716455112089),
             alternative = "none")
