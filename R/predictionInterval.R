predictionInterval <- function(thetao,
                               seo,
                               ser,
                               tau = 0,
                               conf.level = 0.95,
                               designPrior = "predictive") {
    # vectorize function in all arguments
    resultList <- mapply(FUN = function(thetao, seo, ser, tau, conf.level, designPrior) {
        # sanity checks
        if (!is.numeric(thetao)) 
            stop("thetao must be numeric")
        if (!(designPrior %in% c("conditional", "predictive", "EB")))
            stop('designPrior must be either "conditional", "predictive", or "EB"')
        if (!is.numeric(seo) || seo <= 0)
            stop("seo must be numeric and larger than 0")
        if (!is.numeric(ser) || ser <= 0)
            stop("ser must be numeric and larger than 0")
        if (!is.numeric(tau) || tau < 0)
            stop("tau must be numeric and cannot be negative")
        if (!is.numeric(conf.level) || (conf.level < 0 || conf.level > 1))
            stop("conf.level must be numeric and in [0, 1]")

        # determine parameters of predictive distribution of yr
        if(designPrior == "conditional"){
            mu <- thetao
            sigma <- ser
        }
        if(designPrior == "predictive"){
            mu <- thetao
            sigma <- sqrt(seo^2 + ser^2 + 2*tau^2)
        }
        if (designPrior == "EB"){
            s <- pmax(1 - (seo^2 + tau^2)/thetao^2, 0)
            mu <- s*thetao
            sigma <- sqrt(s*(seo^2 + tau^2) + ser^2 + tau^2)
        }

        # compute prediction interval
        lower <- qnorm(p = (1 - conf.level)/2, mean = mu, sd = sigma)
        upper <- qnorm(p = (1 + conf.level)/2, mean = mu, sd = sigma)
        result <- data.frame(lower = lower, mean = mu, upper = upper)
        return(result)
    }, thetao, seo, ser, tau, conf.level, designPrior, SIMPLIFY = FALSE)

    result <- do.call("rbind", args = resultList)
    return(result)
}



# predictionInterval <- function (zo, c = 1, conf.level = 0.95, designPrior = "predictive",
#                                 d = 0)
# {
#     resultList <- mapply(FUN = function(zo, c, conf.level, designPrior,
#                                         d) {
#         if (!(designPrior %in% c("conditional", "predictive",
#                                  "EB")))
#             stop("designPrior must be either \"conditional\", \"predictive\", or \"EB\"")
#         if (!is.numeric(c) || c <= 0)
#             stop("c must be numeric and larger than 0")
#         if (!is.numeric(d) || d < 0)
#             stop("d must be numeric and cannot be negative")
#         if (!is.numeric(conf.level) || (conf.level < 0 || conf.level >
#                                         1))
#             stop("conf.level must be numeric and in [0, 1]")
#         if (designPrior == "conditional") {
#             mu <- zo * sqrt(c)
#             sigma <- 1
#         }
#         if (designPrior == "predictive") {
#             mu <- zo * sqrt(c)
#             sigma <- sqrt(c + 1 + 2 * d * c)
#         }
#         if (designPrior == "EB") {
#             s <- pmax(1 - (1 + d)/zo^2, 0)
#             mu <- s * zo * sqrt(c)
#             sigma <- sqrt(s * c * (1 + d) + 1 + d * c)
#         }
#         lower <- qnorm(p = (1 - conf.level)/2, mean = mu, sd = sigma)
#         upper <- qnorm(p = (1 + conf.level)/2, mean = mu, sd = sigma)
#         result <- data.frame(lower = lower, mean = mu, upper = upper)
#         return(result)
#     }, zo, c, conf.level, designPrior, d, SIMPLIFY = FALSE)
#     result <- do.call("rbind", args = resultList)
#     return(result)
# }
