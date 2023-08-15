#' Convert between estimates, z-values, p-values, and confidence intervals
#'
#' @rdname conversionHelpers
#' @param lower Numeric vector of lower confidence interval bounds.
#' @param upper Numeric vector of upper confidence interval bounds.
#' @param conf.level The confidence level of the confidence intervals.
#' Default is 0.95.
#' @param ratio Indicates whether the confidence interval is for a
#' ratio, e.g. an odds ratio, relative risk or hazard ratio.
#' If \code{TRUE}, the standard error of the log ratio is computed.
#' Defaults to \code{FALSE}.
#' @return \code{ci2se} returns a numeric vector of standard errors.
#' @examples
#' ci2se(lower = 1, upper = 3)
#' ci2se(lower = 1, upper = 3, ratio = TRUE)
#' ci2se(lower = 1, upper = 3, conf.level = 0.9)
#'
#' @export
ci2se <- function(
    lower,
    upper,
    conf.level = 0.95,
    ratio = FALSE
) {

    check_lower_upper_arg(lower = lower, upper = upper)
    check_conflevel_arg(conf.level = conf.level)
    check_ratio_arg(ratio = ratio)

    ci2se_internal(
        lower = lower,
        upper = upper,
        conf.level = conf.level,
        ratio = ratio
    )
}

ci2se_internal <- function(
    lower,
    upper,
    conf.level = 0.95,
    ratio = FALSE
) {

    level <- 1 - conf.level
    q <- stats::qnorm(p = 1 - level / 2, lower.tail = TRUE)

    if (ratio) {
        stopifnot(all(lower > 0))
        lower <- log(lower)
        upper <- log(upper)
    }

    se <- (upper - lower) / (2 * q)
    return(se)
}

#' @rdname conversionHelpers
#' @param antilog Indicates whether the estimate is reported on the ratio scale.
#' Only applies if \code{ratio = TRUE}. Defaults to \code{FALSE}.
#' @return \code{ci2estimate} returns a numeric vector of parameter estimates.
#' @examples
#' ci2estimate(lower = 1, upper = 3)
#' ci2estimate(lower = 1, upper = 3, ratio = TRUE)
#' ci2estimate(lower = 1, upper = 3, ratio = TRUE, antilog = TRUE)
#'
#' @export
ci2estimate <- function(
    lower,
    upper,
    ratio = FALSE,
    antilog = FALSE
) {

    check_lower_upper_arg(lower = lower, upper = upper)
    check_ratio_arg(ratio = ratio)
    check_antilog_arg(antilog = antilog)

    ci2estimate_internal(
        lower = lower,
        upper = upper,
        ratio = ratio,
        antilog = antilog
    )
}

ci2estimate_internal <- function(
    lower,
    upper,
    ratio = FALSE,
    antilog = FALSE
) {

    if (ratio) {
        stopifnot(all(lower > 0))
        lower <- log(lower)
        upper <- log(upper)
    }

    res <- (lower + upper) / 2

    if (ratio && antilog) res <- exp(res)

    return(res)
}

#' @rdname conversionHelpers
#' @return \code{ci2z} returns a numeric vector of z-values.
#' @examples
#' ci2z(lower = 1, upper = 3)
#' ci2z(lower = 1, upper = 3, ratio = TRUE)
#' ci2z(lower = 1, upper = 3, conf.level = 0.9)
#'
#' @export
ci2z <- function(
    lower,
    upper,
    conf.level = 0.95,
    ratio = FALSE
) {

    check_lower_upper_arg(lower = lower, upper = upper)
    check_conflevel_arg(conf.level = conf.level)
    check_ratio_arg(ratio = ratio)

    ci2z_internal(
        lower = lower,
        upper = upper,
        conf.level = conf.level,
        ratio = ratio
    )
}

ci2z_internal <- function(
    lower,
    upper,
    conf.level = 0.95,
    ratio = FALSE
) {

    estimate <- ci2estimate_internal(
        lower = lower,
        upper = upper,
        ratio = ratio
    )

    se <- ci2se_internal(
        lower = lower,
        upper = upper,
        conf.level = conf.level,
        ratio = ratio
    )

    if (any(se == 0)) stop("Invalid standard error: Some SEs are 0.")

    z <- estimate / se

    return(z)
}

#' @rdname conversionHelpers
#' @param alternative Direction of the alternative of the p-value.
#' Either "two.sided" (default), "one.sided", "less", or "greater".
#' If "one.sided" or "two.sided" is specified, the z-value is assumed
#' to be positive.
#' @return \code{ci2p} returns a numeric vector of p-values.
#' @examples
#' ci2p(lower = 1, upper = 3)
#' ci2p(lower = 1, upper = 3, alternative = "one.sided")
#'
#' @export
ci2p <- function(
    lower,
    upper,
    conf.level = 0.95,
    ratio = FALSE,
    alternative = "two.sided"
) {

    check_lower_upper_arg(lower = lower, upper = upper)
    check_conflevel_arg(conf.level = conf.level)
    check_ratio_arg(ratio = ratio)
    check_alternative_arg(alternative = alternative)

    ci2p_internal(
        lower = lower,
        upper = upper,
        conf.level = conf.level,
        ratio = ratio,
        alternative = alternative
    )
}

ci2p_internal <- function(
    lower,
    upper,
    conf.level = 0.95,
    ratio = FALSE,
    alternative = "two.sided"
) {

    z <- ci2z_internal(
        lower = lower,
        upper = upper,
        conf.level = conf.level,
        ratio = ratio
    )

    p <- z2p_internal(
        z = z,
        alternative = alternative
    )

    return(p)
}


#' @rdname conversionHelpers
#' @param z Numeric vector of z-values.
#' @details \code{z2p} is vectorized over all arguments.
#' @return \code{z2p} returns a numeric vector of p-values. The
#' dimension of the output depends on the input. In general,
#' the output will be an array of dimension
#' \code{c(nrow(z), ncol(z), length(alternative))}. If any of these
#' dimensions is 1, it will be dropped.
#' @examples
#' z2p(z = c(1, 2, 5))
#' z2p(z = c(1, 2, 5), alternative = "less")
#' z2p(z = c(1, 2, 5), alternative = "greater")
#' z <- seq(-3, 3, by = 0.01)
#' plot(z, z2p(z), type = "l", xlab = "z", ylab = "p", ylim = c(0, 1))
#' lines(z, z2p(z, alternative = "greater"), lty = 2)
#' legend("topright", c("two-sided", "greater"), lty = c(1, 2), bty = "n")
#'
#' @export
z2p <- function(
    z,
    alternative = "two.sided"
) {

    check_z_arg(z = z)
    check_alternative_arg(alternative = alternative)

    z2p_internal(
        z = z,
        alternative = alternative
    )
}

z2p_internal <- function(
    z,
    alternative = "two.sided"
) {

    p <- vapply(
        alternative,
        function(alt) {
            if (alt == "two.sided") {
                2 * stats::pnorm(abs(z), lower.tail = FALSE)
            } else if (alt == "less") {
                stats::pnorm(q = z, lower.tail = TRUE)
            } else {
                ## alternative is "greater" or "one.sided")
                stats::pnorm(q = z, lower.tail = FALSE)
            }
        },
        double(length(z)),
        USE.NAMES = FALSE
    )

    set_output_dims(
        x = p,
        d_input = if (is.array(z)) dim(z) else length(z),
        alternative = alternative
    )
}

#' @rdname conversionHelpers
#' @param p Numeric vector of p-values.
#' @details \code{p2z} is vectorized over all arguments.
#' @return \code{p2z} returns a numeric vector of z-values. The
#' dimension of the output depends on the input. In general,
#' the output will be an array of dimension
#' \code{c(nrow(p), ncol(p), length(alternative))}. If any of these
#' dimensions is 1, it will be dropped.
#' @examples
#' p2z(p = c(0.005, 0.01, 0.05))
#' p2z(p = c(0.005, 0.01, 0.05), alternative = "greater")
#' p2z(p = c(0.005, 0.01, 0.05), alternative = "less")
#' p <- seq(0.001, 0.05, 0.0001)
#' plot(p, p2z(p), type = "l", ylim = c(0, 3.5), ylab = "z")
#' lines(p, p2z(p, alternative = "greater"), lty = 2)
#' legend("bottomleft", c("two-sided", "greater"), lty = c(1, 2), bty = "n")
#' @export
p2z <- function(
    p,
    alternative = "two.sided"
) {

    check_p_arg(p = p)
    check_alternative_arg(alternative = alternative)

    p2z_internal(
        p = p,
        alternative = alternative
    )

}

p2z_internal <- function(
    p,
    alternative = "two.sided"
) {

    z <- vapply(
        alternative,
        function(alt) {

            if (alt == "two.sided") {
                stats::qnorm(p = p / 2, lower.tail = FALSE)
            } else if (alt == "less") {
                stats::qnorm(p = p, lower.tail = TRUE)
            } else {
                ## alternative is "one.sided" or "greater"
                stats::qnorm(p = p, lower.tail = FALSE)
            }
        },
        double(length(p)),
        USE.NAMES = FALSE
    )

    set_output_dims(
        x = z,
        d_input = if (is.array(p)) dim(p) else length(p),
        alternative = alternative
    )
}


################################################################################
#                             Helper functions                                 #
################################################################################

# set output dimensions and names ==============================================
set_output_dims <- function(x, d_input, alternative) {

    is_arr <- length(d_input) > 1L
    l_alt <- length(alternative)

    if (is_arr) {
        # case: x is an array
        if (l_alt > 1L) {
            # if alternative has length > 1, add dimension
            dims <- c(d_input, l_alt)
            l_dims <- length(dims)
            dim(x) <- dims
            dimnames(x)[[l_dims]] <- alternative
        } else {
            # otherwise just return with same dimensions
            dim(x) <- d_input
        }
    } else {
        # case: x is an atomic vector
        l_alt1 <- l_alt == 1L
        if (l_alt1 || d_input == 1L) {
            # if any of alternative or x has length 1, return atomic vector
            # as is. However, if alternative is larger 1, set names
            x <- drop(x)
            if (!l_alt1) names(x) <- alternative
        } else {
            # all other cases: return matrix with nrow = lx and ncol = l_alt
            dim(x) <- c(d_input, l_alt)
            colnames(x) <- alternative
        }
    }

    x
}

# Argument checks ==============================================================
is_numeric_finite <- function(x) {
    if (is.numeric(x) && all(is.finite(x))) TRUE else FALSE
}

is_num_fin_poslength <- function(x) {
    if (is_numeric_finite(x) && length(x) > 0L) TRUE else FALSE
}

is_in_01 <- function(p) {
    if (all(0 < p) && all(p < 1)) TRUE else FALSE
}

is_in_01_right_include <- function(p) {
    if (all(0 < p) && all(p <= 1)) TRUE else FALSE
}

is_true_false <- function(x) {
    if (is.logical(x) && length(x) == 1L && !is.na(x)) TRUE else FALSE
}

is_same_length <- function(x, y) {
    if (length(x) == length(y)) TRUE else FALSE
}

check_conflevel_arg <- function(conf.level) {
    no_error <- is_numeric_finite(conf.level) &&
        length(conf.level) == 1L &&
        is_in_01(conf.level)
    if (!no_error) {
        stop(
            "Argument 'conf.level' must be numeric, of length 1 and in (0, 1).",
            call. = TRUE
        )
    }
    invisible(NULL)
}

check_ratio_arg <- function(ratio) {
    if (!is_true_false(ratio)) {
        stop(
            "Argument 'ratio' must be either TRUE or FALSE.",
            call. = TRUE
        )
    }
    invisible(NULL)
}

check_antilog_arg <- function(antilog) {
    if (!is_true_false(x = antilog)) {
        stop(
            "Argument 'antilog' must be either TRUE or FALSE.",
            call. = TRUE
        )
    }
    invisible(NULL)
}

check_lower_upper_arg <- function(lower, upper) {

    ok <- vapply(list(lower, upper), is_num_fin_poslength, logical(1L))
    no_error <- all(ok) && is_same_length(upper, lower) && all(lower <= upper)
    if (!no_error) {
        stop(
            paste0(
                    "Invalid 'lower' and 'upper': Both must be numeric, ",
                    "finite, of the same length and 'lower' should be ",
                    "<= 'upper'."
            ),
            call. = TRUE
        )
    }
    invisible(NULL)
}

check_p_arg <- function(p) {
    if (!is_numeric_finite(p) || !is_in_01_right_include(p)) {
        stop(
            "Argument 'p' must be numeric, finite and in (0, 1].",
            call. = TRUE
        )
    }
    invisible(NULL)
}

check_z_arg <- function(z) {
    if (!is_numeric_finite(z)) {
        stop(
            "Argument 'z' must be numeric and finite.",
            call. = TRUE
        )
    }
    invisible(NULL)
}

check_alternative_arg <- function(alternative) {
    if (
        is.null(alternative) ||
        !all(alternative %in% c("two.sided", "one.sided", "less", "greater"))
    ) {
        msg <- paste0(
                    "Argument 'alternative' must be one of c(\"two.sided\", ",
                    "\"one.sided\", \"less\", \"greater\")"
            )
        stop(msg, call. = TRUE)
    }
    invisible(NULL)
}
