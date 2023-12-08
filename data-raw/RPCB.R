# load ReplicationSuccess
library(ReplicationSuccess)

# Load RPCB data (from Samuel Pawel's Github)
RPCB <- read.csv(
    "https://raw.githubusercontent.com/SamCH93/thesis/main/source/data/rpcb-effect-level.csv"
)

# Recalculate p-values is done in Charlotte Micheloud's thesis:
# For code see:
# https://gitlab.uzh.ch/charlotte.micheloud/phd-thesis/-/blob/master/reproMaterial/Thesis_introduction.R

RPCB <- within(
    RPCB,
    {
        # Add a temporary variable =============================================
        ## index which original study have positive effects
        is_pos <- resulto == "Positive"
        ## Initialize added variables that should be NA if
        ## the original effect is negative.
        ## This is the case for 1-sided p-values
        po1 <- pr1 <- pS1 <- rep(NA_real_, length(is_pos))

        # Do recalculations ====================================================
        ## 95\% confidence intervals
        CIo_u <- smdo + qnorm(1 - 0.025) * so
        CIo_l <- smdo - qnorm(1 - 0.025) * so

        CIr_u <- smdr + qnorm(1 - 0.025) * sr
        CIr_l <- smdr - qnorm(1 - 0.025) * sr
        ## relative effect size
        d <- smdr / smdo
        ## variance ratio
        c <- so^2 / sr^2
        ## Calculation of z-values
        ## and p-values (one and two-sided)
        ## for superiority
        zo <- smdo / so
        zr <- smdr / sr
        po1[is_pos] <- z2p(zo[is_pos], alternative = "greater") # positives only
        pr1[is_pos] <- z2p(zr[is_pos], alternative = "greater") # positives only
        po <- z2p(zo, alternative = "two.sided")
        pr <- z2p(zr, alternative = "two.sided")
        ## significance criterion
        ttr <- ifelse(po1 < 0.025 & pr1 < 0.025, 1, 0)
        ## meta-analysis
        var_ma <- 1 / (1 / so^2 + 1 / sr^2)
        se_ma <- sqrt(var_ma)
        ma <- (smdo / so^2 + smdr / sr^2) * var_ma
        p_ma <- pnorm(abs(ma / sqrt(se_ma)), lower.tail = FALSE) * 2
        CIma_l <- ma - qnorm(1 - 0.025) * se_ma
        CIma_u <- ma + qnorm(1 - 0.025) * se_ma
        ## pSceptical for superiority
        pS <- pSceptical(
            zo = smdo / so,
            zr = smdr / sr,
            c = so^2 / sr^2,
            type = "nominal",
            alternative = "two.sided"
        )
        pS1[is_pos] <- pSceptical(
            zo = smdo[is_pos] / so[is_pos],
            zr = smdr[is_pos] / sr[is_pos],
            c = so[is_pos]^2 / sr[is_pos]^2,
            type = "nominal",
            alternative = "one.sided"
        )

        # Clean up temporary/unused variables ==================================
        rm(is_pos, var_ma)
    }
)

# Reorder columns
o <- c(
    "osf", "id", "paper", "experiment", "effectType", "effect", "resulto", "no",
    "smdo", "so", "zo", "po", "po1", "CIo_l", "CIo_u", "ESo", "seESo",
    "lowerESo", "upperESo", "resultr", "nr", "smdr",  "sr", "zr", "pr", "pr1",
    "CIr_l", "CIr_u", "ESr", "seESr", "lowerESr", "upperESr",
    "d", "c", "ttr",
    "ma", "se_ma", "p_ma", "CIma_l", "CIma_u", "pS", "pS1"
)

RPCB <- RPCB[o]

# store data frame
save(RPCB, file = "../data/RPCB.rda")
