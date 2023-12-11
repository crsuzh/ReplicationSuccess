# load ReplicationSuccess
library(ReplicationSuccess)
library(dplyr) # for data wrangling

## RPCB data with SMD transformed effect sizes but before aggregating internal
## replications:
## Download the zip archive if it does not exist
destfile <- file.path(tempdir(), "osf_squy7.zip")
if (!file.exists(destfile)) {
    download.file(
        url = "https://files.osf.io/v1/resources/squy7/providers/github/Prepped%20data/Intermediate%20work/?zip=",
        destfile = destfile
    )
}
contents <- unzip(zipfile = destfile, list = TRUE)
file <- grep(
    pattern = "intermediate_dataset_step3.csv",
    x = contents$Name,
    fixed = TRUE,
    value = TRUE
)
out_dir <- file.path(tempdir(), "RPCB")
dir.create(out_dir, showWarnings = FALSE)
unzip(zipfile = destfile, files = file, exdir = out_dir)
data_file <- file.path(out_dir, file)

# Import data, clean up, save 

dat <- read.csv(file = data_file)
dat2 <- dat %>%
    ## only take studies with quantitative effect information available
    filter(!is.na(origDirection),
           quantPair == TRUE)

## organize data set for reanalysis
datClean <- dat2 %>%
    mutate(
        ## define identifier for paper, experiment, effect, internal replication
        id = paste0("(", pID, ", ", eID, ", ", oID, ", ", internalID, ")")
    ) %>%
    select(
        osf = OSF.project.link,
        id,
        paper = pID,
        experiment = eID,
        effect = oID,
        internalReplication = internalID,
        resulto = origDirection,
        resultr = repDirection,
        ## effect sizes and standard errors on SMD scale
        smdo = origES3,
        so = origSE3,
        smdr = repES3,
        sr = repSE3,
        ## effect sizes, standard errors, p-values, confidence intervals on original scale
        effectType = Effect.size.type,
        ESo = Original.effect.size,
        seESo = Original.standard.error,
        lowerESo = Original.lower.CI,
        upperESo = Original.upper.CI,
        po = origPval,
        ESr = Replication.effect.size,
        seESr = Replication.standard.error,
        lowerESr = Replication.lower.CI,
        upperESr = Replication.upper.CI,
        pr = repPval,
        ## original and replication sample size
        ## (not clear whether group or full sample size)
        no = origN,
        nr = repN
    )

## should give 20 original null effects and 10 "successful" null replications
## (see null results in Table 1)
datClean %>%
    summarise(nulls = sum(resulto == "Null"),
              successes = sum(resulto == "Null" &
                              resultr %in% c("Null-positive", "Null-negative",
                                             "Null")))
## this should give the same
datClean %>%
    summarise(nulls = sum(resulto == "Null"),
              successes = sum(resulto == "Null" &
                              pr > 0.05))

## should give 112 original positive effects and 44 successful replications
## (see positive results in Table 1)
datClean %>%
    summarise(positives = sum(resulto == "Positive"),
              successes = sum(resulto == "Positive" &
                              resultr == "Positive" &
                              sign(smdo) == sign(smdr)))
## this should give the same
datClean %>%
    summarise(positives = sum(resulto == "Positive"),
              successes = sum(resulto == "Positive" &
                              pr < 0.05 &
                              sign(smdo) == sign(smdr)))

## save
# write.csv(datClean, "rpcb-outcome-level.csv", row.names = FALSE)

## aggregate internal replications with fixed-effect meta-analysis to get
## "effect-level data"
dat3 <- datClean %>%
    mutate(id2 = paste0("(", paper, ", ", experiment, ", ", effect, ")"))
datClean2 <- lapply(unique(dat3$id2), FUN = function(id2) {
    ## pool the internal replications with fixed-effect meta-analysis
    datr <- dat3[dat3$id2 == id2, ]
    nreplications <- nrow(datr)
    varsmdrMA <- 1 / sum(1 / datr$sr^2)
    smdrMA <- sum(datr$smdr / datr$sr^2) * varsmdrMA
    ## add the sample sizes of the internal replications
    nrTotal <- sum(datr$nr)
    if (nreplications > 1) {
        ## TODO translate back the pooled effect to the original scale?
        ## check whether still a "null" replication based on Wald p-value
        prMA <- 2 * pnorm(q = abs(smdrMA) / sqrt(varsmdrMA), lower.tail = FALSE)
        if (prMA > 0.05) {
            resultMA <- "Null"
            prNew <- prMA
        } else {
            resultMA <- "Positive"
            prNew <- prMA
        }
    } else {
        resultMA <- datr$resultr
        prNew <- datr$pr
    }
    out <- datr[1, ] %>%
        mutate(smdr = smdrMA,
               sr = sqrt(varsmdrMA),
               nr = nrTotal,
               id = id2,
               resultr = resultMA,
               pr = prNew) %>%
        select(-internalReplication, -id2)
    return(out)
}) %>%
    bind_rows()

## should give 15 original null effects and 11 "successful" null replications
## (see null results in Table 1)
# Load RPCB data (from Samuel Pawel's Github)
RPCB <- datClean2
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
