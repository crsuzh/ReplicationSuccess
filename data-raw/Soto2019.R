library(readxl)
library(ReplicationSuccess)

# Get the data
## Download the zip archive if it does not exist
destfile <- file.path(tempdir(), "osf_d3xb.zip")
if (!file.exists(destfile)) {
    download.file(
        url = "https://files.osf.io/v1/resources/d3xb7/providers/osfstorage/?zip=",
        destfile = destfile
    )
}

## Extract the file with the data
contents <- unzip(zipfile = destfile, list = TRUE)
file1 <- grep(
    pattern = "Aggregated results and chi2 tests.xlsx",
    x = contents$Name,
    fixed = TRUE,
    value = TRUE
)
file2 <- grep(
    pattern = "Replication coding and results.xlsx",
    x = contents$Name,
    fixed = TRUE,
    value = TRUE
)
files <- c(file1, file2)
out_dir <- file.path(tempdir(), "Soto")
dir.create(out_dir, showWarnings = FALSE)
unzip(zipfile = destfile, files = files, exdir = out_dir)
data_files <- file.path(out_dir, files)
dat <- readxl::read_xlsx(data_files[1], sheet = "Data")
study_info <- readxl::read_xlsx(data_files[2], sheet = "Results")

## remove the zip archive
# unlink(destfile)

# =============================================================================

# Get the original study citations

## Keep only relevant info
study_info <- subset(
    study_info,
    select = c(OutcomeNumber, TraitPredicted, OriginalStudyCitation)
)
## Remove title, journal etc. from citations and remove suboutcome number
names(study_info)[1L] <- "OutcomeSubnumber"
study_info <- within(
    study_info,
    {
        # Keep only authors and year for citation info
        OriginalStudyCitation <- sub(
            "^(.+?\\(\\d{4}\\)).+$",
            "\\1",
            OriginalStudyCitation,
            perl = TRUE
        )
        # Add the outcome number
        # (since there is sometimes more than one study per outcome)
        OutcomeNumber <- as.numeric(sub("^(\\d+).*$", "\\1", OutcomeSubnumber))
    }
)
## Combine all citations from studies belonging to the same outcome
## (sometimes the outcomes are combined outcomes consisting of different
##  suboutcomes. We combine all the studies under a combined outcome
## )
study_info <- stats::aggregate(
    x = OriginalStudyCitation ~ TraitPredicted + OutcomeNumber,
    FUN = function(x) paste(unique(x), collapse = " / "),
    data = study_info,
    na.action = identity
)

# =============================================================================

# Merge the citation info into the data set

dat <- merge(x = dat, y = study_info, all = TRUE, sort = FALSE)

# =============================================================================

# Calculate what we need from the data set

df_list <- lapply(
    list("o", "r"),
    function(study, dat) {
        # Calculate all the variables
        is_orig <- study == "o"

        ## Get the names in the data file
        dat_nm <- if (is_orig) "Original" else "Replication"
        ## get fiso/fisr
        fis <- dat[[paste0(dat_nm, "FisheredEffectSizeByOutcome")]]
        ## get no/nr
        n <- dat[[paste0(dat_nm, "SampleSizeByOutcome")]]
        ## get ro/rr from fiso/fisr
        r <- tanh(fis)
        ## get se_fiso/se_fisr
        se_fis <- (sqrt(n - 3))^(-1)
        ## get po/pr
        p <- ReplicationSuccess::z2p(fis / se_fis, alternative = "two.sided")
        ## get po1/pr1
        p1 <- ReplicationSuccess::z2p(fis / se_fis, alternative = "one.sided")

        # Use nice names for the predicted traits
        trait_map <- c(
            "E" = "Extraversion",
            "A" = "Agreeableness",
            "C" = "Conscientiousness",
            "N" = "Neuroticism/Negative Emotionality",
            "O" = "Openness to Experience/Open-Mindedness"
        )
        tp <- unname(trait_map[dat$TraitPredicted])

        # Make a data frame
        df_names <- c(
            "study", "project", "outcome", "trait_predicted",
            paste0("r", study), paste0("fis", study), paste0("se_fis", study),
            paste0("p", study), paste0("p", study, "1"), "pm_belief",
            paste0("n", study)
        )
        res <- list(
            if (is_orig) dat$OriginalStudyCitation else NULL, # study
            if (is_orig) "LOOPR" else NULL,                   # project
            if (is_orig) dat$OutcomeName else NULL,           # outcome
            if (is_orig) tp else NULL,                        # trait_predicted
            r,                                                # ro/rr
            fis,                                              # fiso/fisr
            se_fis,                                           # se_fiso/se_fisr
            p,                                                # po/pr
            p1,                                               # po1/pr1
            if (is_orig) NA else NULL,                        # pm_belief
            n                                                 # no/nr
        )
        keep <- vapply(res, function(x) !is.null(x), logical(1L))
        stats::setNames(as.data.frame(res[keep]), df_names[keep])
    },
    dat = dat
)

df <- do.call("cbind", df_list)

# Order the columns as in RProjects data set
nm_rproj <- names(ReplicationSuccess::RProjects)
idx <- names(df) %in% nm_rproj
df <- cbind(df[!idx], df[idx][nm_rproj])
save(df, file = "../data/Soto2019.rda")
