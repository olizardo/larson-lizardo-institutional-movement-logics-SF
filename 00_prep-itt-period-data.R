# --------------------------------------------------------------------
# 00: Build the ITT (issue/tactic/target) indicator data files
#
# R port of do/period-data-four-chunks.do (plus the ITT-keeping part of
# the original pipeline). Starts from the raw project dataset
# "Full Data 2.2.dta" and produces:
#   - ca-data-two-mode-{0,1,2,5,7,10}p.dta  (event x ITT indicator files,
#     thresholded by ITT prevalence)
#   - ca-data-period{1-4}.dta               (same events split into the
#     four analysis periods)
#
# Validated against the archived intermediates shipped with the paper:
# generated files reproduce them exactly (see 07_validate.R).
# --------------------------------------------------------------------

if (!file.exists("run-all.R")) stop("Run scripts from the repository root (the folder containing run-all.R).")
ROOT    <- getwd()
ARCHIVE <- file.path(ROOT, "larson-lizardo-institutional-movement-logics-SF")
DERIVED <- file.path(ROOT, "data-derived")
dir.create(DERIVED, showWarnings = FALSE, recursive = TRUE)

library(foreign)
library(dplyr)
library(haven)

fd <- read.dta(file.path(ARCHIVE, "dta", "Full Data 2.2.dta"))

# ITT indicator block, in the original Stata stored order
# (varlist ANIMAL_RIGHTS - USSR)
itt <- names(fd)[which(names(fd) == "ANIMAL_RIGHTS"):which(names(fd) == "USSR")]
stopifnot(length(itt) == 78)

d <- fd[, c("evyy", "eventid", itt)]
names(d)[1:2] <- c("EVYY", "EVENTID")

# drop years outside the study window (1955 pilot cases and unyeared events)
d <- d[!d$EVYY %in% c(0, 1955), ]

# ---- event x ITT two-mode files ----------------------------------------
# NOTE: in the original do-file the threshold loop for the two-mode files
# only applies value labels and saves; it does NOT drop ITTs (the drop
# happens later, in supplementary.do, for the period files). The shipped
# two-mode-*.dta files are therefore identical copies with all 78 ITTs.
ths <- c("0p" = 0, "1p" = 0.01, "2p" = 0.02, "5p" = 0.05, "7p" = 0.07, "10p" = 0.10)
for (s in names(ths)) {
  out <- d[, c("EVYY", "EVENTID", itt)]
  write_dta(out, file.path(DERIVED, sprintf("ca-data-two-mode-%s.dta", s)))
  cat(sprintf("ca-data-two-mode-%s.dta: %d events x %d ITTs\n", s, nrow(out), length(itt)))
}

# ---- four analysis periods ---------------------------------------------
# Stata: recode EVYY 1960/1965=1 1966/1972=2 1973/1987=3 1988/1995=4
# (events with missing EVYY stay in the two-mode files but fall outside
# every period, as in the original pipeline)
d$PERIOD <- ifelse(d$EVYY <= 1965, 1L,
            ifelse(d$EVYY <= 1972, 2L,
            ifelse(d$EVYY <= 1987, 3L, 4L)))
print(table(d$PERIOD, useNA = "ifany"))

for (i in 1:4) {
  sub <- d[which(d$PERIOD == i), c("EVENTID", itt)]
  write_dta(sub, file.path(DERIVED, sprintf("ca-data-period%d.dta", i)))
}
cat("Done: period files written to", DERIVED, "\n")
