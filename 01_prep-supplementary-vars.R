# --------------------------------------------------------------------
# 01: Build the supplementary-variable files and merge them with the
#     period ITT files
#
# R port of do/supplementary.do + do/ca-data-recode-v2.2.do:
#   - construct the categorical supplementary variables from
#     "Full Data 2.2.dta" (targets, SMOs, violence, event size,
#     initiating group characteristics, arrests, Census region)
#   - join them onto each period ITT file by EVENTID
#   - apply the prevalence thresholds (drop ITTs with mean < j, drop
#     all-zero supplementary categories' variables)
#   - save ca-data-period{1-4}-{0,1,2,5,7,10}p.dta
#
# The column sets produced here match the archived analysis files used
# for the published results (verified in 07_validate.R). The published
# analysis uses the 1p (mean >= .01) threshold files.
# --------------------------------------------------------------------

if (!file.exists("run-all.R")) stop("Run scripts from the repository root (the folder containing run-all.R).")
ROOT    <- getwd()
ARCHIVE <- file.path(ROOT, "larson-lizardo-institutional-movement-logics-SF")
DERIVED <- file.path(ROOT, "data-derived")

library(foreign)
library(dplyr)
library(haven)

fd <- read.dta(file.path(ARCHIVE, "dta", "Full Data 2.2.dta"))

itt <- names(fd)[which(names(fd) == "ANIMAL_RIGHTS"):which(names(fd) == "USSR")]

# ---- port of ca-data-recode-v2.2.do: Census-region dummies -------------
ne <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA")
mw <- c("IL","IN","MI","OH","WI","IA","KS","MN","MO","NE","ND","SD")
so <- c("DE","FL","GA","MD","NC","SC","VA","DC","WV","AL","KY","MS","TN","AR","LA","OK","TX")
we <- c("AZ","CO","ID","MT","NV","NM","UT","WY","AK","CA","HI","OR","WA")

state_cols <- lapply(c("state1","state2","state3","state4"), function(v) as.character(fd[[v]]))
state_cols[[1]][state_cols[[1]] == "Washington"] <- "DC"   # do-file: replace state1 only

inlist <- function(cols, codes) {
  hits <- lapply(cols, function(x) !is.na(x) & x %in% codes)
  as.numeric(Reduce(`|`, hits))
}
northeast <- inlist(state_cols, ne)
midwest   <- inlist(state_cols, mw)
south     <- inlist(state_cols, so)
west      <- inlist(state_cols, we)

# ---- supplementary variables (port of supplementary.do) ----------------
igrp <- lapply(c("igrp1c1","igrp1c2","igrp2c1","igrp2c2","igrp3c1","igrp3c2"),
               function(v) as.numeric(fd[[v]]))
grp <- function(f) as.numeric(Reduce(`|`, lapply(igrp, f)))
black     <- grp(function(x) !is.na(x) & x == 401)
white     <- grp(function(x) !is.na(x) & x == 430)
othrace   <- grp(function(x) !is.na(x) & (x == 400 | (x > 401 & x < 430) | (x > 434 & x < 500)))
jewish    <- grp(function(x) !is.na(x) & x == 432)
women     <- grp(function(x) !is.na(x) & x == 500)
students  <- grp(function(x) !is.na(x) & x == 800)
polgroup  <- grp(function(x) !is.na(x) & x == 1000)
relgroup  <- grp(function(x) !is.na(x) & x == 1100)
residents <- grp(function(x) !is.na(x) & x == 1500)
smo       <- grp(function(x) !is.na(x) & x == 1800)
occgroup  <- grp(function(x) !is.na(x) & x == 100)

targnum  <- fd$targnum;  targnum[targnum >= 2] <- 2          # recode targnum 2/7 = 2
smonum   <- fd$smonum;   smonum[smonum == 0] <- 1            # recode smonum 0 = 1
smonum[smonum >= 4] <- 4                                     # recode smonum 4/100 = 4

# police presence/action: egen group(police1-police4) after replacing
# missing with 0, then the recode 2=12 3=11 4=9 6=12 7=11 8=12 10=12
pk <- lapply(c("police1","police2","police3","police4"), function(v) {
  x <- fd[[v]]; x[is.na(x)] <- 0; as.numeric(x)
})
pk_unique  <- unique(data.frame(p1 = pk[[1]], p2 = pk[[2]], p3 = pk[[3]], p4 = pk[[4]]))
pk_sorted  <- pk_unique[order(pk_unique$p1, pk_unique$p2, pk_unique$p3, pk_unique$p4), , drop = FALSE]
key        <- do.call(paste, c(pk, sep = "|"))
key_sorted <- do.call(paste, c(pk_sorted, sep = "|"))
police     <- match(key, key_sorted)
police[police %in% c(2, 6, 8)] <- 12
police[police %in% c(3, 7)]    <- 11
police[police == 4]            <- 9

arrests   <- as.numeric(!is.na(fd$arrestd))                  # gen arrests = arrestd != .

# event size: exact or estimated participant count, binned
particex  <- fd$particex
particex[!is.na(particex) & particex <= 9]              <- 1
particex[!is.na(particex) & particex >= 10 & particex <= 49]   <- 2
particex[!is.na(particex) & particex >= 50 & particex <= 99]   <- 3
particex[!is.na(particex) & particex >= 100 & particex <= 999] <- 4
particex[!is.na(particex) & particex >= 1000 & particex <= 9999] <- 5
particex[!is.na(particex) & particex >= 10000]                 <- 6
eventsize <- fd$partices
eventsize[is.na(eventsize)] <- particex[is.na(eventsize)]

violtype <- fd$violtype
violtype[fd$viold == 0] <- 0                                 # violtype = 0 if viold == 0

region <- rep(NA_real_, nrow(fd))
for (k in 1:4) region[fd[[c("northeast","midwest","south","west")[k]]] == 1] <- k

# ---- assemble with labels, in the original stored order ----------------
lab <- function(x, labs) labelled(as.numeric(x), labs)
yn  <- c(No = 0, Yes = 1)

sup <- data.frame(
  EVENTID   = as.integer(fd$eventid),
  TARGNUM   = lab(targnum, c(`No target ID` = 0, `One target` = 1, `More than one target` = 2)),
  SMONAMED  = lab(fd$smonamed, c(`No SMO identified` = 0, `SMO Identified` = 1)),
  SMONUM    = lab(smonum, c(`One SMO` = 1, `Two SMOs` = 2, `Three SMOs` = 3, `4+ SMOs` = 4)),
  VIOLTYPE  = lab(violtype, c(`Non-violent event` = 0, Weapons = 1, `Physical Viol.` = 2,
                              `Other Viol.` = 3, `Weapons and Physical Viol.` = 4,
                              `Weapons and Other Viol.` = 5, `Physical and Other Viol.` = 6,
                              `All types of violence` = 7)),
  COUNTERD  = lab(fd$counterd, c(`No Counter-Dem.` = 0, `Counter-Dem. present` = 1)),
  PROPDAM   = lab(fd$propdam, c(`No Property Damage` = 0, `Property Damage` = 1)),
  INJURY    = lab(fd$injury, yn),
  DEATHS    = lab(fd$deaths, yn),
  BLACK     = lab(black, yn), WHITE = lab(white, yn), OTHRACE = lab(othrace, yn),
  JEWISH    = lab(jewish, yn), POLGROUP = lab(polgroup, yn), RELGROUP = lab(relgroup, yn),
  OCCGROUP  = lab(occgroup, yn), STUDENTS = lab(students, yn),
  RESIDENTS = lab(residents, yn), SMO = lab(smo, yn),
  POLICE_PRESENCE = lab(police, c(`No Police Presence` = 1,
                            `Police presence - No action` = 5,
                            `Police presence - Action` = 9,
                            `Police presence - Force` = 11,
                            `Police presence - Violence` = 12)),
  ARRESTS   = lab(arrests, yn),
  EVENTSIZE = lab(eventsize, c(Small = 1, `Medium Gathering` = 2, `Large Gathering` = 3,
                               `Hundreds of Persons` = 4, `Thousands of Persons` = 5,
                               `Tens of Thousands` = 6)),
  REGION    = lab(region, c(Northeast = 1, Midwest = 2, South = 3, West = 4))
)

# NOTE: POLICE is included here because the published Table 3 rankings
# contain police-presence categories, which only appear if POLICE was
# among the supplementary variables in the run behind the published
# table. The archived -1p files (a later re-run) dropped POLICE; 07_validate.R
# therefore compares only the columns shared with the archive.
write_dta(sup, file.path(DERIVED, "Supplementary-Vars-2.2.dta"))
cat("Supplementary-Vars-2.2.dta:", nrow(sup), "events x", ncol(sup), "vars\n")

# ---- merge with period files and apply thresholds ----------------------
sup_vars <- names(sup)[-1]
ths <- c("0p" = 0, "1p" = 0.01, "2p" = 0.02, "5p" = 0.05, "7p" = 0.07, "10p" = 0.10)

for (i in 1:4) {
  per <- read_dta(file.path(DERIVED, sprintf("ca-data-period%d.dta", i)))
  m   <- inner_join(per, as.data.frame(sup), by = "EVENTID")
  stopifnot(nrow(m) == nrow(per))
  for (s in names(ths)) {
    j      <- ths[[s]]
    keep_i <- itt[vapply(m[itt], function(x) (mean(x) >= j) && (max(x) > 0), logical(1))]
    keep_s <- sup_vars[vapply(m[sup_vars], function(x) max(x, na.rm = TRUE) > 0, logical(1))]
    out    <- m[, c("EVENTID", keep_i, keep_s)]
    write_dta(out, file.path(DERIVED, sprintf("ca-data-period%d-%s.dta", i, s)))
  }
  cat(sprintf("period %d: %d events; file sizes:",
              i, nrow(m)), sapply(names(ths), function(s) {
    dd <- read_dta(file.path(DERIVED, sprintf("ca-data-period%d-%s.dta", i, s)))
    ncol(dd)
  }), "\n")
}
cat("Done: thresholded period files written to", DERIVED, "\n")
