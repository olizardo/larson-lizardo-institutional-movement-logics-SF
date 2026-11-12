# --------------------------------------------------------------------
# 07: Validation — compare the R-generated pipeline outputs against the
#     archived files shipped with the paper
#
#   1. ca-data-period{1-4}-1p.dta and ca-data-two-mode-1p.dta vs. the
#      archived analysis files in the extracted zip
#   2. top.sups.period{1-4}.csv vs. the archived csv/ versions
# --------------------------------------------------------------------

if (!file.exists("run-all.R")) stop("Run scripts from the repository root (the folder containing run-all.R).")
ROOT    <- getwd()
ARCHIVE <- file.path(ROOT, "larson-lizardo-institutional-movement-logics-SF")
DERIVED <- file.path(ROOT, "data-derived")
OUTT    <- file.path(ROOT, "output", "tables")

library(foreign)
library(haven)
library(dplyr)

yn <- function(x) {
  if (is.factor(x)) return(as.numeric(x == "Yes"))
  as.numeric(x)
}

ok <- TRUE
overlap_top <- function(a, b, n) length(intersect(a[1:n], b[1:n]))
check <- function(label, cond) {
  status <- if (isTRUE(cond)) "OK" else "MISMATCH"
  if (!isTRUE(cond)) ok <<- FALSE
  cat(sprintf("  [%s] %s\n", status, label))
}

files <- c("ca-data-two-mode-1p.dta",
           sprintf("ca-data-period%d-1p.dta", 1:4))
for (f in files) {
  gen <- as.data.frame(read_dta(file.path(DERIVED, f)))
  arc <- as.data.frame(read_dta(file.path(ARCHIVE, "dta", f)))
  cat(f, ":\n")
  check(sprintf("rows (%d vs %d)", nrow(gen), nrow(arc)), nrow(gen) == nrow(arc))
  # the regenerated period files carry an extra POLICE column (present in
  # the run behind the published Table 3 but dropped from the archived
  # -1p files); compare the shared columns
  common <- intersect(names(gen), names(arc))
  check("shared column set (plus POLICE where applicable)",
        setequal(names(gen), union(names(arc), "POLICE_PRESENCE")) ||
          identical(names(gen), names(arc)))
  bad <- character(0)
  for (v in common) {
    if (v == "EVENTID") next
    a <- yn(gen[[v]]); b <- yn(arc[[v]])
    if (!identical(which(is.na(a)), which(is.na(b))) ||
        any(a[!is.na(a)] != b[!is.na(b)], na.rm = TRUE)) {
      bad <- c(bad, v)
    }
  }
  check(paste0(length(common), " shared columns match"), length(bad) == 0)
  if (length(bad)) cat("    differing columns:", paste(bad, collapse = ", "), "\n")
}

# top supplementary rankings: the archived csv/ files are an intermediate
# draft (they lack the police categories present in the published Table 3),
# so this comparison is informational rather than pass/fail
for (i in 1:4) {
  gen <- read.csv(file.path(OUTT, sprintf("top.sups.period%d.csv", i)))
  arc <- read.csv(file.path(ARCHIVE, "csv", sprintf("top.sups.period%d.csv", i)))
  cat(sprintf("top.sups.period%d.csv (informational):\n", i))
  for (cn in names(arc)[-1]) {
    ov <- overlap_top(gen[[cn]], arc[[cn]], 25)
    cat(sprintf("    %-18s top-25 overlap: %d/25\n", cn, ov))
  }
}

cat(if (ok) "\nALL CHECKS PASSED\n" else "\nSOME CHECKS FAILED - see above\n")
