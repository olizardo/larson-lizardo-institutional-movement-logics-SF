# --------------------------------------------------------------------
# Run the full replication pipeline for
# Larson & Lizardo, "Institutional Movement Logics..." (Social Forces)
#
# Run from the repository root:  Rscript run-all.R
# (or source("run-all.R") in an R session started in this folder)
# --------------------------------------------------------------------

scripts <- c(
  "00_prep-itt-period-data.R",
  "01_prep-supplementary-vars.R",
  "02_mca-clustering-periods.R",
  "03_figure2-years.R",
  "04_figures3-6-periods.R",
  "05_table3-top-sups.R",
  "06_curated-tables.R",
  "07_validate.R"
)
for (s in scripts) {
  cat("\n=====================================================\n")
  cat("Running:", s, "\n")
  cat("=====================================================\n")
  source(s, echo = TRUE, max.deparse.length = 200)
}
