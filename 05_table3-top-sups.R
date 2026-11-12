# --------------------------------------------------------------------
# 05: Table 3 — top supplementary variable categories per IML per period
#
# Port of R/sup-clust-dims.r and R/top-sups-period{1-4}.r: ranks the
# supplementary variable categories by their Euclidean distance to each
# logic's cluster centroid in the 3-dimensional MCA space.
#
# Outputs:
#   output/tables/top.sups.period{1-4}.csv  (top 25 per logic, cluster
#       number order, as in the original analysis files)
#   output/tables/table3-top10-sups.csv     (tidy top-10 version, as in
#       the published Table 3)
# --------------------------------------------------------------------

if (!file.exists("run-all.R")) stop("Run scripts from the repository root (the folder containing run-all.R).")
ROOT <- getwd()
DERIVED <- file.path(ROOT, "data-derived")
OUTT <- file.path(ROOT, "output", "tables")
dir.create(OUTT, showWarnings = FALSE, recursive = TRUE)

library(dplyr)
library(tidyr)

load(file.path(DERIVED, "ITT-MCA.RData"))

# cluster-number -> published logic-name mapping, as in the original
# top-sups-period{1-4}.r scripts
logic_names <- list(
  p1 = c("Racial.Agressor", "National.Conform", "Local.Crusaders", "Civil.Rights"),
  p2 = c("Racial.Agressor", "Local.Crusaders", "School.Obstruct", "National.Conform"),
  p3 = c("Local.Crusaders", "Racial.Agressor", "National.Conform", "School.Obstruct"),
  p4 = c("National.Conform", "Local.Crusaders", "Racial.Agressor", "School.Obstruct", "Pro.Life")
)

top_sups_one <- function(pi) {
  mca.res  <- get(sprintf("mca.res%d", pi))
  itt.yes  <- get(sprintf("itt.yes.p%d", pi))
  res.hc   <- get(sprintf("res.hc%d", pi))
  sup <- data.frame(mca.res$quali.sup$coord)
  ic  <- data.frame(itt.yes, cluster = res.hc$cluster)
  cent <- aggregate(ic, by = list(ic$cluster), FUN = mean)[, c("Group.1", "Dim.1", "Dim.2", "Dim.3")]
  k <- nrow(cent)
  dists <- sapply(seq_len(k), function(j) {
    (sup$Dim.1 - cent$Dim.1[j])^2 + (sup$Dim.2 - cent$Dim.2[j])^2 + (sup$Dim.3 - cent$Dim.3[j])^2
  })
  colnames(dists) <- sprintf("dist%d", seq_len(k))
  sup <- cbind(sup, dists)
  out <- sapply(seq_len(k), function(j) rownames(sup)[order(sup[[sprintf("dist%d", j)]])])
  out <- data.frame(out[1:25, , drop = FALSE])
  names(out) <- logic_names[[pi]]
  out$Rank <- 1:25
  out
}

tidy <- list()
for (pi in 1:4) {
  ts <- top_sups_one(pi)
  fn <- file.path(OUTT, sprintf("top.sups.period%d.csv", pi))
  write.csv(ts, fn, row.names = FALSE)
  cat("wrote", fn, "\n")
  long <- tidyr::pivot_longer(ts, cols = -Rank,
                              names_to = "Logic", values_to = "Category")
  long$Period <- c("1960-1965","1966-1972","1973-1987","1988-1995")[pi]
  # published Table 3 drops the MCA "NA" missing-data categories
  long <- long[!grepl("\\.NA$", long$Category), ]
  long$Rank10 <- ave(long$Rank, long$Logic, FUN = seq_along)
  tidy[[pi]] <- long[long$Rank10 <= 10,
                     c("Period", "Logic", "Rank10", "Category")]
}
table3 <- do.call(rbind, tidy)

# pretty-print category names in the published style: initiating-group
# categories in capitals; binary "No" categories as "X (Absence)"
pretty_var <- c(BLACK = "BLACK", WHITE = "WHITE", OTHRACE = "OTHER RACE",
                JEWISH = "JEWISH", POLGROUP = "POLITICAL GROUP",
                RELGROUP = "RELIGIOUS GROUP", OCCGROUP = "OCCUPATIONAL GROUP",
                STUDENTS = "STUDENTS", RESIDENTS = "RESIDENTS", SMO = "SMO")
pretty_special <- c(ARRESTS_Yes = "Arrests made", ARRESTS_No = "No arrests made",
                    INJURY_Yes = "Injuries reported", INJURY_No = "No injuries reported",
                    DEATHS_Yes = "Deaths reported", DEATHS_No = "No deaths reported",
                    PROPDAM_Yes = "Property Damage", PROPDAM_No = "No Property Damage")
prettify <- function(x) {
  ifelse(x %in% names(pretty_special), pretty_special[x],
  ifelse(grepl("_Yes$", x), pretty_var[sub("_Yes$", "", x)],
  ifelse(grepl("_No$",  x), paste(pretty_var[sub("_No$", "", x)], "(Absence)"),
         x)))
}
table3$CategoryPublished <- prettify(table3$Category)

write.csv(table3, file.path(OUTT, "table3-top10-sups.csv"), row.names = FALSE)
cat("wrote output/tables/table3-top10-sups.csv\n")
