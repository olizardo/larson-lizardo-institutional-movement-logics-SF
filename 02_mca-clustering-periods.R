# --------------------------------------------------------------------
# 02: Multiple Correspondence Analysis of ITTs per period + divisive
#     hierarchical clustering of the "yes" modalities
#
# R port of R/mca-anaysis.r (the script that generated the objects
# behind Figures 3-6 and Table 3). Produces data-derived/ITT-MCA.RData
# containing mca.res1-4, itt.yes.p1-4 and res.hc1-4.
# --------------------------------------------------------------------

if (!file.exists("run-all.R")) stop("Run scripts from the repository root (the folder containing run-all.R).")
ROOT    <- getwd()
DERIVED <- file.path(ROOT, "data-derived")

library(FactoMineR)
library(factoextra)
library(cluster)
library(haven)
library(dplyr)

# --- factor conversion matching the original foreign::read.dta behavior --
# Stata saved the ITTs with 0="No"/1="Yes" value labels and the
# supplementary variables with their own labels; read.dta returned both
# as factors with levels in ascending value order.
to_factor <- function(x) {
  labs <- attr(x, "labels")   # named numeric: values = codes, names = labels
  if (is.null(labs)) {
    return(factor(x, levels = sort(unique(x))))
  }
  vals <- as.numeric(labs)
  o    <- order(vals)
  factor(x, levels = vals[o], labels = names(labs)[o])
}

prep_period <- function(f, n_dummy, supnames) {
  d  <- read_dta(file.path(DERIVED, f))
  md <- d[, -1]                                  # drop EVENTID
  n_sup <- length(supnames)
  is_dummy <- !(seq_len(ncol(md)) %in% ((ncol(md) - n_sup + 1):ncol(md)))
  md[is_dummy] <- lapply(md[is_dummy], function(x)
    factor(x, levels = c(0, 1), labels = c("No", "Yes")))
  md[!is_dummy] <- lapply(md[!is_dummy], to_factor)
  # position checks mirroring the original hardcoded column ranges
  stopifnot(ncol(md) == n_dummy + n_sup)
  stopifnot(identical(names(md)[(ncol(md) - n_sup + 1):ncol(md)], supnames))
  md
}

sup19 <- c("TARGNUM","SMONAMED","SMONUM","VIOLTYPE","COUNTERD","PROPDAM",
           "INJURY","DEATHS","BLACK","WHITE","OTHRACE","JEWISH","POLGROUP",
           "RELGROUP","OCCGROUP","STUDENTS","RESIDENTS","SMO","ARRESTS")
sup22 <- c(sup19[1:18], "POLICE_PRESENCE", "ARRESTS", "EVENTSIZE", "REGION")

specs <- list(
  p1 = list(file = "ca-data-period1-1p.dta", nd = 37, supnames = sup22),
  p2 = list(file = "ca-data-period2-1p.dta", nd = 40, supnames = sup22),
  p3 = list(file = "ca-data-period3-1p.dta", nd = 52, supnames = sup22),
  p4 = list(file = "ca-data-period4-1p.dta", nd = 51, supnames = sup22)
)

for (p in names(specs)) {
  md <- prep_period(specs[[p]]$file, specs[[p]]$nd, specs[[p]]$supnames)
  sup_pos <- (ncol(md) - length(specs[[p]]$supnames) + 1):ncol(md)
  res <- MCA(md, ncp = 3, graph = FALSE, method = "Indicator",
             na.method = "NA", quali.sup = sup_pos)
  assign(paste0("mydata.", p), md)
  assign(paste0("mca.res", substr(p, 2, 2)), res)
  comps <- data.frame(res$var$coord)
  # select the "Yes" modalities (even rows: No, Yes, No, Yes, ...)
  assign(paste0("itt.yes.", p), comps[seq(2, nrow(comps), by = 2), ])
}

ks <- c(4, 4, 4, 5)
for (i in 1:4) {
  assign(sprintf("res.hc%d", i),
         eclust(get(sprintf("itt.yes.p%d", i)), "diana", k = ks[i],
                graph = FALSE, hc_metric = "euclidean",
                hc_method = "complete", stand = FALSE))
  cat(sprintf("Period %d: %d yes-modalities clustered into k = %d\n",
              i, nrow(get(sprintf("itt.yes.p%d", i))), ks[i]))
}

save(list = ls(), file = file.path(DERIVED, "ITT-MCA.RData"))
cat("Saved", file.path(DERIVED, "ITT-MCA.RData"), "\n")
