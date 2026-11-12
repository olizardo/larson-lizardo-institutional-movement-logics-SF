# --------------------------------------------------------------------
# 03: Figure 2 — hierarchical clustering of years (1960-1995)
#
# Port of the year-MCA block at the end of R/pca-plots-final-paper.R
# (also in R/years-plot.r): MCA of events x years + ITT indicators on
# the 1p threshold two-mode file, hybrid k-means/hierarchical
# clustering (hkmeans, k = 4) of the 36 year coordinates.
# --------------------------------------------------------------------

if (!file.exists("run-all.R")) stop("Run scripts from the repository root (the folder containing run-all.R).")
ROOT    <- getwd()
DERIVED <- file.path(ROOT, "data-derived")
OUTF    <- file.path(ROOT, "output", "figures")
dir.create(OUTF, showWarnings = FALSE, recursive = TRUE)

library(FactoMineR)
library(factoextra)
library(haven)
library(ggplot2)

tm <- read_dta(file.path(DERIVED, "ca-data-two-mode-1p.dta"))
tm$EVYY <- factor(tm$EVYY)
# keep EVYY (col 1) and the ITT dummies (col 3 onward); drop EVENTID
md <- tm[, c(1, 3:ncol(tm))]
md[] <- lapply(md, function(x) {
  if (is.numeric(x) && all(na.omit(x) %in% c(0, 1))) {
    factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  } else x
})

mca.res.year <- MCA(md, ncp = 15, graph = FALSE, method = "indicator", na.method = "NA")
year.comps <- data.frame(mca.res.year$var$coord)
year.comps <- year.comps[1:36, 1:12]   # 36 year modalities, first 12 dimensions

res.hc.year <- hkmeans(year.comps, 4)
print(res.hc.year$size)

p.year <- fviz_cluster(res.hc.year, show.clust.cent = FALSE,
                       repel = TRUE, geom = "text", labelsize = 16) +
  theme_bw() +
  scale_fill_grey(start = 0, end = .5) +
  scale_colour_grey(start = 0, end = .5) +
  geom_hline(yintercept = 0, colour = "gray55") +
  geom_vline(xintercept = 0, colour = "gray55") +
  labs(title = " ")

ggsave(file.path(OUTF, "figure2-years-hclustering.png"), p.year,
       width = 16, height = 10, dpi = 300)
cat("Saved output/figures/figure2-years-hclustering.png\n")
