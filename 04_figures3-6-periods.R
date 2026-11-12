# --------------------------------------------------------------------
# 04: Figures 3-6 — divisive hierarchical clustering PCA plots per period
#
# Port of R/pca-plots-final-paper.R (plots p1-p4), including the
# original annotation geometry. Requires data-derived/ITT-MCA.RData
# from 02_mca-clustering-periods.R.
# --------------------------------------------------------------------

if (!file.exists("run-all.R")) stop("Run scripts from the repository root (the folder containing run-all.R).")
ROOT    <- getwd()
DERIVED <- file.path(ROOT, "data-derived")
OUTF    <- file.path(ROOT, "output", "figures")

library(factoextra)
library(ggplot2)

load(file.path(DERIVED, "ITT-MCA.RData"))

# ---- Figure 3: Period 1 (1960-1965) ------------------------------------
p1 <- fviz_cluster(res.hc1, show.clust.cent = FALSE, labelsize = 8, repel = TRUE, geom = "text") +
  theme_minimal() +
  geom_hline(yintercept = 0, colour = "gray55") +
  geom_vline(xintercept = 0, colour = "gray55") +
  scale_fill_grey(start = 0, end = .7) +
  scale_color_grey(start = 0, end = .7) +
  ggtitle(" ") + theme(legend.position = "none") +
  annotate("text", x = 1.5, y = -0.2, label = "National Political Conformist", fontface = "bold") +
  annotate("text", x = -0.7, y = -2.3, label = "Racial Aggressor", fontface = "bold") +
  annotate("text", x = -0.2, y = 0.37, label = "Civil Rights", fontface = "bold") +
  annotate("text", x = -1.2, y = 0.5, label = "Local Political Crusader", fontface = "bold") +
  annotate("text", x = 2.4, y = -3, label = "National", size = 6) +
  annotate("text", x = -1.7, y = -3, label = "Local", size = 6) +
  annotate("text", x = -2.1, y = -2.7, label = "Racial Agressor", size = 6) +
  annotate("text", x = -2.2, y = 2.2, label = "Others", size = 6) +
  geom_segment(aes(x = -1.5, xend = 2.2, y = -3, yend = -3), size = 1.75, arrow = arrow()) +
  geom_segment(aes(x = 2.2, xend = -1.5, y = -3, yend = -3), size = 1.75, arrow = arrow()) +
  geom_segment(aes(x = -2.2, xend = -2.2, y = -2.5, yend = 2), size = 1.75, arrow = arrow()) +
  geom_segment(aes(x = -2.2, xend = -2.2, y = 2, yend = -2.5), size = 1.75, arrow = arrow())
ggsave(file.path(OUTF, "figure3-period1-pca-hclust.png"), p1, width = 16, height = 10, dpi = 300)

# ---- Figure 4: Period 2 (1966-1972) ------------------------------------
p2 <- fviz_cluster(res.hc2, show.clust.cent = FALSE, labelsize = 8, repel = TRUE, geom = "text") +
  theme_minimal() +
  geom_hline(yintercept = 0, colour = "gray55") +
  geom_vline(xintercept = 0, colour = "gray55") +
  scale_fill_grey(start = 0, end = .7) +
  scale_color_grey(start = 0, end = .7) +
  ggtitle(" ") + theme(legend.position = "none") +
  annotate("text", x = 0.25, y = 1, label = "National Political Conformists", fontface = "bold") +
  annotate("text", x = -3, y = -0.2, label = "Racial Aggressors", fontface = "bold") +
  annotate("text", x = 0.1, y = -1.5, label = "School Obstructionists", fontface = "bold") +
  annotate("text", x = 0.75, y = -0.58, label = "Local Political Crusaders", fontface = "bold") +
  annotate("text", x = 1.2, y = -3, label = "Others", size = 6) +
  annotate("text", x = -3.8, y = -3, label = "Racial Agressor", size = 6) +
  annotate("text", x = -4.6, y = -2.7, label = "Local", size = 6) +
  annotate("text", x = -4.6, y = 2.2, label = "National", size = 6) +
  geom_segment(aes(x = -3.4, xend = 1, y = -3, yend = -3), size = 1.75, arrow = arrow()) +
  geom_segment(aes(x = 1, xend = -3.4, y = -3, yend = -3), size = 1.75, arrow = arrow()) +
  geom_segment(aes(x = -4.4, xend = -4.4, y = -2.5, yend = 2), size = 1.75, arrow = arrow()) +
  geom_segment(aes(x = -4.4, xend = -4.4, y = 2, yend = -2.5), size = 1.75, arrow = arrow())
ggsave(file.path(OUTF, "figure4-period2-pca-hclust.png"), p2, width = 16, height = 10, dpi = 300)

# ---- Figure 5: Period 3 (1973-1987) ------------------------------------
p3 <- fviz_cluster(res.hc3, show.clust.cent = FALSE, labelsize = 8, repel = TRUE, geom = "text") +
  theme_minimal() +
  geom_hline(yintercept = 0, colour = "gray55") +
  geom_vline(xintercept = 0, colour = "gray55") +
  scale_fill_grey(start = 0, end = .7) +
  scale_color_grey(start = 0, end = .7) +
  ggtitle(" ") + theme(legend.position = "none") +
  annotate("text", x = 0.75, y = 0.9, label = "National Political Conformists", fontface = "bold") +
  annotate("text", x = 0.6, y = -1.7, label = "Racial Aggressors", fontface = "bold") +
  annotate("text", x = 2.2, y = 0.15, label = "School Obstructionists", fontface = "bold") +
  annotate("text", x = -0.8, y = -0.95, label = "Local Political Crusaders", fontface = "bold") +
  annotate("text", x = 2.7, y = 1.8, label = "Non-Political Obstructionists", size = 6) +
  annotate("text", x = -2.3, y = 1.8, label = "Political", size = 6) +
  annotate("text", x = -1.95, y = -3.8, label = "Non-Political Aggressors", size = 6) +
  geom_segment(aes(x = -2, xend = 2, y = 1.8, yend = 1.8), size = 1.75, arrow = arrow()) +
  geom_segment(aes(x = 2, xend = -2, y = 1.8, yend = 1.8), size = 1.75, arrow = arrow()) +
  geom_segment(aes(x = -2.2, xend = -2.2, y = -3.7, yend = 1.6), size = 1.75, arrow = arrow()) +
  geom_segment(aes(x = -2.2, xend = -2.2, y = 1.6, yend = -3.7), size = 1.75, arrow = arrow())
ggsave(file.path(OUTF, "figure5-period3-pca-hclust.png"), p3, width = 16, height = 10, dpi = 300)

# ---- Figure 6: Period 4 (1988-1995) ------------------------------------
p4 <- fviz_cluster(res.hc4, show.clust.cent = FALSE, labelsize = 8, repel = TRUE, geom = "text") +
  theme_bw() +
  geom_hline(yintercept = 0, colour = "gray55") +
  geom_vline(xintercept = 0, colour = "gray55") +
  scale_fill_grey(start = 0, end = .7) +
  scale_color_grey(start = 0, end = .7) +
  ggtitle(" ") + theme(legend.position = "none") +
  annotate("text", x = -1, y = -0.65, label = "National Political Conformists", fontface = "bold") +
  annotate("text", x = 1.25, y = -1.75, label = "Racial Aggressors", fontface = "bold") +
  annotate("text", x = -3.25, y = 0.15, label = "School Obstructionists", fontface = "bold") +
  annotate("text", x = 1, y = 1.5, label = "Local Political Crusaders", fontface = "bold") +
  annotate("text", x = -1.1, y = -2.9, label = "Pro-Life Interventionist", fontface = "bold") +
  annotate("text", x = 2.5, y = 1.8, label = "Political", size = 6) +
  annotate("text", x = -3.7, y = 1.8, label = "Non-Political Obstructionists", size = 6) +
  annotate("text", x = 2.1, y = -3.2, label = "Non-Political Aggressors", size = 6) +
  geom_segment(aes(x = -2.8, xend = 2.1, y = 1.8, yend = 1.8), size = 1.75, arrow = arrow()) +
  geom_segment(aes(x = 2.1, xend = -2.8, y = 1.8, yend = 1.8), size = 1.75, arrow = arrow()) +
  geom_segment(aes(x = 2.5, xend = 2.5, y = -3, yend = 1.6), size = 1.75, arrow = arrow()) +
  geom_segment(aes(x = 2.5, xend = 2.5, y = 1.6, yend = -3), size = 1.75, arrow = arrow())
ggsave(file.path(OUTF, "figure6-period4-pca-hclust.png"), p4, width = 16, height = 10, dpi = 300)

cat("Saved figures 3-6 to", OUTF, "\n")
