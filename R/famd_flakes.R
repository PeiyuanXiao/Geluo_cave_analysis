# =============================================================================
# Geluo Cave (GLD) — FAMD on complete flakes
# Active variables (cleaned per discussion):
#   quantitative : Length, Width, Thickness, log(Mass), n_dors_scar, IPA
#   qualitative  : butt_type, Dorsal_scar_pattern
# Dropped: Elongation, Thinness (exact ratios of L/W/T -> redundant)
#          butt_dep            (redundant with Thickness, r=0.86; structurally NA)
# IPA: 1176 typo corrected in source. Linear/Punctiform butts (no measurable
#      platform angle) coded IPA = 0 per decision. 2 Cortical butts with merely
#      *unrecorded* IPA are dropped (n 118 -> 116) — coding 0 would misrepresent
#      them. NOTE: the 0 spike sits far below the real range (82-133), and
#      "IPA==0" mirrors butt_type linear/punctiform (mild redundancy).
# Dorsal_scar_pattern: 7 NAs recoded to existing level "Indeterminate"
#                      (reversible).
# Layer is a SUPPLEMENTARY variable: it does NOT shape the axes, but is
#   projected so we can see / test whether layers separate in tech-morphospace.
# Trenches pooled (Trench/Layer confound not addressed, per current decision).
# =============================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(ggplot2)
  library(FactoMineR); library(factoextra)
  library(cluster); library(vegan)
})
set.seed(42)
data_file <- "data/GLD_lithic_data.xlsx"
out_dir   <- "output"
if (!dir.exists(out_dir)) dir.create(out_dir)
source("R/plot_style.R")

cf <- read_excel(data_file, sheet = "Complete_flake"); names(cf) <- trimws(names(cf))

dat <- cf %>%
  mutate(IPA = ifelse(butt_type %in% c("Linear", "Punctiform"), 0,
                      as.numeric(IPA))) %>%   # structural NA -> 0
  transmute(
    Length, Width, Thickness,
    logMass     = log(Mass),
    n_dors_scar = as.numeric(n_dors_scar),
    IPA,
    butt_type           = factor(butt_type),
    Dorsal_scar_pattern = factor(ifelse(is.na(Dorsal_scar_pattern),
                                         "Indeterminate", Dorsal_scar_pattern)),
    Layer = factor(Layer, levels = c(2, 3), labels = c("Layer 2", "Layer 3"))
  ) %>%
  filter(!is.na(IPA)) %>%                     # drop 2 cortical-butt unrecorded IPA
  as.data.frame()

stopifnot(sum(is.na(dat)) == 0)            # must be complete for FAMD

sink(file.path(out_dir, "famd_summary.txt"), split = TRUE)
cat("FAMD input: n =", nrow(dat), " active vars = 8 (6 quant + 2 qual)\n")
cat("butt_type levels:\n");           print(table(dat$butt_type))
cat("Dorsal_scar_pattern levels:\n"); print(table(dat$Dorsal_scar_pattern))

# ---- FAMD (Layer supplementary) ---------------------------------------------
sup <- which(names(dat) == "Layer")
res <- FAMD(dat, sup.var = sup, ncp = 5, graph = FALSE)

cat("\n=== Eigenvalues / variance explained ===\n")
print(round(res$eig, 3))

cat("\n=== Variable contributions to Dim.1 & Dim.2 (%) ===\n")
print(round(res$var$contrib[, 1:2], 2))

cat("\n=== Quantitative variables: correlation with Dim.1 & Dim.2 ===\n")
print(round(res$quanti.var$coord[, 1:2], 3))

# ---- Does LAYER separate in the tech-morphospace? ---------------------------
ind <- as.data.frame(res$ind$coord); ind$Layer <- dat$Layer
cat("\n=== Layer differences on FAMD axes (Mann-Whitney) ===\n")
for (d in c("Dim.1", "Dim.2", "Dim.3")) {
  p <- suppressWarnings(wilcox.test(ind[[d]] ~ ind$Layer)$p.value)
  cat(sprintf("  %s: median L2=%.2f L3=%.2f  MWU p=%.4f\n", d,
              median(ind[[d]][ind$Layer == "Layer 2"]),
              median(ind[[d]][ind$Layer == "Layer 3"]), p))
}

cat("\n=== PERMANOVA on Gower distance (active vars) ~ Layer ===\n")
gw <- daisy(dat[, setdiff(names(dat), "Layer")], metric = "gower")
print(adonis2(gw ~ dat$Layer, permutations = 9999))
cat("\n-- multivariate dispersion (betadisper) check --\n")
print(anova(betadisper(gw, dat$Layer)))
sink()

# =============================================================================
# FIGURES (house style — see R/plot_style.R)
# =============================================================================
v1 <- round(res$eig[1, 2], 1); v2 <- round(res$eig[2, 2], 1)

# scree
ggsave(file.path(out_dir, "fig_famd_scree.png"),
       gld_scree(res$eig) + labs(title = "Complete-flake FAMD"),
       width = 6, height = 4.5, dpi = 300)

# individuals ordination (hulls + spokes + centroids)
scores <- data.frame(Dim1 = res$ind$coord[, 1], Dim2 = res$ind$coord[, 2],
                     Group = dat$Layer)
ggsave(file.path(out_dir, "fig_famd_ind_layer.png"),
       gld_ordination(scores,
                      xlab = paste0("Dim.1 (", v1, "%)"),
                      ylab = paste0("Dim.2 (", v2, "%)"),
                      title = "Complete flakes in FAMD space",
                      subtitle = "Layer projected as supplementary variable"),
       width = 7.0, height = 5.6, dpi = 300)

# quantitative variable coordinates (loading bars)
qc <- as.data.frame(res$quanti.var$coord[, 1:2])
qc <- data.frame(Variable = rownames(qc), Dim1 = qc[, 1], Dim2 = qc[, 2])
ggsave(file.path(out_dir, "fig_famd_loadings.png"),
       gld_loadings(qc, var_levels = qc$Variable,
                    subtitle = "Quantitative variable coordinates"),
       width = 7.4, height = 4.4, dpi = 300)

# variable contributions to Dim.1 + Dim.2
ct <- data.frame(Variable = rownames(res$var$contrib),
                 contrib = rowSums(res$var$contrib[, 1:2]))
ggsave(file.path(out_dir, "fig_famd_contrib.png"), gld_contrib(ct),
       width = 6.4, height = 4.4, dpi = 300)

# qualitative category coordinates
qv <- as.data.frame(res$quali.var$coord[, 1:2])
qv <- data.frame(Dim1 = qv[, 1], Dim2 = qv[, 2], Cat = rownames(qv))
p_quali <- ggplot(qv, aes(Dim1, Dim2)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_point(color = "#6BA8CE", size = 2.6) +
  geom_text(aes(label = gsub("_", " ", Cat)), size = 3, vjust = -0.7,
            color = "#303238", check_overlap = TRUE) +
  labs(subtitle = "Qualitative category coordinates",
       x = paste0("Dim.1 (", v1, "%)"), y = paste0("Dim.2 (", v2, "%)")) +
  gld_theme
ggsave(file.path(out_dir, "fig_famd_quali.png"), p_quali,
       width = 7.0, height = 5.2, dpi = 300)

cat("\nDone. See output/famd_summary.txt and fig_famd_*.png\n")
