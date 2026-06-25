# =============================================================================
# Geluo Cave (GLD) — FAMD + PERMANOVA on CORES
# Active variables:
#   quantitative : Length, Width, Thickness, log(Mass), Flake_angle, Tot_scar
#   qualitative  : Techno_organization
# Excluded Techno_organization levels: Bipolar_on_anvil, Initial_exploitation
#   -> n 16 -> 14 (L2=8, L3=6). The 2 excluded cores carried the only NAs, so
#      the 14 retained cores are complete.
# Layer is SUPPLEMENTARY (projected, does not shape axes).
#
# ***  WARNING: n = 14. This is EXPLORATORY / DESCRIPTIVE only.  ***
#   - FAMD with 6 quant + 1 qual var at n=14 is overfit/unstable.
#   - PERMANOVA by Layer (8 vs 6) has almost no power: a non-significant
#     result is uninformative; a significant one rests on a few specimens.
#   - 'Multifacial_bidirection' & 'Unifacial_oppsite' are singletons, both in
#     Layer 2 -> may create artefactual separation along a FAMD axis.
#   - log(Mass) used to damp a 1246 g outlier.  Trenches pooled.
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

co <- read_excel(data_file, sheet = "Core"); names(co) <- trimws(names(co))

excl <- c("Bipolar_on_anvil", "Initial_exploitation")
dat <- co %>%
  filter(!Techno_organization %in% excl) %>%
  transmute(
    Length, Width, Thickness,
    logMass     = log(Mass),
    Flake_angle = as.numeric(Flake_angle),
    Tot_scar    = as.numeric(Tot_scar),
    Techno_organization = factor(Techno_organization),
    Layer = factor(Layer, levels = c(2, 3), labels = c("Layer 2", "Layer 3"))
  ) %>%
  droplevels() %>% as.data.frame()

stopifnot(sum(is.na(dat)) == 0)

sink(file.path(out_dir, "famd_cores_summary.txt"), split = TRUE)
cat("*** EXPLORATORY: n =", nrow(dat), " (L2 =", sum(dat$Layer == "Layer 2"),
    " L3 =", sum(dat$Layer == "Layer 3"), ") — interpret with extreme caution ***\n")
cat("\nTechno_organization x Layer:\n")
print(addmargins(table(dat$Techno_organization, dat$Layer)))

# ---- FAMD (Layer supplementary) ---------------------------------------------
sup <- which(names(dat) == "Layer")
res <- FAMD(dat, sup.var = sup, ncp = 5, graph = FALSE)

cat("\n=== Eigenvalues / variance explained ===\n");   print(round(res$eig, 3))
cat("\n=== Variable contributions to Dim.1 & Dim.2 (%) ===\n")
print(round(res$var$contrib[, 1:2], 2))
cat("\n=== Quantitative variables: corr with Dim.1 & Dim.2 ===\n")
print(round(res$quanti.var$coord[, 1:2], 3))

# ---- Layer separation -------------------------------------------------------
ind <- as.data.frame(res$ind$coord); ind$Layer <- dat$Layer
cat("\n=== Layer differences on FAMD axes (Mann-Whitney) ===\n")
for (d in c("Dim.1", "Dim.2")) {
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

ggsave(file.path(out_dir, "fig_core_famd_scree.png"),
       gld_scree(res$eig) + labs(title = "Core FAMD"),
       width = 6, height = 4.5, dpi = 300)

scores <- data.frame(Dim1 = res$ind$coord[, 1], Dim2 = res$ind$coord[, 2],
                     Group = dat$Layer)
ggsave(file.path(out_dir, "fig_core_famd_ind_layer.png"),
       gld_ordination(scores,
                      xlab = paste0("Dim.1 (", v1, "%)"),
                      ylab = paste0("Dim.2 (", v2, "%)"),
                      title = "Cores in FAMD space (n = 14)",
                      subtitle = "Exploratory; Layer supplementary; L2 singletons may distort"),
       width = 7.0, height = 5.6, dpi = 300)

qc <- as.data.frame(res$quanti.var$coord[, 1:2])
qc <- data.frame(Variable = rownames(qc), Dim1 = qc[, 1], Dim2 = qc[, 2])
ggsave(file.path(out_dir, "fig_core_famd_loadings.png"),
       gld_loadings(qc, var_levels = qc$Variable,
                    subtitle = "Quantitative variable coordinates"),
       width = 7.4, height = 4.4, dpi = 300)

ct <- data.frame(Variable = rownames(res$var$contrib),
                 contrib = rowSums(res$var$contrib[, 1:2]))
ggsave(file.path(out_dir, "fig_core_famd_contrib.png"), gld_contrib(ct),
       width = 6.4, height = 4.4, dpi = 300)

qv <- as.data.frame(res$quali.var$coord[, 1:2])
qv <- data.frame(Dim1 = qv[, 1], Dim2 = qv[, 2], Cat = rownames(qv))
p_quali <- ggplot(qv, aes(Dim1, Dim2)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_point(color = "#6BA8CE", size = 2.6) +
  geom_text(aes(label = gsub("_", " ", Cat)), size = 3, vjust = -0.7,
            color = "#303238", check_overlap = TRUE) +
  labs(subtitle = "Techno_organization category coordinates",
       x = paste0("Dim.1 (", v1, "%)"), y = paste0("Dim.2 (", v2, "%)")) +
  gld_theme
ggsave(file.path(out_dir, "fig_core_famd_quali.png"), p_quali,
       width = 7.0, height = 5.2, dpi = 300)

cat("\nDone. See output/famd_cores_summary.txt and fig_core_famd_*.png\n")
