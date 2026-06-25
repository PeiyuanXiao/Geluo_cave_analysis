# =============================================================================
# Geluo Cave (GLD) — Inter-layer comparison of TECHNOLOGICAL TYPE
#   (1) Complete flakes: tech_type, EXCLUDING Cortical_flake
#   (2) Cores: Typology
# Layers: Layer 2 vs Layer 3.
#
# WARNING — small / sparse samples:
#   flakes n = 112 (dominated by Unipolar); cores n = 16.
#   Tests use Fisher's exact (Monte-Carlo) but power is very low, esp. cores.
#   A non-significant result here is NOT evidence of "no change".
#   Trenches pooled (Trench/Layer confound not addressed, per current decision).
# =============================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(ggplot2); library(scales)
})
set.seed(42)
data_file <- "data/GLD_lithic_data.xlsx"
out_dir   <- "output"
if (!dir.exists(out_dir)) dir.create(out_dir)
source("R/plot_style.R")

cramers_v <- function(tab) {
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)
  n <- sum(tab); k <- min(nrow(tab), ncol(tab))
  as.numeric(sqrt(chi / (n * (k - 1))))
}
lay <- function(x) factor(x, levels = c(2, 3), labels = c("Layer 2", "Layer 3"))

report <- function(tab, label) {
  cat("\n--", label, "--\n")
  cat("\nCounts:\n");                    print(addmargins(tab))
  cat("\nProportions within layer:\n");  print(round(prop.table(tab, 2), 3))
  ft <- fisher.test(tab, simulate.p.value = TRUE, B = 10000)
  cat("\nFisher's exact (Monte-Carlo, B=10000):  p =", signif(ft$p.value, 4), "\n")
  cat("Cramer's V =", round(cramers_v(tab), 3), "\n")
  invisible(ft)
}

cf   <- read_excel(data_file, sheet = "Complete_flake"); names(cf)   <- trimws(names(cf))
core <- read_excel(data_file, sheet = "Core");           names(core) <- trimws(names(core))
cf$Layer   <- lay(cf$Layer)
core$Layer <- lay(core$Layer)

sink(file.path(out_dir, "techtype_summary.txt"), split = TRUE)

# =============================================================================
# (1) COMPLETE-FLAKE tech_type  (exclude Cortical_flake)
# =============================================================================
cat("================ (1) FLAKE tech_type (excl. Cortical_flake) ================\n")
cf2 <- cf %>% filter(tech_type != "Cortical_flake")
cat("n =", nrow(cf2), " (L2:", sum(cf2$Layer == "Layer 2"),
    " L3:", sum(cf2$Layer == "Layer 3"), ")\n")

tab_tt <- table(cf2$tech_type, cf2$Layer)
report(tab_tt, "Full categories")

# Collapsed by reduction strategy: Unipolar / Bipolar / Other
cf2 <- cf2 %>%
  mutate(tt_grp = case_when(
    tech_type == "Unipolar_flake" ~ "Unipolar",
    tech_type %in% c("Bipolar_flake", "Bipolar_on_anvil_flake") ~ "Bipolar",
    TRUE ~ "Other"
  ) %>% factor(levels = c("Unipolar", "Bipolar", "Other")))
tab_ttg <- table(cf2$tt_grp, cf2$Layer)
report(tab_ttg, "Collapsed: Unipolar / Bipolar / Other")

# =============================================================================
# (2) CORE Typology
# =============================================================================
cat("\n\n================ (2) CORE Typology ================\n")
cat("n =", nrow(core), " (L2:", sum(core$Layer == "Layer 2"),
    " L3:", sum(core$Layer == "Layer 3"), ")\n")
cat("** n=16 — descriptive only; any test is severely underpowered. **\n")

tab_cty <- table(core$Typology, core$Layer)
report(tab_cty, "Full categories")

core <- core %>%
  mutate(ty_grp = case_when(
    Typology == "Bifacial_exploitation"  ~ "Bifacial",
    Typology == "Unifacial_exploitation" ~ "Unifacial",
    TRUE ~ "Other"
  ) %>% factor(levels = c("Bifacial", "Unifacial", "Other")))
tab_ctyg <- table(core$ty_grp, core$Layer)
report(tab_ctyg, "Collapsed: Bifacial / Unifacial / Other")

sink()

# =============================================================================
# FIGURES
# =============================================================================
bar_prop <- function(tab, fill_lab, title, subtitle = NULL) {
  d <- as.data.frame(prop.table(tab, 2)); names(d) <- c("Cat", "Layer", "Prop")
  ggplot(d, aes(Layer, Prop, fill = Cat)) +
    geom_col(colour = "#303238", linewidth = 0.25, width = 0.7) +
    scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.02))) +
    scale_fill_manual(values = gld_qual, labels = function(x) gsub("_", " ", x)) +
    labs(title = title, subtitle = subtitle, x = NULL, y = "Proportion", fill = fill_lab) +
    gld_theme
}
ggsave(file.path(out_dir, "fig_flake_techtype.png"),
       bar_prop(tab_tt,  "tech_type", "Flake tech_type by layer",
                "Complete flakes, excl. Cortical (n = 112)"),
       width = 6.8, height = 5, dpi = 300)
ggsave(file.path(out_dir, "fig_flake_techtype_grouped.png"),
       bar_prop(tab_ttg, "Strategy", "Flake reduction strategy by layer",
                "Unipolar / Bipolar / Other"),
       width = 5.8, height = 5, dpi = 300)
ggsave(file.path(out_dir, "fig_core_typology.png"),
       bar_prop(tab_cty, "Typology", "Core Typology by layer",
                "n = 16 — descriptive only"),
       width = 6.8, height = 5, dpi = 300)

cat("\nDone. See output/techtype_summary.txt and fig_*.png\n")
