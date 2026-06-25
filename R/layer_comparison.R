# =============================================================================
# Geluo Cave (GLD) — Inter-layer comparison of the lithic assemblage
# Hypothesis (H0): the assemblage does NOT change across stratigraphic layers
#                  in (A) raw-material composition, (B) artifact size, and
#                  (C) type (class) composition.
# Layers present: Layer 2 (n=104) vs Layer 3 (n=232)  -> a two-group comparison.
#
# NOTE: Trench and Layer are partially confounded (T2 ~ mostly L3, T3 ~ mostly
#       L2). Per current decision this is NOT addressed here; trenches are
#       pooled. Treat layer differences as potentially spatial in part.
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(effsize)
  library(scales)
  library(rstatix)
  library(ggpubr)
})

set.seed(42)                       # reproducible Monte-Carlo p-values
data_file <- "data/GLD_lithic_data.xlsx"
out_dir   <- "output"
if (!dir.exists(out_dir)) dir.create(out_dir)
source("R/plot_style.R")

# ---- helpers ----------------------------------------------------------------
cramers_v <- function(tab) {
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)
  n   <- sum(tab)
  k   <- min(nrow(tab), ncol(tab))
  as.numeric(sqrt(chi / (n * (k - 1))))
}

# ---- read & combine all five class sheets -----------------------------------
sheets <- c("Core", "Complete_flake", "Broken_flake", "Tool", "Chunk")
common <- c("ID","Trench","Layer","Raw_mat","weath_deg",
            "Length","Width","Thickness","Mass")

read_sheet <- function(s) {
  df <- read_excel(data_file, sheet = s)
  names(df) <- trimws(names(df))
  df$Class <- s
  df[, c(intersect(common, names(df)), "Class")]
}
all_art <- bind_rows(lapply(sheets, read_sheet))

all_art <- all_art %>%
  mutate(
    Layer = factor(Layer, levels = c(2, 3), labels = c("Layer 2", "Layer 3")),
    Class = factor(Class,
                   levels = c("Core","Complete_flake","Broken_flake","Tool","Chunk"))
  )

cf <- all_art %>% filter(Class == "Complete_flake")   # size sub-set

# capture all console output to a file as well
sink(file.path(out_dir, "results_summary.txt"), split = TRUE)
cat("Total artifacts:", nrow(all_art), "\n")
cat("By layer:\n"); print(table(all_art$Layer))

# =============================================================================
# A. RAW-MATERIAL COMPOSITION  (all artifacts, n = 336)
# =============================================================================
cat("\n\n=========== A. RAW-MATERIAL COMPOSITION ===========\n")
major <- c("Spilite","Quartz","Andesite","Chert","Granite")
all_art <- all_art %>%
  mutate(Raw_grp = factor(ifelse(Raw_mat %in% major, Raw_mat, "Other"),
                          levels = c(major, "Other")))

tab_raw <- table(all_art$Raw_grp, all_art$Layer)
cat("\nCounts:\n");        print(addmargins(tab_raw))
cat("\nColumn proportions (within layer):\n")
print(round(prop.table(tab_raw, margin = 2), 3))

chi_raw <- chisq.test(tab_raw, simulate.p.value = TRUE, B = 10000)
cat("\nChi-square (Monte-Carlo, B=10000): X2 =",
    round(chi_raw$statistic, 2), " p =", signif(chi_raw$p.value, 4), "\n")
cat("Cramer's V =", round(cramers_v(tab_raw), 3), "\n")
cat("\nStandardized residuals (|z|>2 ~ notable):\n")
print(round(chi_raw$stdres, 2))

# =============================================================================
# B. SIZE DISTRIBUTION  (complete flakes only)
# =============================================================================
cat("\n\n=========== B. SIZE (complete flakes) ===========\n")
size_vars <- c("Length","Width","Thickness","Mass")

size_res <- lapply(size_vars, function(v) {
  x2 <- cf[[v]][cf$Layer == "Layer 2"]
  x3 <- cf[[v]][cf$Layer == "Layer 3"]
  sh2 <- shapiro.test(x2)$p.value
  sh3 <- shapiro.test(x3)$p.value
  mwu <- suppressWarnings(wilcox.test(x2, x3))
  wt  <- t.test(x2, x3)                       # Welch, for reference
  cd  <- cliff.delta(x2, x3)                  # +ve => Layer 2 > Layer 3
  data.frame(
    Variable = v,
    n_L2 = length(x2), n_L3 = length(x3),
    median_L2 = round(median(x2), 2), median_L3 = round(median(x3), 2),
    mean_L2 = round(mean(x2), 2),     mean_L3 = round(mean(x3), 2),
    shapiro_L2 = signif(sh2, 3),      shapiro_L3 = signif(sh3, 3),
    p_MWU = signif(mwu$p.value, 4),
    p_Welch = signif(wt$p.value, 4),
    cliff_delta = round(cd$estimate, 3),
    magnitude = as.character(cd$magnitude)
  )
})
size_tab <- bind_rows(size_res)
size_tab$p_MWU_holm <- signif(p.adjust(size_tab$p_MWU, "holm"), 4)
cat("\n"); print(size_tab, row.names = FALSE)
cat("\n(cliff_delta > 0 means Layer 2 larger; Holm correction over the 4 tests)\n")

# =============================================================================
# C. TYPE (CLASS) COMPOSITION  (all artifacts, n = 336)
# =============================================================================
cat("\n\n=========== C. TYPE / CLASS COMPOSITION ===========\n")
tab_type <- table(all_art$Class, all_art$Layer)
cat("\nCounts:\n");        print(addmargins(tab_type))
cat("\nColumn proportions (within layer):\n")
print(round(prop.table(tab_type, margin = 2), 3))

chi_type <- chisq.test(tab_type, simulate.p.value = TRUE, B = 10000)
cat("\nChi-square (Monte-Carlo, B=10000): X2 =",
    round(chi_type$statistic, 2), " p =", signif(chi_type$p.value, 4), "\n")
cat("Cramer's V =", round(cramers_v(tab_type), 3), "\n")
cat("\nStandardized residuals (|z|>2 ~ notable):\n")
print(round(chi_type$stdres, 2))

sink()

# =============================================================================
# FIGURES (house style — see R/plot_style.R)
# =============================================================================

# A. raw material proportions
df_raw <- as.data.frame(prop.table(tab_raw, 2))
names(df_raw) <- c("Raw_mat", "Layer", "Prop")
p_raw <- ggplot(df_raw, aes(Layer, Prop, fill = Raw_mat)) +
  geom_col(position = "stack", colour = "#303238", linewidth = 0.25, width = 0.7) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = gld_qual) +
  labs(title = "Raw-material composition by layer", subtitle = "All artifacts (n = 336)",
       y = "Proportion", x = NULL, fill = "Raw material") +
  gld_theme
ggsave(file.path(out_dir, "fig_rawmat.png"), p_raw, width = 6, height = 5, dpi = 300)

# B. complete-flake size box-plots (jitter + box + mean + Mann-Whitney bracket)
df_size <- cf %>%
  select(Layer, all_of(size_vars)) %>%
  pivot_longer(all_of(size_vars), names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = factor(Variable, levels = size_vars))

# per-facet Mann-Whitney brackets (free_y -> per-variable y positions)
mwu_brackets <- df_size %>%
  group_by(Variable) %>%
  wilcox_test(Value ~ Layer) %>%
  ungroup()
facet_rng <- df_size %>%
  group_by(Variable) %>%
  summarise(ymax = max(Value, na.rm = TRUE),
            yrange = diff(range(Value, na.rm = TRUE)), .groups = "drop")
mwu_brackets <- mwu_brackets %>%
  left_join(facet_rng, by = "Variable") %>%
  mutate(y.position = ymax + yrange * 0.08)

p_size <- ggplot(df_size, aes(Layer, Value)) +
  geom_jitter(aes(color = Layer), width = 0.3, height = 0,
              size = 1.4, alpha = 0.55, shape = 16) +
  geom_boxplot(color = "black", fill = NA, width = 0.6,
               linewidth = 0.6, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 2, color = "black") +
  stat_pvalue_manual(mwu_brackets, label = "p", y.position = "y.position",
                     tip.length = 0.012, bracket.size = 0.4, label.size = 3,
                     color = "#202124") +
  facet_wrap(~ Variable, scales = "free_y") +
  scale_color_manual(values = gld_colors) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
  labs(title = "Complete-flake size by layer",
       subtitle = "Length / Width / Thickness (mm), Mass (g); Mann-Whitney p",
       x = NULL, y = NULL) +
  gld_theme +
  theme(panel.grid.major.x = element_blank(), legend.position = "none")
ggsave(file.path(out_dir, "fig_size.png"), p_size, width = 7.2, height = 6, dpi = 300)

# C. type composition proportions
df_type <- as.data.frame(prop.table(tab_type, 2))
names(df_type) <- c("Class", "Layer", "Prop")
p_type <- ggplot(df_type, aes(Layer, Prop, fill = Class)) +
  geom_col(position = "stack", colour = "#303238", linewidth = 0.25, width = 0.7) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = gld_qual) +
  labs(title = "Type (class) composition by layer", subtitle = "All artifacts (n = 336)",
       y = "Proportion", x = NULL, fill = "Class") +
  gld_theme
ggsave(file.path(out_dir, "fig_type.png"), p_type, width = 6, height = 5, dpi = 300)

cat("\nDone. Figures + results_summary.txt written to '", out_dir, "/'.\n", sep = "")
