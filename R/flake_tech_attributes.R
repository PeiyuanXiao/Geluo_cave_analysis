# =============================================================================
# Geluo Cave (GLD) — Complete-flake technological attributes BY Toth type
# x-axis = Toth_type (recoded from Unicode Roman numerals U+2160-2165 to I-VI;
#          BOA = bipolar-on-anvil). Composite of 6 panels, colour mapped to type.
#   numeric (Cortex, Elongation, IPA, n_dors_scar) -> boxplot + jitter + mean
#   categorical (butt_type, Dorsal_scar_pattern)   -> stacked proportion bars
# NOTE: Toth type is defined by platform + dorsal cortex, so the Cortex and
#       butt_type panels are largely circular (they re-express the definition);
#       Elongation / IPA / n_dors_scar / Dorsal_scar_pattern are the informative
#       contrasts. No titles/subtitles (house rule).
# =============================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
})
data_file <- "data/GLD_lithic_data.xlsx"
out_dir   <- "output"
if (!dir.exists(out_dir)) dir.create(out_dir)
source("R/plot_style.R")

cf <- read_excel(data_file, sheet = "Complete_flake"); names(cf) <- trimws(names(cf))

# recode Toth type: Unicode Roman numerals (U+2160..U+2165) -> ASCII
toth_lev  <- c("I", "II", "III", "IV", "V", "VI", "BOA")
toth_keys <- c(intToUtf8(0x2160), intToUtf8(0x2161), intToUtf8(0x2162),
               intToUtf8(0x2163), intToUtf8(0x2164), intToUtf8(0x2165), "BOA")
toth_map  <- setNames(toth_lev, toth_keys)
cf$Toth <- factor(unname(toth_map[cf$Toth_type]), levels = toth_lev)
stopifnot(!any(is.na(cf$Toth)))

toth_colors <- setNames(gld_qual[seq_along(toth_lev)], toth_lev)

# --- boxplot panel (numeric ~ Toth): hollow box, colour only on points --------
box_by_toth <- function(yvar, ylab, data = cf) {
  ggplot(data, aes(Toth, {{ yvar }})) +
    geom_boxplot(fill = NA, color = "black", linewidth = 0.45, width = 0.65,
                 outlier.shape = NA) +
    geom_jitter(aes(color = Toth), width = 0.18, height = 0, size = 1.2,
                alpha = 0.75, shape = 16) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 2, color = "black") +
    scale_color_manual(values = toth_colors) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.08))) +
    labs(x = "Toth type", y = ylab) +
    gld_theme +
    theme(legend.position = "none", panel.grid.major.x = element_blank())
}

p_cortex <- box_by_toth(Cortex,      "Cortex proportion")
p_elong  <- box_by_toth(Elongation,  "Elongation (L/W)")
p_ipa    <- box_by_toth(IPA,         "IPA (degrees)", data = filter(cf, !is.na(IPA)))
p_ndsc   <- box_by_toth(n_dors_scar, "Dorsal scar count")

# --- heatmaps (count of category x Toth) -------------------------------------
heat_panel <- function(tab, xlab) {
  ggplot(tab, aes(cat, Toth, fill = n)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = ifelse(n > 0, n, "")), size = 3, color = "#202124") +
    scale_fill_gradient(low = "#EEF4F9", high = "#6BA8CE") +
    scale_y_discrete(limits = rev(toth_lev)) +
    scale_x_discrete(labels = function(x) gsub("_", " ", x)) +
    labs(x = xlab, y = "Toth type") +
    gld_theme +
    theme(legend.position = "none", panel.grid = element_blank(),
          axis.text.x = element_text(angle = 25, hjust = 1))
}

butt_lev <- c("Cortical", "Plain", "Linear", "Faceted", "Punctiform")
butt_tab <- cf %>%
  mutate(cat = factor(butt_type, levels = butt_lev)) %>%
  count(Toth, cat) %>% complete(Toth, cat, fill = list(n = 0))
p_butt <- heat_panel(butt_tab, "Butt type")

dsp_lev <- cf %>%
  mutate(dp = ifelse(is.na(Dorsal_scar_pattern), "Indeterminate", Dorsal_scar_pattern)) %>%
  count(dp) %>% arrange(desc(n)) %>% pull(dp)
dsp_tab <- cf %>%
  mutate(cat = factor(ifelse(is.na(Dorsal_scar_pattern), "Indeterminate",
                             Dorsal_scar_pattern), levels = dsp_lev)) %>%
  count(Toth, cat) %>% complete(Toth, cat, fill = list(n = 0))
p_dsp <- heat_panel(dsp_tab, "Dorsal scar pattern")

combo <- (p_cortex | p_elong) / (p_ipa | p_ndsc) / (p_butt | p_dsp) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 13))

ggsave(file.path(out_dir, "fig_flake_tech_attributes.png"), combo,
       width = 10, height = 10, dpi = 300)

cat("Toth type counts:\n"); print(table(cf$Toth))
cat("\nDone. fig_flake_tech_attributes.png written.\n")
