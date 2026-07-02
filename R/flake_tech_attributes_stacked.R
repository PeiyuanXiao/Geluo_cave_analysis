# GLD complete-flake technological attributes by Toth type: STACKED-BAR variant.
# Boxplots (a-c) identical to flake_tech_attributes.R; d-f are stacked count bars
# (bar height = total count, viridis segments = category shares). Kept alongside the
# bubble version for comparison -> fig_flake_tech_attributes_stacked.png.

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
})
data_file <- "data/GLD_lithic_data.xlsx"
out_dir   <- "output"
if (!dir.exists(out_dir)) dir.create(out_dir)
source("R/plot_style.R")

cf <- read_excel(data_file, sheet = "Complete_flake"); names(cf) <- trimws(names(cf))

# recode Toth type: Unicode Roman numerals (U+2160..U+2165) -> ASCII; BOA appended
toth_lev  <- c("I", "II", "III", "IV", "V", "VI", "BOA")
toth_keys <- c(intToUtf8(0x2160), intToUtf8(0x2161), intToUtf8(0x2162),
               intToUtf8(0x2163), intToUtf8(0x2164), intToUtf8(0x2165), "BOA")
toth_map  <- setNames(toth_lev, toth_keys)
cf$Toth <- factor(unname(toth_map[cf$Toth_type]), levels = toth_lev)
stopifnot(!any(is.na(cf$Toth)))

# ordered viridis gradient for I-VI; BOA (separate technology) gets a warm swatch
toth_seq    <- c("#482878", "#3E4A89", "#31688E", "#21908C", "#35B779", "#90D743")
toth_colors <- setNames(c(toth_seq, "#D9A07A"), toth_lev)

box_by_toth <- function(yvar, ylab, data = cf) {
  box_width <- 0.65
  ggplot(data, aes(Toth, {{ yvar }})) +
    geom_jitter(aes(color = Toth), width = box_width / 2, height = 0, size = 1.2,
                alpha = 0.75, shape = 16) +
    geom_boxplot(fill = NA, color = "black", linewidth = 0.45, width = box_width,
                 outlier.shape = NA) +
    stat_summary(fun = mean, geom = "point", shape = 21, size = 2.1,
                 color = "black", fill = "black", stroke = 0.45) +
    scale_color_manual(values = toth_colors) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.08))) +
    labs(x = "Toth type", y = ylab) +
    gld_theme +
    theme(legend.position = "none", panel.grid.major.x = element_blank())
}

p_elong  <- box_by_toth(Elongation,  "Elongation (L/W)")
p_ipa    <- box_by_toth(IPA,         "IPA (degrees)", data = filter(cf, !is.na(IPA)))
p_ndsc   <- box_by_toth(n_dors_scar, "Dorsal scar count")

# stacked count bars: x = Toth, height = total count, viridis fill = category share
stacked_bar_panel <- function(tab, fill_lab) {
  ggplot(tab, aes(Toth, n, fill = cat)) +
    geom_col(width = 0.8, color = "white", linewidth = 0.25) +
    scale_fill_viridis_d(name = fill_lab,
                         labels = function(x) gsub("_", " ", x)) +
    scale_x_discrete(limits = toth_lev) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Toth type", y = NULL) +
    gld_theme +
    theme(legend.position = "right", panel.grid.major.x = element_blank())
}

# category order = stack/legend order (Central/Primary swapped)
cloc_lev <- c("Tertiary", "Crescent", "Distal", "Central", "Primary")
cloc_tab <- cf %>%
  mutate(cat = factor(Cortex_loc, levels = cloc_lev)) %>%
  count(Toth, cat) %>% complete(Toth, cat, fill = list(n = 0))
p_cloc <- stacked_bar_panel(cloc_tab, "Cortex location")

# Platform type (Faceted/Linear swapped)
butt_lev <- c("Cortical", "Plain", "Faceted", "Linear", "Punctiform")
butt_tab <- cf %>%
  mutate(cat = factor(butt_type, levels = butt_lev)) %>%
  count(Toth, cat) %>% complete(Toth, cat, fill = list(n = 0))
p_butt <- stacked_bar_panel(butt_tab, "Platform type")

# NA -> Indeterminate; explicit category order
dsp_lev <- c("Cortex", "Unidirectional_proximal", "Bidirectional_opposite",
             "Lateral", "Orthogonal", "Multidirectional", "Centripetal",
             "Kombewa", "Indeterminate")
dsp_tab <- cf %>%
  mutate(cat = factor(ifelse(is.na(Dorsal_scar_pattern), "Indeterminate",
                             Dorsal_scar_pattern), levels = dsp_lev)) %>%
  count(Toth, cat) %>% complete(Toth, cat, fill = list(n = 0))
p_dsp <- stacked_bar_panel(dsp_tab, "Dorsal scar pattern")

# x title only on the bottom row (c, f)
p_elong <- p_elong + labs(x = NULL)
p_ipa   <- p_ipa   + labs(x = NULL)
p_cloc  <- p_cloc  + labs(x = NULL)
p_butt  <- p_butt  + labs(x = NULL)

# left column = boxplots (a-c), right column = stacked bars (d-f)
combo <- wrap_plots(list(p_elong, p_ipa, p_ndsc, p_cloc, p_butt, p_dsp),
                    ncol = 2, byrow = FALSE) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 13))

ggsave(file.path(out_dir, "fig_flake_tech_attributes_stacked.png"), combo,
       width = 9.5, height = 10, dpi = 300)

cat("Toth type counts:\n"); print(table(cf$Toth))
cat("\nDone. fig_flake_tech_attributes_stacked.png written.\n")
