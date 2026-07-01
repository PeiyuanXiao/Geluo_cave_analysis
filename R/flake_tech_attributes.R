# =============================================================================
# Geluo Cave (GLD) — Complete-flake technological attributes BY Toth type
# x-axis = Toth_type (recoded from Unicode Roman numerals U+2160-2165 to I-VI;
#          BOA = bipolar-on-anvil). Composite of 6 panels, colour mapped to type.
#   numeric (Elongation, IPA, n_dors_scar)                   -> boxplot + jitter + mean
#   categorical (Cortex_loc, butt_type, Dorsal_scar_pattern) -> count heatmaps
# NOTE: Toth type is defined by platform + dorsal cortex, so the Cortex_loc and
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

# --- bubble panels (count of category x Toth) --------------------------------
bubble_panel <- function(tab, xlab) {
  max_count <- max(tab$n, na.rm = TRUE)

  ggplot(tab, aes(cat, Toth)) +
    geom_point(data = filter(tab, n > 0), aes(fill = n),
               shape = 21, size = 7.5, color = "grey25", stroke = 0.25,
               alpha = 0.95) +
    geom_text(aes(label = ifelse(n > 0, n, "")), size = 3, color = "#202124") +
    scale_fill_gradient(low = "#EEF4F9", high = "#6BA8CE",
                        limits = c(0, max_count)) +
    scale_y_discrete(limits = rev(toth_lev)) +
    scale_x_discrete(labels = function(x) gsub("_", " ", x)) +
    labs(x = xlab, y = "Toth type") +
    gld_theme +
    theme(legend.position = "none",
          panel.grid.major = element_line(color = "#E6E6E6", linewidth = 0.3),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 25, hjust = 1))
}

cloc_lev <- cf %>% count(Cortex_loc) %>% arrange(desc(n)) %>% pull(Cortex_loc)
cloc_tab <- cf %>%
  mutate(cat = factor(Cortex_loc, levels = cloc_lev)) %>%
  count(Toth, cat) %>% complete(Toth, cat, fill = list(n = 0))
p_cloc <- bubble_panel(cloc_tab, "Cortex location")

butt_lev <- c("Cortical", "Plain", "Linear", "Faceted", "Punctiform")
butt_tab <- cf %>%
  mutate(cat = factor(butt_type, levels = butt_lev)) %>%
  count(Toth, cat) %>% complete(Toth, cat, fill = list(n = 0))
p_butt <- bubble_panel(butt_tab, "Butt type")

dsp_lev <- cf %>%
  mutate(dp = ifelse(is.na(Dorsal_scar_pattern), "Indeterminate", Dorsal_scar_pattern)) %>%
  count(dp) %>% arrange(desc(n)) %>% pull(dp)
dsp_tab <- cf %>%
  mutate(cat = factor(ifelse(is.na(Dorsal_scar_pattern), "Indeterminate",
                             Dorsal_scar_pattern), levels = dsp_lev)) %>%
  count(Toth, cat) %>% complete(Toth, cat, fill = list(n = 0))
p_dsp <- bubble_panel(dsp_tab, "Dorsal scar pattern")

combo <- (p_elong | p_ipa) / (p_ndsc | p_cloc) / (p_butt | p_dsp) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 13))

ggsave(file.path(out_dir, "fig_flake_tech_attributes.png"), combo,
       width = 8, height = 10, dpi = 300)

cat("Toth type counts:\n"); print(table(cf$Toth))
cat("\nDone. fig_flake_tech_attributes.png written.\n")
