# =============================================================================
# Geluo Cave (GLD) — Complete-flake technological attributes (composite figure)
# Descriptive distributions over all complete flakes (n = 118). Chart type chosen
# per variable nature:
#   Cortex (0-1 proportion)      -> histogram
#   Elongation (L/W, continuous) -> histogram
#   IPA (continuous, 17 NA)      -> histogram (n = 101; NA = linear/punctiform)
#   n_dors_scar (count 0-6)      -> bar
#   butt_type (5 categories)     -> bar, ordered
#   Dorsal_scar_pattern (9 cats) -> horizontal bar, ordered (NA -> Indeterminate)
# Composed with patchwork (3 x 2), tags a-f. No titles/subtitles (house rule).
# =============================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(ggplot2); library(patchwork)
})
data_file <- "data/GLD_lithic_data.xlsx"
out_dir   <- "output"
if (!dir.exists(out_dir)) dir.create(out_dir)
source("R/plot_style.R")

cf <- read_excel(data_file, sheet = "Complete_flake"); names(cf) <- trimws(names(cf))
fill_col <- "#6BA8CE"

hist_panel <- function(aes_x, xlab, binwidth, boundary, data = cf) {
  ggplot(data, aes({{ aes_x }})) +
    geom_histogram(binwidth = binwidth, boundary = boundary,
                   fill = fill_col, color = "#303238", linewidth = 0.25) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(x = xlab, y = "Count") + gld_theme
}

# a. Cortex proportion
p_cortex <- hist_panel(Cortex, "Cortex proportion", binwidth = 0.1, boundary = 0)

# b. Elongation
p_elong <- hist_panel(Elongation, "Elongation (L/W)", binwidth = 0.15, boundary = 0)

# c. IPA (drop structural NA)
p_ipa <- hist_panel(IPA, "IPA (degrees)", binwidth = 5, boundary = 80,
                    data = filter(cf, !is.na(IPA)))

# d. dorsal scar count (discrete)
p_ndsc <- ggplot(cf, aes(factor(n_dors_scar))) +
  geom_bar(fill = fill_col, color = "#303238", linewidth = 0.25, width = 0.8) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Dorsal scar count", y = "Count") +
  gld_theme + theme(panel.grid.major.x = element_blank())

# e. butt type (ordered)
bt <- cf %>% count(butt_type) %>% mutate(butt_type = reorder(butt_type, -n))
p_butt <- ggplot(bt, aes(butt_type, n)) +
  geom_col(fill = fill_col, color = "#303238", linewidth = 0.25, width = 0.75) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Butt type", y = "Count") +
  gld_theme + theme(panel.grid.major.x = element_blank(),
                    axis.text.x = element_text(angle = 20, hjust = 1))

# f. dorsal scar pattern (horizontal, ordered; NA -> Indeterminate)
dsp <- cf %>%
  mutate(dp = ifelse(is.na(Dorsal_scar_pattern), "Indeterminate", Dorsal_scar_pattern)) %>%
  count(dp) %>% mutate(dp = reorder(dp, n))
p_dsp <- ggplot(dsp, aes(n, dp)) +
  geom_col(fill = fill_col, color = "#303238", linewidth = 0.25, width = 0.75) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
  scale_y_discrete(labels = function(x) gsub("_", " ", x)) +
  labs(x = "Count", y = "Dorsal scar pattern") +
  gld_theme + theme(panel.grid.major.y = element_blank())

combo <- (p_cortex | p_elong) / (p_ipa | p_ndsc) / (p_butt | p_dsp) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 13))

ggsave(file.path(out_dir, "fig_flake_tech_attributes.png"), combo,
       width = 9.5, height = 9.5, dpi = 300)

cat("Done. fig_flake_tech_attributes.png written (n =", nrow(cf), "complete flakes).\n")
