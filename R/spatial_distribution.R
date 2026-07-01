# =============================================================================
# Geluo Cave (GLD) — spatial distribution of piece-plotted finds
# Plan view (X-Y) and stratigraphic profile (X-Z) of point-provenienced lithics.
# Style mirrors the SKG spatial plots: shape-21 points filled by unit, grey
# outline, integer-metre grid, coord_fixed (true 1:1 scale), theme_bw.
#
# Coordinate note (from the source sheet): x = 横向 (transverse),
#   y = 纵向 (longitudinal), z = 高程 (elevation, m a.s.l.).
# The coord sheet has no cultural-layer field, and its 2024 IDs (24GLD_1_1)
# do NOT match the 2022 lithic-attribute IDs (22GLD-2-3), so the finds cannot
# be tagged by Layer here — points are grouped by Trench instead (the spatial
# excavation unit), which is the direct analogue of the reference's Unit map.
# =============================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(ggplot2); library(patchwork)
})

data_file <- "data/GLD_lithic_coord.xlsx"
out_dir   <- "output"
if (!dir.exists(out_dir)) dir.create(out_dir)

# ---- data -------------------------------------------------------------------
coord <- read_excel(data_file, sheet = 1)
names(coord) <- trimws(names(coord))

coord <- coord %>%
  transmute(
    ID     = as.character(ID),
    Trench = factor(trimws(as.character(Trench))),
    X      = as.numeric(x),          # 横向 (transverse)
    Y      = as.numeric(y),          # 纵向 (longitudinal)
    Z      = as.numeric(z),          # 高程 (elevation, m a.s.l.)
    Type   = trimws(as.character(Type))
  ) %>%
  filter(!is.na(ID), !is.na(X), !is.na(Y), !is.na(Z)) %>%
  arrange(Trench)                    # deterministic draw order

# focus on stone artifacts (drops the 21 fossils); to include the fossils too,
# comment out this line and map `shape = Type` in the plot functions below.
artifacts <- coord %>% filter(Type == "Stone_artifact")

# ---- shared style -----------------------------------------------------------
# soft fills echoing the reference spatial palette; grey outline keeps them crisp
trench_colors <- c("T1" = "#FFC9C9", "T2" = "#B8E6FE", "T3" = "#D8F5A2")

# plan view: bird's-eye X-Y ----------------------------------------------------
plot_plan <- function(df, title = NULL) {
  xr <- range(df$X, na.rm = TRUE)
  yr <- range(df$Y, na.rm = TRUE)
  ggplot(df, aes(X, Y, fill = Trench)) +
    geom_point(shape = 21, size = 2.5, color = "gray30", stroke = 0.5) +
    scale_fill_manual(values = trench_colors) +
    scale_x_continuous(breaks = seq(floor(xr[1]), ceiling(xr[2]), by = 1)) +
    scale_y_continuous(breaks = seq(floor(yr[1]), ceiling(yr[2]), by = 1)) +
    coord_fixed() +
    theme_bw() +
    labs(title = title, x = "X (m)", y = "Y (m)", fill = "Trench") +
    theme(panel.grid.minor = element_blank(), legend.position = "right")
}

# stratigraphic profile: horizontal axis vs Z (elevation) ----------------------
# horiz = "X" gives the transverse section; switch to "Y" for the longitudinal.
plot_profile <- function(df, horiz = "X", title = NULL) {
  hr <- range(df[[horiz]], na.rm = TRUE)
  zr <- range(df$Z, na.rm = TRUE)
  ggplot(df, aes(.data[[horiz]], Z, fill = Trench)) +
    geom_point(shape = 21, size = 2.5, color = "gray30", stroke = 0.5) +
    scale_fill_manual(values = trench_colors) +
    scale_x_continuous(breaks = seq(floor(hr[1]), ceiling(hr[2]), by = 1)) +
    scale_y_continuous(breaks = seq(floor(zr[1]), ceiling(zr[2]), by = 0.5)) +
    coord_fixed() +
    theme_bw() +
    labs(title = title, x = paste0(horiz, " (m)"),
         y = "Elevation (m a.s.l.)", fill = "Trench") +
    theme(panel.grid.minor = element_blank(), legend.position = "right")
}

# ---- render -----------------------------------------------------------------
p_plan    <- plot_plan(artifacts,    title = "GLD - Plan view")
p_profile <- plot_profile(artifacts, horiz = "X", title = "GLD - Profile (X-Z)")

ggsave(file.path(out_dir, "fig_spatial_plan.png"), p_plan,
       width = 8, height = 7, dpi = 300, bg = "white")
ggsave(file.path(out_dir, "fig_spatial_profile.png"), p_profile,
       width = 10, height = 5, dpi = 300, bg = "white")

# combined: plan (a) above, profile (b) below, uniform a/b tags
p_spatial_combined <- (p_plan / p_profile) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 13, color = "#202124"))
ggsave(file.path(out_dir, "fig_spatial_combined.png"), p_spatial_combined,
       width = 9, height = 11, dpi = 300, bg = "white")

cat("\nDone. See output/fig_spatial_{plan,profile,combined}.png\n")
