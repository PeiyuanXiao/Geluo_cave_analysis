# GLD spatial distribution of piece-plotted finds: plan view (X-Y) + profile (X-Z).
# x = transverse, y = longitudinal, z = elevation (m a.s.l.). The coord sheet has no
# layer field and its IDs don't match the attribute data, so points are grouped by
# Trench (the spatial excavation unit), not Layer.

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(ggplot2); library(patchwork)
})

data_file <- "data/GLD_lithic_coord.xlsx"
out_dir   <- "output"
if (!dir.exists(out_dir)) dir.create(out_dir)

coord <- read_excel(data_file, sheet = 1)
names(coord) <- trimws(names(coord))

coord <- coord %>%
  transmute(
    ID     = as.character(ID),
    Trench = factor(trimws(as.character(Trench))),
    X      = as.numeric(x),
    Y      = as.numeric(y),
    Z      = as.numeric(z),
    Type   = trimws(as.character(Type))
  ) %>%
  filter(!is.na(ID), !is.na(X), !is.na(Y), !is.na(Z)) %>%
  arrange(Trench)

# stone artifacts only (drops 21 fossils); map shape = Type to include them
artifacts <- coord %>% filter(Type == "Stone_artifact")

trench_colors <- c("T1" = "#FFC9C9", "T2" = "#B8E6FE", "T3" = "#D8F5A2")

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

# horiz = "X" gives the transverse section; "Y" for the longitudinal
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

p_plan    <- plot_plan(artifacts,    title = "GLD - Plan view")
p_profile <- plot_profile(artifacts, horiz = "X", title = "GLD - Profile (X-Z)")

ggsave(file.path(out_dir, "fig_spatial_plan.png"), p_plan,
       width = 8, height = 7, dpi = 300, bg = "white")
ggsave(file.path(out_dir, "fig_spatial_profile.png"), p_profile,
       width = 10, height = 5, dpi = 300, bg = "white")

p_spatial_combined <- (p_plan / p_profile) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 13, color = "#202124"))
ggsave(file.path(out_dir, "fig_spatial_combined.png"), p_spatial_combined,
       width = 9, height = 11, dpi = 300, bg = "white")

cat("\nDone. See output/fig_spatial_{plan,profile,combined}.png\n")
