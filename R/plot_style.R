# =============================================================================
# Shared plotting house-style for the GLD analyses.
# Aesthetic ported from the Quina-valleys reference script:
#   minimal theme + boxed panel, centroid/hull/spoke ordinations,
#   sign-coloured loading bars, jittered boxplots.
# Source this AFTER loading dplyr, tidyr, ggplot2.
# =============================================================================

# --- palette (Layer 2 / Layer 3) ---------------------------------------------
gld_colors <- c("Layer 2" = "#E07C90", "Layer 3" = "#6BA8CE")
gld_sign   <- c(Positive = "#6BA8CE", Negative = "#E07C90")
# soft qualitative palette for multi-category composition bars
gld_qual <- c("#E07C90", "#6BA8CE", "#E6C25C", "#8FBF9F", "#B79BC9",
              "#D9A07A", "#7FBDBD", "#A7C7E7", "#C9C9C9", "#D7B8A3")

# --- theme -------------------------------------------------------------------
gld_theme <- theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_line(color = "#E6E8EB", linewidth = 0.35),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "#202124", fill = NA, linewidth = 0.65),
    axis.ticks = element_line(color = "#202124", linewidth = 0.35),
    axis.ticks.length = grid::unit(2.5, "pt"),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "#303238"),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.key = element_blank(),
    strip.text = element_text(face = "bold", color = "#202124"),
    strip.background = element_rect(fill = "#E8E8E8", color = NA),
    plot.background = element_rect(color = NA, fill = "white"),
    panel.background = element_rect(color = NA, fill = "white")
  )

# --- ordination: hulls + spokes-to-centroid + centroid markers ---------------
# `scores` must have columns: Dim1, Dim2, Group
gld_ordination <- function(scores, xlab, ylab, title = NULL, subtitle = NULL,
                           colors = gld_colors, legend_title = "Layer") {
  cent <- scores |>
    dplyr::group_by(Group) |>
    dplyr::summarise(xc = mean(Dim1), yc = mean(Dim2), .groups = "drop")
  spokes <- dplyr::left_join(scores, cent, by = "Group")
  hulls <- scores |>
    dplyr::group_by(Group) |>
    dplyr::filter(dplyr::n() >= 3) |>
    dplyr::group_modify(function(g, key) {
      h <- chull(g$Dim1, g$Dim2)
      dplyr::bind_rows(g[h, ], g[h[1], ])
    }) |>
    dplyr::ungroup()

  ggplot(scores, aes(Dim1, Dim2, color = Group)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.4, linetype = "dashed") +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.4, linetype = "dashed") +
    geom_polygon(data = hulls, aes(Dim1, Dim2, fill = Group, group = Group),
                 alpha = 0.12, color = NA, inherit.aes = FALSE) +
    geom_path(data = hulls, aes(Dim1, Dim2, color = Group, group = Group),
              linewidth = 0.45, alpha = 0.65, inherit.aes = FALSE) +
    geom_segment(data = spokes,
                 aes(Dim1, Dim2, xend = xc, yend = yc, color = Group),
                 linewidth = 0.25, alpha = 0.35, inherit.aes = FALSE) +
    geom_point(size = 1.85, alpha = 0.8, shape = 16) +
    geom_point(data = cent, aes(xc, yc, color = Group),
               shape = 21, fill = "white", size = 4, stroke = 1.1, inherit.aes = FALSE) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    scale_x_continuous(expand = expansion(mult = 0.1)) +
    scale_y_continuous(expand = expansion(mult = 0.1)) +
    labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
         color = legend_title, fill = legend_title) +
    coord_equal() + gld_theme +
    guides(fill = "none",
           color = guide_legend(override.aes = list(size = 3.2, alpha = 1, shape = 16)))
}

# --- variable loadings (sign-coloured horizontal bars, faceted by Dim) -------
# `coord` must have columns: Variable, Dim1, Dim2
gld_loadings <- function(coord, var_levels, subtitle = "Variable coordinates") {
  long <- coord |>
    tidyr::pivot_longer(c(Dim1, Dim2), names_to = "Dim", values_to = "Loading") |>
    dplyr::mutate(
      Dim = factor(Dim, levels = c("Dim1", "Dim2"),
                   labels = c("Dim.1", "Dim.2")),
      Variable = factor(Variable, levels = rev(var_levels)),
      Sign = ifelse(Loading >= 0, "Positive", "Negative")
    )
  ggplot(long, aes(Loading, Variable, fill = Sign)) +
    geom_col(width = 0.7, color = "#303238", linewidth = 0.3) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
    facet_wrap(~ Dim) +
    scale_fill_manual(values = gld_sign) +
    scale_y_discrete(labels = function(x) gsub("_", " ", x)) +
    labs(subtitle = subtitle, x = "Coordinate on dimension", y = NULL, fill = NULL) +
    gld_theme +
    theme(panel.grid.major.y = element_blank(), legend.position = "top")
}

# --- variable contribution bars (to Dim.1 + Dim.2) ---------------------------
# `df` must have columns: Variable, contrib
gld_contrib <- function(df, subtitle = "Variable contribution to Dim.1 + Dim.2 (%)") {
  df <- dplyr::mutate(df, Variable = reorder(Variable, contrib))
  ggplot(df, aes(contrib, Variable)) +
    geom_col(width = 0.7, fill = "#6BA8CE", color = "#303238", linewidth = 0.3) +
    scale_y_discrete(labels = function(x) gsub("_", " ", x)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(subtitle = subtitle, x = "Contribution (%)", y = NULL) +
    gld_theme +
    theme(panel.grid.major.y = element_blank())
}

# --- scree (variance explained) ----------------------------------------------
gld_scree <- function(eig, ndim = 8, subtitle = "Variance explained") {
  k <- min(ndim, nrow(eig))
  d <- data.frame(
    Dim = factor(paste0("Dim", seq_len(k)), levels = paste0("Dim", seq_len(k))),
    Var = eig[seq_len(k), 2]
  )
  ggplot(d, aes(Dim, Var)) +
    geom_col(width = 0.7, fill = "#6BA8CE", color = "#303238", linewidth = 0.3) +
    geom_text(aes(label = sprintf("%.1f", Var)), vjust = -0.4, size = 3, color = "#303238") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(subtitle = subtitle, x = NULL, y = "% variance") +
    gld_theme +
    theme(panel.grid.major.x = element_blank())
}
