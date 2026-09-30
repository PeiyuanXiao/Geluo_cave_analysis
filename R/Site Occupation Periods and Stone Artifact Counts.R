# Load ggplot2 package
library(ggplot2)

# Prepare data (time unit: ka BP)
site_data <- data.frame(
  site = c("Geluo (GL)", "Xiajisha (XJS)", "Yushuiping (YSP)", "Laohu (LH)", 
           "Fodongdi (FD)", "Dedan (DD)", "Xiaodong (XD)"),
  start = c(34, 43, 30, 30, 18, 19.8, 43),
  end   = c(11, 11, 15, 18, 14, 18.2, 25),
  quantity = c(336, 4056, 898, 65, 9735, 591, 5000),
  technocomplex = c(
    "Expedient core-flake technocomplex",
    "Large core-tools technocomplex",
    "Expedient core-flake technocomplex",
    "Expedient core-flake technocomplex",
    "Minialithic technocomplex",
    "Minialithic technocomplex",
    "Minialithic technocomplex"
  )
)

# Order sites by start date (oldest at top) and assign numeric y positions
site_order <- site_data$site[order(-site_data$start)]
site_data$site <- factor(site_data$site, levels = site_order)
site_data$y <- as.numeric(site_data$site)

# Number of sites
n_sites <- length(levels(site_data$site))

# Colors for technocomplexes
techno_colors <- c(
  "Expedient core-flake technocomplex" = "red",
  "Minialithic technocomplex"           = "skyblue",
  "Large core-tools technocomplex"      = "orchid"
)

# Define time periods as background bands (span the full plot height)
periods <- data.frame(
  label = c("Late MIS3", "Early MIS2", "LGM", "LD", "Early MIS1"),
  xmin  = c(29, 26, 19, 11.7, 10),
  xmax  = c(43, 29, 26, 19, 11.7),
  ymin  = 0.3,
  ymax  = n_sites + 0.3,
  fill  = c("orange", "lightblue", "lightblue", "lightcoral", "lightgrey")
)

# Plot
ggplot(site_data) +
  # Background time-period bands
  geom_rect(data = periods,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = label),
            alpha = 0.15, show.legend = FALSE) +
  # Draw segments with open arrowheads
  geom_segment(aes(x = start, xend = end,
                   y = y, yend = y,
                   size = quantity,
                   color = technocomplex),
               lineend = "round",
               arrow = arrow(length = unit(0.25, "cm"), 
                             type = "open",
                             angle = 20)) +
  # Line width scale
  scale_size_continuous(name = "Number of stone artifacts",
                        range = c(0.3, 1.8),
                        breaks = c(100, 500, 1000, 5000, 10000)) +
  # Color mapping for technocomplexes
  scale_color_manual(name = "Technocomplex",
                     values = techno_colors) +
  # Fill colors for time-period bands
  scale_fill_manual(values = setNames(periods$fill, periods$label)) +
  # Reverse x-axis
  scale_x_reverse(breaks = seq(10, 45, by = 5),
                  expand = expansion(mult = c(0.02, 0.02))) +
  # Numeric y-axis with site names
  scale_y_continuous(breaks = 1:n_sites,
                     labels = levels(site_data$site),
                     expand = expansion(add = c(0.2, 0.2))) +
  # Axis labels
  labs(x = "Time (ka BP)", 
       y = "Site") +
  # Theme settings
  theme_minimal(base_size = 12, base_family = "Arial") +
  theme(
    axis.text.y = element_text(size = 11),
    panel.grid = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    # ---- x-axis line and downward ticks ----
    axis.line.x = element_line(color = "black", linewidth = 0.5), 
    axis.ticks.x = element_line(color = "black", linewidth = 0.5), 
    axis.ticks.length.x = unit(0.15, "cm"),                        
    # ---- legend settings ----
    legend.position = "bottom",
    legend.box = "vertical",
    legend.direction = "horizontal",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "cm"),
    legend.margin = margin(t = 2, b = 2),
    legend.spacing.y = unit(0.1, "cm"),
    legend.box.spacing = unit(0.2, "cm"),
    plot.margin = margin(t = 5, r = 10, b = 15, l = 5, unit = "pt")
  ) +
  guides(
    size  = guide_legend(title.position = "top", order = 1, nrow = 1),
    color = guide_legend(title.position = "top", order = 2, nrow = 1,
                         override.aes = list(size = 1.2))
  )
