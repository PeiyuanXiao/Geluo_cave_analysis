# GLD Length/Width/Thickness/Mass by artifact type: 2x2 horizontal boxplots (log10 Mass).
# Types: Cores (Core sheet), Percussion flakes / Bipolar-on-anvil flakes
#        (Complete_flake split on tech_type), Retouched flakes (Tool sheet).

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(ggplot2)
})
data_file <- "data/GLD_lithic_data.xlsx"
out_dir   <- "output"
if (!dir.exists(out_dir)) dir.create(out_dir)
source("R/plot_style.R")

size_vars <- c("Length", "Width", "Thickness", "Mass")
read_sheet <- function(s) {
  df <- read_excel(data_file, sheet = s); names(df) <- trimws(names(df)); df
}
cf   <- read_sheet("Complete_flake")
core <- read_sheet("Core")
tool <- read_sheet("Tool")

dat <- bind_rows(
  core %>% transmute(Type = "Cores",                   across(all_of(size_vars), as.numeric)),
  cf   %>% filter(tech_type != "Bipolar_on_anvil_flake") %>%
           transmute(Type = "Percussion flakes",       across(all_of(size_vars), as.numeric)),
  cf   %>% filter(tech_type == "Bipolar_on_anvil_flake") %>%
           transmute(Type = "Bipolar-on-anvil flakes", across(all_of(size_vars), as.numeric)),
  tool %>% filter(Sub_type == "Retouched_flake") %>%
           transmute(Type = "Retouched flakes",        across(all_of(size_vars), as.numeric))
)

type_levels <- c("Cores", "Percussion flakes", "Bipolar-on-anvil flakes", "Retouched flakes")
type_colors <- c("Cores"                   = "#E6C25C",
                 "Percussion flakes"       = "#E07C90",
                 "Bipolar-on-anvil flakes" = "#6BA8CE",
                 "Retouched flakes"        = "#8FBF9F")

var_labs_log <- c(Length = "Length (mm)", Width = "Width (mm)",
                  Thickness = "Thickness (mm)", Mass = "Mass (g, log10)")

# horizontal layout: Value on x (free per facet), Type on y (Cores at top)
base_boxplot <- function(d, labeller_vec) {
  ggplot(d, aes(Value, Type)) +
    geom_jitter(aes(color = Type), width = 0, height = 0.28,
                size = 1.3, alpha = 0.55, shape = 16) +
    geom_boxplot(color = "black", fill = NA, width = 0.6,
                 linewidth = 0.55, outlier.shape = NA) +
    stat_summary(fun = mean, geom = "point", shape = 16, size = 2, color = "black") +
    facet_wrap(~ Variable, scales = "free_x", ncol = 2,
               labeller = as_labeller(labeller_vec)) +
    scale_color_manual(values = type_colors) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.08))) +
    scale_y_discrete(limits = rev(type_levels)) +
    labs(x = NULL, y = NULL) +
    gld_theme +
    theme(panel.grid.major.y = element_blank(),
          legend.position = "none")
}

dat_long <- dat %>%
  pivot_longer(all_of(size_vars), names_to = "Variable", values_to = "Value") %>%
  mutate(Type     = factor(Type, levels = type_levels),
         Variable = factor(Variable, levels = size_vars))

# log10 Mass: a single large core dwarfs flakes on a linear Mass axis
dat_log <- dat_long %>% mutate(Value = ifelse(Variable == "Mass", log10(Value), Value))
ggsave(file.path(out_dir, "fig_size_by_type_logmass.png"),
       base_boxplot(dat_log, var_labs_log), width = 8.0, height = 6.6, dpi = 300)

cat("group sizes:\n"); print(table(dat$Type))
cat("\nDone. fig_size_by_type_logmass.png written.\n")
