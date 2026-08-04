library(ggplot2)
library(dplyr)
library(readr)
library(metR)
library(terra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
extract_variable <- function(target_idx, var_name, period_label) {
  
  data_folder <- "D:/Rdate/Geluo_R_project/pastclim_data"
  monthly_files <- list.files(data_folder, pattern = "monthly", full.names = TRUE, ignore.case = TRUE)
  monthly_data <- rast(monthly_files[1])
  all_names <- names(monthly_data)
  
  expanded_extent <- ext(95, 108, 19, 31)
  
  if (var_name == "ET") {
    temp_layers <- paste0("temperature_", sprintf("%02d", 1:12), "_", target_idx)
    temp_exist <- temp_layers[temp_layers %in% all_names]
    if (length(temp_exist) != 12) return(NULL)
    
    layer_positions <- sapply(temp_exist, function(ln) which(all_names == ln))
    temp_stack <- monthly_data[[layer_positions]]
    temp_stack <- crop(temp_stack, expanded_extent)
    
    MWM <- max(temp_stack, na.rm = TRUE)
    MCM <- min(temp_stack, na.rm = TRUE)
    result_raster <- ((18 * MWM) - (10 * MCM)) / (MWM - MCM + 8)
    
  } else if (var_name == "WT") {
    winter_months <- c(12, 1, 2)
    winter_layers <- paste0("temperature_", sprintf("%02d", winter_months), "_", target_idx)
    winter_exist <- winter_layers[winter_layers %in% all_names]
    if (length(winter_exist) != 3) return(NULL)
    
    layer_positions <- sapply(winter_exist, function(ln) which(all_names == ln))
    winter_stack <- monthly_data[[layer_positions]]
    winter_stack <- crop(winter_stack, expanded_extent)
    result_raster <- app(winter_stack, fun = mean, na.rm = TRUE)
    
  } else if (var_name == "SR") {
    summer_months <- c(6, 7, 8)
    summer_layers <- paste0("precipitation_", sprintf("%02d", summer_months), "_", target_idx)
    summer_exist <- summer_layers[summer_layers %in% all_names]
    if (length(summer_exist) != 3) return(NULL)
    
    layer_positions <- sapply(summer_exist, function(ln) which(all_names == ln))
    summer_stack <- monthly_data[[layer_positions]]
    summer_stack <- crop(summer_stack, expanded_extent)
    result_raster <- app(summer_stack, fun = sum, na.rm = TRUE)
    
  } else if (var_name == "NPP") {
    npp_layers <- paste0("mo_npp_", sprintf("%02d", 1:12), "_", target_idx)
    npp_exist <- npp_layers[npp_layers %in% all_names]
    if (length(npp_exist) != 12) return(NULL)
    
    layer_positions <- sapply(npp_exist, function(ln) which(all_names == ln))
    npp_stack <- monthly_data[[layer_positions]]
    npp_stack <- crop(npp_stack, expanded_extent)
    result_raster <- app(npp_stack, fun = sum, na.rm = TRUE)
  }
  
  df <- as.data.frame(result_raster, xy = TRUE, na.rm = FALSE)
  colnames(df) <- c("x", "y", "value")
  df$period <- period_label
  df$var_name <- var_name
  
  print(paste("✅", period_label, var_name, "计算完成, 行数:", nrow(df)))
  return(df)
}

targets <- c(43, 50, 58)
periods <- c("34 ka", "20 ka", "12 ka")

ET_list <- list()
WT_list <- list()
SR_list <- list()
NPP_list <- list()

for (i in 1:3) {
  ET_list[[i]] <- extract_variable(targets[i], "ET", periods[i])
  WT_list[[i]] <- extract_variable(targets[i], "WT", periods[i])
  SR_list[[i]] <- extract_variable(targets[i], "SR", periods[i])
  NPP_list[[i]] <- extract_variable(targets[i], "NPP", periods[i])
}
ET_all <- bind_rows(ET_list)
colnames(ET_all)[3] <- "ET"

WT_all <- bind_rows(WT_list)
colnames(WT_all)[3] <- "Winter_Temp"

SR_all <- bind_rows(SR_list)
colnames(SR_all)[3] <- "Summer_Rain"

NPP_all <- bind_rows(NPP_list)
colnames(NPP_all)[3] <- "NPP"

rivers <- ne_download(scale = 10, type = "rivers_lake_centerlines", 
                      category = "physical", returnclass = "sf")

study_extent <- st_bbox(c(xmin = 95, xmax = 108, ymin = 19, ymax = 31), 
                        crs = st_crs(rivers))
rivers_cropped <- st_crop(rivers, study_extent)

rivers_df_list <- list()
for (i in 1:nrow(rivers_cropped)) {
  geom <- st_geometry(rivers_cropped[i, ])
  tryCatch({
    coords <- st_coordinates(geom)
    if (nrow(coords) > 0) {
      rivers_df_list[[length(rivers_df_list) + 1]] <- data.frame(
        lon = coords[, 1], lat = coords[, 2], group = i
      )
    }
  }, error = function(e) {})
}

if (length(rivers_df_list) > 0) {
  rivers_df <- bind_rows(rivers_df_list)
} else {
  rivers_df <- data.frame(
    lon = c(seq(100.0, 102.2, length.out = 25), seq(99.0, 100.6, length.out = 20),
            seq(98.5, 99.4, length.out = 18), seq(102.3, 103.5, length.out = 15)),
    lat = c(seq(28.5, 24.5, length.out = 25), seq(28.5, 25.5, length.out = 20),
            seq(28.5, 26.0, length.out = 18), seq(25.5, 23.5, length.out = 15)),
    group = c(rep(1, 25), rep(2, 20), rep(3, 18), rep(4, 15))
  )
}


sites <- read_csv("D:/Rdate/Geluo_R_project/date_sites/site_date_clean.csv", show_col_types = FALSE)
kunming <- data.frame(site_name = "Kunming", longitude = 102.7317, latitude = 25.0433)

geluo <- sites %>% filter(site_name == "Geluo")
circle_sites <- sites %>% filter(site_name %in% c("Yushuiping", "Laohu", "Fodongdi"))
square_sites <- sites %>% filter(site_name == "Xiajisha")
triangle_sites <- sites %>% filter(site_name %in% c("Xiaodong", "Tangbula", "Dedan"))


et_colors <- c("#313695", "#4575b4", "#74add1", "#abd9e9", 
               "#ffffbf", "#fee090", "#fdae61", "#f46d43", "#d73027")

wt_colors <- c("#313695", "#4575b4", "#74add1", "#abd9e9", 
               "#ffffbf", "#fee090", "#fdae61", "#f46d43", "#d73027")

sr_colors <- c("#ffffcc", "#c7e9b4", "#7fcdbb", "#41b6c4", 
               "#2c7fb8", "#253494", "#081d58")

npp_colors <- c("#ffffe5", "#f7fcb9", "#d9f0a3", "#addd8e",
                "#78c679", "#41ab5d", "#238443", "#006837")

ET_min <- floor(min(ET_all$ET, na.rm = TRUE))
ET_max <- ceiling(max(ET_all$ET, na.rm = TRUE))
ET_breaks <- seq(ET_min, ET_max, by = 1)

WT_min <- floor(min(WT_all$Winter_Temp, na.rm = TRUE))
WT_max <- ceiling(max(WT_all$Winter_Temp, na.rm = TRUE))
WT_breaks <- seq(WT_min, WT_max, by = 2)

SR_min <- floor(min(SR_all$Summer_Rain, na.rm = TRUE) / 100) * 100
SR_max <- ceiling(max(SR_all$Summer_Rain, na.rm = TRUE) / 100) * 100
SR_breaks <- seq(SR_min, SR_max, by = 100)

NPP_min <- floor(min(NPP_all$NPP, na.rm = TRUE) / 200) * 200
NPP_max <- ceiling(max(NPP_all$NPP, na.rm = TRUE) / 200) * 200
NPP_breaks <- seq(NPP_min, NPP_max, by = 200)

plot_variable <- function(data, value_col, colors, breaks, limits, 
                          title_text, rivers_df,
                          x_min=97, x_max=106, y_min=21, y_max=29) {
  
  legend_height <- (y_max - y_min) * 1.0
  contour_size <- 0.45
  
  p <- ggplot() +
    geom_raster(data = data, aes(x = x, y = y, fill = .data[[value_col]]), interpolate = TRUE) +
    scale_fill_gradientn(
      colors = colors,
      limits = limits,
      name = NULL,
      na.value = "transparent",
      guide = guide_colourbar(barheight = unit(legend_height, "cm"),
                              barwidth = unit(0.35, "cm"))
    ) +
    geom_path(data = rivers_df, 
              aes(x = lon, y = lat, group = group),
              color = "steelblue", size = 0.25, alpha = 0.6) +
    stat_contour(data = data, aes(x = x, y = y, z = .data[[value_col]]),
                 breaks = breaks,
                 color = "black", size = contour_size, alpha = 0.7) +
    geom_text_contour(
      data = data, aes(x = x, y = y, z = .data[[value_col]]),
      breaks = breaks,
      color = "black", size = 3.5, fontface = "bold",
      family = "Times New Roman",
      label.placer = label_placer_fraction(frac = 0.5)
    ) +
    geom_point(data = geluo, aes(x = longitude, y = latitude), 
               color = "red", size = 2.5, shape = 16) +
    geom_point(data = circle_sites, aes(x = longitude, y = latitude), 
               color = "black", size = 2.5, shape = 16) +
    geom_point(data = square_sites, aes(x = longitude, y = latitude), 
               color = "black", size = 2.5, shape = 15) +
    geom_point(data = triangle_sites, aes(x = longitude, y = latitude), 
               color = "black", size = 2.5, shape = 17) +
    geom_point(data = kunming, aes(x = longitude, y = latitude), 
               color = "black", size = 4.0, shape = 16) +
    geom_point(data = kunming, aes(x = longitude, y = latitude), 
               color = "white", size = 3.0, shape = 16) +
    geom_point(data = kunming, aes(x = longitude, y = latitude), 
               color = "black", size = 2.0, shape = 16) +
    coord_fixed(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE) +
    scale_x_continuous(breaks = seq(97, 106, by = 2.5),
                       labels = function(x) gsub("°E", "", as.character(x))) +
    scale_y_continuous(breaks = seq(21, 29, by = 2.5),
                       labels = function(y) gsub("°N", "", as.character(y))) +
    labs(title = title_text, x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      text = element_text(family = "Times New Roman"),
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold", family = "Times New Roman"),
      axis.title = element_blank(),
      axis.text = element_text(size = 11, color = "black", face = "bold", family = "Times New Roman"),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.ticks = element_line(color = "black", linewidth = 0.6),
      axis.ticks.length = unit(0.2, "cm"),
      legend.key.height = unit(legend_height, "cm"),
      legend.key.width = unit(0.35, "cm"),
      legend.title = element_blank(),
      legend.text = element_text(size = 10, family = "Times New Roman"),
      legend.position = "right"
    )
  
  return(p)
}

period_names <- c("34ka", "20ka", "12ka")

ET_34 <- ET_all %>% filter(period == "34 ka")
ET_20 <- ET_all %>% filter(period == "20 ka")
ET_12 <- ET_all %>% filter(period == "12 ka")

WT_34 <- WT_all %>% filter(period == "34 ka")
WT_20 <- WT_all %>% filter(period == "20 ka")
WT_12 <- WT_all %>% filter(period == "12 ka")

SR_34 <- SR_all %>% filter(period == "34 ka")
SR_20 <- SR_all %>% filter(period == "20 ka")
SR_12 <- SR_all %>% filter(period == "12 ka")

NPP_34 <- NPP_all %>% filter(period == "34 ka")
NPP_20 <- NPP_all %>% filter(period == "20 ka")
NPP_12 <- NPP_all %>% filter(period == "12 ka")

# ET
p_et_34 <- plot_variable(ET_34, "ET", et_colors, ET_breaks, c(ET_min, ET_max), "Effective Temperature", rivers_df)
p_et_20 <- plot_variable(ET_20, "ET", et_colors, ET_breaks, c(ET_min, ET_max), "Effective Temperature", rivers_df)
p_et_12 <- plot_variable(ET_12, "ET", et_colors, ET_breaks, c(ET_min, ET_max), "Effective Temperature", rivers_df)

ggsave("D:/Rdate/Geluo_R_project/ET_34ka.png", plot = p_et_34, width = 14, height = 12, units = "cm", dpi = 400)
ggsave("D:/Rdate/Geluo_R_project/ET_20ka.png", plot = p_et_20, width = 14, height = 12, units = "cm", dpi = 400)
ggsave("D:/Rdate/Geluo_R_project/ET_12ka.png", plot = p_et_12, width = 14, height = 12, units = "cm", dpi = 400)

# WT
p_wt_34 <- plot_variable(WT_34, "Winter_Temp", wt_colors, WT_breaks, c(WT_min, WT_max), "Winter Temperature", rivers_df)
p_wt_20 <- plot_variable(WT_20, "Winter_Temp", wt_colors, WT_breaks, c(WT_min, WT_max), "Winter Temperature", rivers_df)
p_wt_12 <- plot_variable(WT_12, "Winter_Temp", wt_colors, WT_breaks, c(WT_min, WT_max), "Winter Temperature", rivers_df)

ggsave("D:/Rdate/Geluo_R_project/WinterTemp_34ka.png", plot = p_wt_34, width = 14, height = 12, units = "cm", dpi = 400)
ggsave("D:/Rdate/Geluo_R_project/WinterTemp_20ka.png", plot = p_wt_20, width = 14, height = 12, units = "cm", dpi = 400)
ggsave("D:/Rdate/Geluo_R_project/WinterTemp_12ka.png", plot = p_wt_12, width = 14, height = 12, units = "cm", dpi = 400)

# SR
p_sr_34 <- plot_variable(SR_34, "Summer_Rain", sr_colors, SR_breaks, c(SR_min, SR_max), "Summer Rainfall", rivers_df)
p_sr_20 <- plot_variable(SR_20, "Summer_Rain", sr_colors, SR_breaks, c(SR_min, SR_max), "Summer Rainfall", rivers_df)
p_sr_12 <- plot_variable(SR_12, "Summer_Rain", sr_colors, SR_breaks, c(SR_min, SR_max), "Summer Rainfall", rivers_df)

ggsave("D:/Rdate/Geluo_R_project/SummerRain_34ka.png", plot = p_sr_34, width = 14, height = 12, units = "cm", dpi = 400)
ggsave("D:/Rdate/Geluo_R_project/SummerRain_20ka.png", plot = p_sr_20, width = 14, height = 12, units = "cm", dpi = 400)
ggsave("D:/Rdate/Geluo_R_project/SummerRain_12ka.png", plot = p_sr_12, width = 14, height = 12, units = "cm", dpi = 400)

# NPP
p_npp_34 <- plot_variable(NPP_34, "NPP", npp_colors, NPP_breaks, c(NPP_min, NPP_max), "Net Primary Productivity", rivers_df)
p_npp_20 <- plot_variable(NPP_20, "NPP", npp_colors, NPP_breaks, c(NPP_min, NPP_max), "Net Primary Productivity", rivers_df)
p_npp_12 <- plot_variable(NPP_12, "NPP", npp_colors, NPP_breaks, c(NPP_min, NPP_max), "Net Primary Productivity", rivers_df)

ggsave("D:/Rdate/Geluo_R_project/NPP_34ka.png", plot = p_npp_34, width = 14, height = 12, units = "cm", dpi = 400)
ggsave("D:/Rdate/Geluo_R_project/NPP_20ka.png", plot = p_npp_20, width = 14, height = 12, units = "cm", dpi = 400)
ggsave("D:/Rdate/Geluo_R_project/NPP_12ka.png", plot = p_npp_12, width = 14, height = 12, units = "cm", dpi = 400)