# ============================================
# Watlas Animation Generator
# ============================================

# installation of the simplified branch
# devtools::install_github("leonawicz/mapmate")

# 📦 Load all necessary packages
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(ggplot2)
  library(viridis)
  library(foreach)
  library(doFuture)
  library(ragg)
  library(mapmate)
  library(tools4watlas)
  library(terra)
  library(tidyterra)
  library(sf)
  library(av)
  library(data.table)
})

library(dplyr)
library(purrr)
library(ggplot2)
library(viridis)
library(foreach)
library(doFuture)
library(ragg)
library(mapmate)
library(tools4watlas)
library(terra)
library(tidyterra)
library(sf)
library(av)
library(data.table)

# ============================================
# Main function
# ============================================

generate_animation <- function(output_dir) {
  message("📂 Output directory: ", output_dir)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  unlink(file.path(output_dir, "*"), recursive = TRUE)
  
  # Load example data from package:
  data <- as.data.table(tools4watlas::data_example)
  
  # Create time steps
  time_steps <- atl_time_steps(
    datetime_vector = data$datetime,
    time_interval = "10 min",
    output_path = output_dir
  )
  
  # Load tide data (adjust paths as needed)
  tidal_pattern_fp <- "/app/waterdata/allYears-tidalPattern-west_terschelling-UTC.csv"
  measured_water_fp <- "/app/waterdata/allYears-gemeten_waterhoogte-west_terschelling-clean-UTC.csv"
  tidal_pattern <- fread(tidal_pattern_fp)
  measured_water <- fread(measured_water_fp)
  
  time_steps[, time := as.numeric(datetime)]
  time_steps <- atl_add_tidal_data(
    data = time_steps,
    tide_data = tidal_pattern,
    tide_data_highres = measured_water,
    waterdata_resolution = "10 min",
    waterdata_interpolation = "1 min",
    offset = 30
  )
  
  # Load bathymetry
  bat_raster <- rast("/app/bathymetry/bodemhoogte_20mtr_UTM31_int.tif")
  bbox <- atl_bbox(data, buffer = 800)
  bat_cropped <- crop(bat_raster, bbox)
  bat_wrapped <- wrap(bat_cropped)
  
  # ============================================
  # Parallel PNG generation
  # ============================================
  
  registerDoFuture()
  plan(multisession)
  
  foreach(i = seq_len(nrow(time_steps))) %dofuture% {
    step_time <- time_steps$datetime[i]
    water_level <- time_steps$waterlevel[i] / 100
    
    subset_data <- data[data$datetime %between% c(step_time - 6*3600, step_time), ]
    
    if (nrow(subset_data) > 0) {
      subset_data[, a := atl_alpha_along(datetime, head = 30, skew = -2), by = tag]
      subset_data[, s := atl_size_along(datetime, head = 70, to = c(0.3, 2)), by = tag]
    }
    
    bat_now <- unwrap(bat_wrapped)
    bat_mask <- bat_now < water_level
    bat_mask[bat_mask == 0] <- NA
    waterline_sf <- as.polygons(bat_mask, values = TRUE, dissolve = TRUE) |> st_as_sf()
    
    bm <- atl_create_bm(buffer = 1800, raster_data = bat_now, option = "bathymetry", shade = FALSE, scalebar = FALSE)
    
    p <- bm +
      geom_sf(data = waterline_sf, fill = "lightblue", alpha = 0.2, color = scales::alpha("white", 0.2), linewidth = 2) +
      geom_path(data = subset_data, aes(x, y, color = tag), alpha = subset_data$a, linewidth = subset_data$s) +
      annotate("text", x = -Inf, y = -Inf, hjust = -0.1, vjust = -2.4,
               label = format(step_time, "%Y-%m-%d %H:%M"), size = 4) +
      ggspatial::annotation_scale(aes(location = "br"), text_cex = 1, height = unit(0.3, "cm"),
                                  pad_x = unit(0.4, "cm"), pad_y = unit(0.6, "cm")) +
      coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"]), expand = FALSE)
    
    agg_png(filename = time_steps$path[i], width = 3840, height = 2160, units = "px", res = 300)
    print(p)
    dev.off()
  }
  
  plan(sequential)
  
  # ============================================
  # MP4 Animation
  # ============================================
  
  png_files <- list.files(output_dir, pattern = "\\.png$", full.names = TRUE) |> sort()
  video_out <- file.path(output_dir, "animation.mp4")
  
  av_encode_video(
    input = png_files,
    output = video_out,
    framerate = 10,
    vfilter = "scale=trunc(iw/2)*2:trunc(ih/2)*2"
  )
  
  message("✅ Animation written to: ", video_out)
  
  list(
    status = "success",
    video_path = video_out
  )
}
