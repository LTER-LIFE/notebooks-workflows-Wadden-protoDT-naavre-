# ============================================
# 🌐 Watlas Animation API
# ============================================

library(plumber)
library(jsonlite)
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

# Source the animation generator function
source("/app/generate_animation.R")

# ============================================
# Health check endpoint
# ============================================

#* API Health check
#* @get /health
function() {
  list(
    status = "ok",
    message = "Watlas animation API is running.",
    timestamp = Sys.time()
  )
}

# ============================================
# Animation endpoint
# ============================================

#* Generate animation
#* @param output_dir:optional The folder where output files (PNGs & MP4) will be saved.
#* @get /generate
function(output_dir = NULL, res) {
  tryCatch({
    if (is.null(output_dir) || output_dir == "") {
      output_dir <- tempfile("watlas_animation_")
      message("No output_dir provided; using temp folder: ", output_dir)
    }
    
    result <- generate_animation(output_dir = output_dir)
    
    list(
      status = "success",
      video_path = result$video_path,
      video_url = "http://localhost:8000/animation_output/animation.mp4"
    )
  }, error = function(e) {
    res$status <- 500
    list(
      status = "error",
      message = e$message
    )
  })
}

# Serve static assets from aniomation_output folder
#* @assets /animation_output /app/animation_output
function(){}

#* Display animation video
#* @get /video
#* @serializer html
function() {
  video_url <- "/animation_output/animation.mp4"
  html <- sprintf('
    <html>
      <body>
        <h3>Animation Video</h3>
        <video width="800" controls>
          <source src="%s" type="video/mp4">
          Your browser does not support the video tag.
        </video>
      </body>
    </html>', video_url)
  html
}

