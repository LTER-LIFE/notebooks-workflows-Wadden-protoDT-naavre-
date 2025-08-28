# ==================================================
# Watlas API Dockerfile (final fixed version)
# ==================================================

# Use a geospatial-ready R base image
FROM rocker/geospatial:4.3.2

# Install missing system libraries
RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    ffmpeg \
    && rm -rf /var/lib/apt/lists/*

# Install CRAN packages
RUN install2.r --error \
    plumber \
    jsonlite \
    httr \
    dplyr \
    purrr \
    ggplot2 \
    viridis \
    foreach \
    doFuture \
    ragg \
    data.table \
    tidyterra \
    av \
    remotes

# Install GitHub-only packages
RUN Rscript -e "remotes::install_github('leonawicz/mapmate')"
RUN Rscript -e "remotes::install_github('allertbijleveld/tools4watlas')"

WORKDIR /app

COPY plumber.R /app/
COPY generate_animation.R /app/

# Copy data folders (bathymetry and waterdata)
COPY bathymetry /app/bathymetry
COPY waterdata /app/waterdata

# Create animation_output folder (empty)
RUN mkdir -p /app/animation_output

EXPOSE 8000

CMD ["R", "-e", "pr <- plumber::pr('plumber.R'); pr$run(host='0.0.0.0', port=8000)"]
