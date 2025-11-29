FROM rocker/r2u:22.04

# r2u has pre-built Ubuntu binaries - much faster and more reliable
# Install system dependencies including fonts for hrbrthemes and PostgreSQL client
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpq-dev \
    fonts-roboto \
    && rm -rf /var/lib/apt/lists/* \
    && fc-cache -fv

# Install all R packages (RPostgres + pool for PostgreSQL connection pooling)
RUN install.r shiny shiny.semantic highcharter tidyr wesanderson \
    shinycssloaders shinyWidgets shinyjs dplyr readr maps DT RPostgres pool

# Verify RPostgres works
RUN R -e "library(RPostgres); library(pool); cat('RPostgres and pool loaded successfully\n')"

# Install font-related packages and hrbrthemes from GitHub
RUN R -e "install.packages(c('systemfonts', 'extrafont', 'remotes'), repos='https://cloud.r-project.org/')"
RUN R -e "remotes::install_github('hrbrmstr/hrbrthemes')"

# Create app directory
RUN mkdir -p /srv/shiny-server/shs_app

# Copy app files (mfd_app_v3.R from shs_modules - with responsive layout and updated title)
COPY shs_modules/mfd_app_v3.R /srv/shiny-server/shs_app/app.R
COPY shs_modules/mapdata.rds /srv/shiny-server/shs_app/

# Set working directory
WORKDIR /srv/shiny-server/shs_app

# Expose port
EXPOSE 3838

# Run app (database connection configured via environment variables)
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/shs_app', host='0.0.0.0', port=3838)"]
