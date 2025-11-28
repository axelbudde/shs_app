FROM rocker/shiny:4.3.3

# Install system dependencies including fonts for hrbrthemes
RUN apt-get update && apt-get install -y \
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
    fonts-roboto \
    fonts-roboto-fontface \
    && rm -rf /var/lib/apt/lists/* \
    && fc-cache -fv

# Install core R packages
RUN R -e "install.packages(c( \
    'shiny', \
    'shiny.semantic', \
    'highcharter', \
    'tidyr', \
    'wesanderson', \
    'shinycssloaders', \
    'shinyWidgets', \
    'shinyjs', \
    'dplyr', \
    'readr', \
    'maps', \
    'DT' \
), repos='https://cloud.r-project.org/')"

# Install duckdb separately (large package) with explicit verification
RUN R -e "install.packages('duckdb', repos='https://cloud.r-project.org/')" && \
    R -e "if (!require('duckdb')) stop('duckdb installation failed!')"

# Install font-related packages and hrbrthemes from GitHub
RUN R -e "install.packages(c('systemfonts', 'extrafont', 'remotes'), repos='https://cloud.r-project.org/')"
RUN R -e "remotes::install_github('hrbrmstr/hrbrthemes')"

# Create app directory
RUN mkdir -p /srv/shiny-server/shs_app

# Copy app files (mfd_app_v3.R from shs_modules - with responsive layout and updated title)
COPY shs_modules/mfd_app_v3.R /srv/shiny-server/shs_app/app.R
COPY ihme_data.duckdb /srv/shiny-server/shs_app/
COPY shs_modules/mapdata.rds /srv/shiny-server/shs_app/

# Set working directory
WORKDIR /srv/shiny-server/shs_app

# Expose port
EXPOSE 3838

# Run app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/shs_app', host='0.0.0.0', port=3838)"]
