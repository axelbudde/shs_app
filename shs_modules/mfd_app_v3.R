library(readr)
library(shiny)
library(shiny.semantic)
library(highcharter)
library(tidyr)
library(wesanderson)
library(shinycssloaders)
library(dplyr)
library(shinyWidgets)
library(shinyjs)
library(maps)
library(DT)
library(duckdb)

myGridTemplate <- grid_template(
  default = list(
    areas = rbind(
      c("user", "map"),
      c("user", "mfd")
    ),
    cols_width = c(
      "350px",
      "1fr"
    ),
    rows_height = c(
      "0.5fr",
      "0.5fr"
    )
  )
)

bar_charts_grid <- grid_template(
  default = list(
    areas = cbind(
      c("tab_1"),
      c("tab_2")
    ),
    cols_width = c("0.5fr", "0.5fr"),
    rows_height = c("1fr")
  )
)

hierarchical_grid <- grid_template(
  default = list(
    areas = rbind(
      c("sunburst"),
      c("treemap")
    ),
    cols_width = c("0.5fr", "0.5fr"),
    rows_height = c("1fr")
  )
)

# display_grid(myGridTemplate)

# Connect to DuckDB database (fast startup, queries on demand)
db_con <- dbConnect(duckdb(), "ihme_data.duckdb", read_only = TRUE)

# Helper function to query the database
query_data <- function(con, causes, sex, measure = NULL) {
  # Build SQL query with parameterized filters
  cause_list <- paste0("'", gsub("'", "''", causes), "'", collapse = ", ")
  sex_escaped <- gsub("'", "''", sex)

  if (is.null(measure)) {
    sql <- sprintf("
      SELECT * FROM ihme_data_geo
      WHERE cause_name IN (%s)
      AND sex_label = '%s'
    ", cause_list, sex_escaped)
  } else {
    measure_escaped <- gsub("'", "''", measure)
    sql <- sprintf("
      SELECT * FROM ihme_data_geo
      WHERE cause_name IN (%s)
      AND sex_label = '%s'
      AND measure_name = '%s'
    ", cause_list, sex_escaped, measure_escaped)
  }

  dbGetQuery(con, sql)
}

# Optimized function for SHS aggregation - pushes GROUP BY to DuckDB
query_shs_aggregated <- function(con, causes, sex) {
  cause_list <- paste0("'", gsub("'", "''", causes), "'", collapse = ", ")
  sex_escaped <- gsub("'", "''", sex)

  # Do the aggregation in SQL (much faster than R)
  sql <- sprintf("
    SELECT
      location_name, location_id, year_id, age_group_name, age_id,
      sex_label, cause_name, income_group, continent, subregion, metric_name,
      SUM(CASE WHEN measure_name = 'Deaths' THEN val ELSE 0 END) as deaths_val,
      SUM(CASE WHEN measure_name = 'Prevalence' THEN val ELSE 0 END) as prev_val,
      SUM(CASE WHEN measure_name = 'Incidence' THEN val ELSE 0 END) as inc_val
    FROM ihme_data_geo
    WHERE cause_name IN (%s)
    AND sex_label = '%s'
    GROUP BY
      location_name, location_id, year_id, age_group_name, age_id,
      sex_label, cause_name, income_group, continent, subregion, metric_name
  ", cause_list, sex_escaped)

  dbGetQuery(con, sql)
}

# Define all SHS causes (used for filtering when SHS toggle is on)
all_shs_causes <- c(
  "Acute glomerulonephritis",
  "Alzheimer's disease and other dementias",
  "Chronic kidney disease",
  "Cirrhosis and other chronic liver diseases",
  "Congenital birth defects",
  "Diabetes mellitus",
  "Extensively drug-resistant tuberculosis",
  "HIV/AIDS",
  "Injuries",
  "Intracerebral hemorrhage",
  "Ischemic stroke",
  "Leukemia",
  "Multidrug-resistant tuberculosis without extensive drug resistance",
  "Multiple sclerosis",
  "Musculoskeletal disorders",
  "Neonatal encephalopathy due to birth asphyxia and trauma",
  "Neonatal preterm birth",
  "Neoplasms",
  "Other digestive diseases",
  "Other neglected tropical diseases",
  "Other neoplasms",
  "Parkinson's disease",
  "Schistosomiasis",
  "Sickle cell disorders",
  "Subarachnoid hemorrhage",
  "Tetanus",
  "Thalassemias"
)

# Set highcharter options
options(highcharter.theme = hc_theme_economist(tooltip = list(valueDecimals = 3)))

# Get dropdown values from DuckDB (fast queries)
sexes <- as.matrix(dbGetQuery(db_con, "SELECT DISTINCT sex_label FROM ihme_data ORDER BY sex_label")$sex_label)

ages <- c(
  "Age-standardized",
  "<1 year",
  "1-4 years",
  "5-9 years",
  "10-14 years",
  "15-19 years",
  "20-24 years",
  "25-29 years",
  "30-34 years",
  "35-39 years",
  "40-44 years",
  "45-49 years",
  "50-54 years",
  "55-59 years",
  "60-64 years",
  "65-69 years",
  "70-74 years",
  "75-79 years",
  "80-84",
  "85-89",
  "90-94",
  "95+ years",
  "All ages"
)

ages_thalassemias <- c(
  "<1 year",
  "1-4 years",
  "5-9 years",
  "10-14 years",
  "15-19 years"
)

measures <- as.list(dbGetQuery(db_con, "SELECT DISTINCT measure_name FROM ihme_data ORDER BY measure_name")$measure_name)

years <- as.list(dbGetQuery(db_con, "SELECT DISTINCT year_id FROM ihme_data ORDER BY year_id")$year_id)

health_conditions <- c(
  "All health conditions",
  "Acute glomerulonephritis",
  "Alzheimer's disease and other dementias",
  "Chronic kidney disease",
  "Cirrhosis and other chronic liver diseases",
  "Congenital birth defects",
  "Diabetes mellitus",
  "Drug-susceptible tuberculosis",
  "Extensively drug-resistant tuberculosis",
  "HIV/AIDS",
  "Injuries",
  "Intracerebral hemorrhage",
  "Ischemic stroke",
  "Leukemia",
  "Multidrug-resistant tuberculosis without extensive drug resistance",
  "Multiple sclerosis",
  "Musculoskeletal disorders",
  "Neonatal encephalopathy due to birth asphyxia and trauma",
  "Neonatal preterm birth",
  "Neoplasms",
  "Other digestive diseases",
  "Other neglected tropical diseases",
  "Other neoplasms",
  "Parkinson's disease",
  "Schistosomiasis",
  "Sickle cell disorders",
  "Subarachnoid hemorrhage",
  "Tetanus",
  "Thalassemias"
)

health_conditions_shs <- c(
  "All health conditions",
  "Alzheimer's disease and other dementias",
  "Congenital birth defects",
  "Diabetes mellitus",
  "HIV/AIDS",
  "Kidney diseases",
  "Injuries",
  "Leukemia",
  "Liver diseases",
  "Multiple sclerosis",
  "Musculoskeletal disorders",
  "Neonatal encephalopathy due to birth asphyxia and trauma",
  "Neonatal preterm birth",
  "Neoplasms",
  "Other neglected tropical diseases",
  "Other neoplasms",
  "Parkinson's disease",
  "Sickle cell disorders",
  "Stroke",
  "Tetanus",
  "Thalassemias",
  "Tuberculosis"
)

metrics <- as.list(dbGetQuery(db_con, "SELECT DISTINCT metric_name FROM ihme_data ORDER BY metric_name")$metric_name)

# Use cached map data for coords (geographic data is in DuckDB via ihme_data_geo view)
if (file.exists("mapdata.rds")) {
  mapdata <- read_rds("mapdata.rds")
} else {
  mapdata <- get_data_from_map(download_map_data("custom/world-highres"))
  saveRDS(mapdata, "mapdata.rds")
}

# Note: ihme_data_population.R join with mapdata is now done in DuckDB
# The ihme_data_geo view already has continent, subregion, region_wb columns

coords_data <- mapdata %>%
  rename(
    x = "hc-middle-x",
    y = "hc-middle-y",
    names = "name"
  ) %>%
  select(
    x,
    y,
    names
  )

ui <- semanticPage(
  tags$style(HTML(".btn  { margin-bottom: 10px;}")),
  tags$script('
   document.addEventListener("click", function(e) {
      Shiny.setInputValue("shiftclick", e.shiftKey);
   });
 '),
  tags$script('$(document).on("shiny:sessioninitialized",function(){$Intl.DateTimeFormat().resolvedOptions().timeZone, function(response) {Shiny.setInputValue("getIP", response);});})'),
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),
  tags$script('
  $(document).ready(function () {
    navigator.geolocation.getCurrentPosition(onSuccess, onError);

    function onError (err) {
    Shiny.onInputChange("geolocation", false);
    }

   function onSuccess (position) {
      setTimeout(function () {
          var coords = position.coords;
          console.log(coords.latitude + ", " + coords.longitude);
          Shiny.onInputChange("geolocation", true);
          Shiny.onInputChange("lat", coords.latitude);
          Shiny.onInputChange("long", coords.longitude);
      }, 1100)
  }
  });
'),
  useShinyjs(),
  margin = "20px",
  # CSS replaces deprecated setSliderColor
  tags$style(HTML("
    .irs-bar, .irs-bar-edge, .irs-single, .irs-to, .irs-from {
      background: #6794A7 !important;
      border-color: #6794A7 !important;
    }
  ")),
  # Responsive CSS
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0, maximum-scale=1.0"),
    # JavaScript to handle responsive grid at runtime
    tags$script(HTML("
      $(document).ready(function() {
        function adjustGridForScreenSize() {
          var screenWidth = window.innerWidth;
          var gridContainer = $('.ss-grid').first();

          if (screenWidth <= 1024) {
            // Stack vertically on smaller screens
            gridContainer.css({
              'display': 'block',
              'grid-template-columns': 'none',
              'grid-template-areas': 'none'
            });
            gridContainer.children().css({
              'width': '100%',
              'max-width': '100%',
              'grid-area': 'unset',
              'margin-bottom': '10px'
            });
          } else {
            // Restore grid on larger screens
            gridContainer.css({
              'display': 'grid',
              'grid-template-columns': 'minmax(280px, 350px) 1fr',
              'grid-template-areas': '\"user map\" \"user mfd\"'
            });
            gridContainer.children().each(function() {
              $(this).css({
                'width': '',
                'max-width': ''
              });
            });
          }
        }

        // Run on load and resize
        adjustGridForScreenSize();
        $(window).resize(function() {
          adjustGridForScreenSize();
        });
      });
    ")),
    tags$style(HTML("
      /* Base responsive styles - prevent horizontal overflow */
      html, body {
        overflow-x: hidden !important;
        max-width: 100vw !important;
        box-sizing: border-box;
      }

      *, *::before, *::after {
        box-sizing: border-box;
      }

      .ui.grid {
        margin: 0 !important;
        max-width: 100% !important;
      }

      /* Chart cards - make them flexible */
      .ui.card, .ui.cards > .card {
        box-sizing: border-box;
        max-width: 100% !important;
      }

      /* Semantic page container */
      .pusher, .ui.container {
        max-width: 100% !important;
        overflow-x: hidden !important;
      }

      /* shiny.semantic grid */
      .ss-grid {
        max-width: 100% !important;
        overflow-x: hidden !important;
      }

      .ss-grid > div {
        max-width: 100% !important;
        overflow-x: hidden !important;
      }

      /* Standard monitors: 1024px - 1400px */
      @media screen and (max-width: 1400px) {
        /* Make grid template more flexible */
        .ss-grid {
          display: flex !important;
          flex-wrap: wrap !important;
        }

        .ss-grid > div[style*='grid-area: user'] {
          flex: 0 0 300px !important;
          max-width: 300px !important;
        }

        .ss-grid > div[style*='grid-area: map'],
        .ss-grid > div[style*='grid-area: mfd'] {
          flex: 1 1 calc(100% - 320px) !important;
          max-width: calc(100% - 320px) !important;
        }
      }

      /* Tablet: 768px - 1024px */
      @media screen and (max-width: 1024px) {
        /* Stack grid vertically */
        .ss-grid {
          display: block !important;
        }

        /* Settings panel: full width on tablets */
        .ss-grid > div[style*='grid-area: user'],
        .ss-grid > div[style*='grid-area: map'],
        .ss-grid > div[style*='grid-area: mfd'] {
          width: 100% !important;
          max-width: 100% !important;
          margin: 5px 0 !important;
        }

        /* Chart cards: side by side but smaller */
        .ui.card[style*='width: 48%'] {
          width: 48% !important;
          min-width: 280px;
        }

        /* Reduce margins */
        .ui.form .field {
          margin-bottom: 0.5em !important;
        }

        /* Smaller headers */
        .ui.header {
          font-size: 1.1em !important;
        }

        h3.ui.header {
          font-size: 1em !important;
        }
      }

      /* Mobile: below 768px */
      @media screen and (max-width: 768px) {
        /* Full width for all major sections */
        .ss-grid > div {
          width: 100% !important;
          max-width: 100% !important;
          margin: 5px 0 !important;
        }

        /* Chart cards: stack vertically */
        .ui.card[style*='width: 48%'],
        .cards .ui.card,
        .ui.card {
          width: 100% !important;
          max-width: 100% !important;
          margin: 10px 0 !important;
        }

        /* Cards container: stack children */
        .ui.cards {
          flex-direction: column !important;
        }

        /* Reduce page margin */
        body {
          padding: 5px !important;
          margin: 5px !important;
        }

        /* Radio buttons: stack if needed */
        .inline.fields {
          flex-wrap: wrap !important;
        }

        .inline.fields .field {
          margin-bottom: 5px !important;
        }

        /* Smaller fonts */
        .ui.header {
          font-size: 1em !important;
        }

        h3.ui.header {
          font-size: 0.95em !important;
        }

        /* Make dropdowns full width */
        .ui.dropdown, .ui.selection.dropdown {
          width: 100% !important;
          max-width: 100% !important;
        }

        /* Adjust chart heights for mobile */
        .highcharts-container {
          min-height: 250px !important;
        }

        /* Tab menu: scrollable on mobile */
        .ui.tabular.menu {
          overflow-x: auto;
          flex-wrap: nowrap;
        }

        .ui.tabular.menu .item {
          flex-shrink: 0;
        }

        /* Ensure tables don't overflow */
        .dataTables_wrapper {
          max-width: 100% !important;
          overflow-x: auto !important;
        }

        table.dataTable {
          width: 100% !important;
        }
      }

      /* Small mobile: below 480px */
      @media screen and (max-width: 480px) {
        /* Even more compact */
        .ui.form .field {
          margin-bottom: 0.3em !important;
        }

        /* Smaller padding */
        .ui.card > .content {
          padding: 0.8em !important;
        }

        /* Radio labels smaller */
        .ui.radio.checkbox label {
          font-size: 0.85em !important;
        }

        /* DataTable adjustments */
        .dataTables_wrapper {
          font-size: 0.85em;
        }

        table.dataTable {
          font-size: 0.8em !important;
        }
      }

      /* Ensure highcharts are responsive */
      .highcharts-container, .highchart {
        width: 100% !important;
        max-width: 100% !important;
      }
    "))
  ),
  grid(
    myGridTemplate,
    area_styles = list(
      title = "margin: 10px",
      mfd = "margin: 10px",
      user = "margin: 10px",
      map = "margin: 10px",
      bubble = "margin: 10px",
      sunburst = "margin: 10px"
    ),
    title = card(
      style = "border-radius: 0; width: 100%; background: #ffffff",
      div(
        class = "content",
tags$h1(
          style = "margin: 0; font-size: 1.8em; color: #333;",
          "Serious Health-Related Suffering (SHS)"
        )
      )
    ),
    mfd = card(
      style = "border-radius: 0; width: 100%; background: #ffffff",
      div(
        class = "content",
        tabset(
          tabs = list(
            list(
              menu =
                h3(
                  list(
                    icon("bar chart icon"),
                    " ",
                    "SHS Bar Charts"
                  )
                ),
              content =
                div(
                  class = "ui cards",
                  style = "border-radius: 0; width: 100%; background: #ffffff",
                  card(
                    class = "ui card",
                    style = "border-radius: 0; width: 48%; background: #ffffff",
                    div(
                      class = "content",
                      form(
                        class = "ui form",
                        field(
                          class = "inline fields",
                          tags$h3(
                            class = "ui header",
                            "SHS Historical Chart"
                          ),
                          icon("ruler horizontal icon", style = "margin-left: 10px; margin-right: 5px; color: #666;"),
                          multiple_radio(
                            input_id = "radio_indicator",
                            label = "",
                            choices = c(
                              "per 100k",
                              "Absolute"
                            ),
                            choices_value = c(
                              "Rate",
                              "Number"
                            ),
                            selected = "Rate",
                            position = "inline",
                            type = "radio"
                          )
                        ),
                        withSpinner(
                          highchartOutput(
                            "shs_historical_chart"
                          ),
                          type = 8,
                          color = "#6794A7",
                          hide.ui = TRUE
                        )
                      )
                    )
                  ),
                  card(
                    style = "border-radius: 0; width: 48%; background: #ffffff",
                    div(
                      class = "content",
                      form(
                        class = "ui form",
                        field(
                          class = "inline fields",
                          tags$h3(
                            class = "ui header",
                            "SHS Country Ranking"
                          ),
                          multiple_radio(
                            input_id = "order",
                            label = "",
                            choices = c(
                              "Top 20",
                              "Bottom 20"
                            ),
                            choices_value = c(
                              "top_20",
                              "bottom_20"
                            ),
                            selected = "top_20",
                            position = "inline",
                            type = "radio"
                          )
                        ),
                        withSpinner(
                          ui_element = highchartOutput(
                            "shs_top_ten"
                          ),
                          type = 8,
                          color = "#6794A7",
                          hide.ui = TRUE
                        )
                      )
                    )
                  )
                ),
              id = "shs_history_tab"
            ),
            list(
              menu =
                h3(
                  list(
                    icon("sitemap icon"),
                    " ",
                    "SHS Hierarchy"
                  )
                ),
              content =
                div(
                  class = "ui cards",
                  style = "border-radius: 0; width: 100%; background: #ffffff",
                  card(
                    style = "border-radius: 0; width: 48%; background: #ffffff",
                    div(
                      class = "content",
                      form(
                        class = "ui form",
                        field(
                          class = "inline fields",
                          tags$h3(
                            class = "ui header",
                            "SHS Hierarchy"
                          ),
                          div(),
                          multiple_radio(
                            input_id = "sunburst_treemap",
                            label = "",
                            choices = c(
                              "Sunburst",
                              "Treemap"
                            ),
                            choices_value = c(
                              "sunburst",
                              "treemap"
                            ),
                            selected = "sunburst",
                            position = "inline",
                            type = "radio"
                          )
                        ),
                        withSpinner(
                          ui_element = highchartOutput(
                            "shs_sunburst"
                          ),
                          type = 8,
                          color = "#6794A7",
                          hide.ui = TRUE
                        )
                      )
                    )
                  ),
                  card(
                    style = "border-radius: 0; width: 48%; background: #ffffff",
                    div(
                      class = "content",
                      form(
                        class = "ui form",
                        field(
                          class = "inline fields",
                          tags$h3(
                            class = "ui header",
                            "SHS Circle Packing"
                          ),
                          multiple_radio(
                            input_id = "breakdown",
                            label = "",
                            choices = c(
                              "Continents",
                              "Regions",
                              "Subregions",
                              "Income"
                            ),
                            choices_value = c(
                              "continent",
                              "region_wb",
                              "subregion",
                              "income_group"
                            ),
                            selected = "continent",
                            position = "inline",
                            type = "radio"
                          )
                        ),
                        withSpinner(
                          ui_element = highchartOutput(
                            "shs_bubble"
                          ),
                          type = 8,
                          color = "#6794A7",
                          hide.ui = TRUE
                        )
                      )
                    )
                  )
                ),
              id = "shs_ranking_tab"
            ),
            list(
              menu =
                h3(
                  list(
                    icon("table icon"),
                    " ",
                    "SHS Table"
                  )
                ),
              content =
                div(
                  class = "ui cards",
                  style = "border-radius: 0; width: 100%; background: #ffffff",
                  div(
                    class = "ui card",
                    style = "border-radius: 0; width: 100%; background: #ffffff",
                    div(
                      class = "content",
                      form(
                        class = "ui form",
                        field(
                          class = "inline fields",
                          tags$h3(
                            class = "ui header",
                            "SHS Table"
                          ),
                          div()
                        )
                      ),
                      div(
                        class = "header",
                        textOutput("shs_table_title"),
                        div(
                          class = "meta",
                          textOutput("shs_table_sub_title")
                        )
                      ),
                      withSpinner(
                        ui_element = semantic_DTOutput(
                          "shs_table"
                        ),
                        type = 8,
                        color = "#6794A7",
                        hide.ui = TRUE
                      )
                    )
                  )
                ), id = "shs_table_tab"
            )
          ),
          menu_class = "ui secondary pointing menu"
        )
      )
    ),
    map = card(
      style = "border-radius: 0; height: 100%; width: 100%; background: #ffffff",
      div(
        class = "content",
        form(
          class = "ui form",
          field(
            class = "inline fields",
            tags$h3(
              class = "ui header",
              icon("globe icon"),
              "SHS World Map"
            ),
            multiple_radio(
              input_id = "radio_scale",
              label = "",
              choices = c(
                "Linear",
                "Logarithmic"
              ),
              choices_value = c(
                "linear",
                "logarithmic"
              ),
              selected = "linear",
              position = "inline",
              type = "radio",
              icon = "ruler horizontal icon"
            )
          ),
          withSpinner(
            ui_element = highchartOutput(
              "shs_map",
              height = "48vh"
            ),
            type = 8,
            color = "#6794A7",
            hide.ui = FALSE
          )
        )
      )
    ),
    user = card(
      style = "border-radius: 0; height = 100%; width: 100%; background: #F5F5F5",
      div(
        class = "content",
        tags$h3(
          class = "ui header",
          icon("cog icon"),
          "SHS World Map Settings"
        ),
        actionButton(
          "reset_settings",
          "Reset to defaults",
          class = "ui mini basic button",
          style = "margin-bottom: 10px;"
        ),
        br(),
        form(
          class = "ui equal width form",
          field(
            class = "inline fields",
            tags$h5(
              class = "ui header",
              icon("weight icon"),
              div(
                class = "content",
                "Measure"
              )
            ),
            dropdown_input(
              input_id = "shs_measure_name",
              choices = measures,
              value = "Prevalence",
              type = "ui fluid search selection dropdown"
            )
          ),
          field(
            class = "inline fields",
            shiny.semantic::toggle(
              input_id = "shs_toggle",
              label = tags$h5("Calculate SHS"),
              is_marked = FALSE
            )
          )
        ),
        br(),
        form(
          class = "ui equal width form",
          field(
            class = "inline fields",
            tags$h5(
              class = "ui header",
              icon("stethoscope icon"),
              div(
                class = "content",
                "Cause"
              )
            ),
            dropdown_input(
              "shs_health_condition_name",
              choices = health_conditions,
              value = "All health conditions",
              type = "selection fluid"
            )
          )
        ),
        br(),
        form(
          class = "ui form",
          field(
            class = "inline fields",
            tags$h5(
              class = "ui header",
              icon("user clock icon"),
              div(
                class = "content",
                "Age"
              )
            ),
            dropdown_input(
              input_id = "shs_age_group_name",
              choices = ages,
              default_text = "Select age group",
              value = "Age-standardized",
              type = "selection fluid"
            )
          )
        ),
        br(),
        form(
          class = "ui form",
          field(
            class = "inline fields",
            tags$h5(
              class = "ui header",
              icon("mars venus icon"),
              div(
                class = "content",
                "Sex"
              )
            ),
            multiple_radio(
              input_id = "shs_sex_label",
              label = "",
              choices = c(
                "Both",
                "Male",
                "Female"
              ),
              choices_value = c(
                "Both",
                "Male",
                "Female"
              ),
              selected = "Both",
              position = "inline",
              type = "radio"
            )
          )
        ),
        form(
          class = "ui form",
          field(
            class = "inline fields",
            tags$h5(
              class = "ui header",
              icon("history icon"),
              div(
                class = "content",
                "Year"
              )
            ),
            sliderTextInput(
              inputId = "shs_year_id",
              label = NULL,
              choices = as.character(years),
              selected = as.character(2019),
              animate = TRUE,
              grid = TRUE,
              force_edges = TRUE
            )
          )
        )
      )
    ),
    info = card(
      style = "border-radius: 0; width: 100%; background: #ffffff",
      div(
        class = "content",
        form(
          class = "ui form",
          field(
            class = "inline fields",
            tags$h3(
              class = "ui header",
              icon("bar chart icon"),
              "SHS Historical Chart"
            ),
            tags$span(
              style = "color: #666; font-size: 12px; margin-left: 10px;",
              "Shift+click on map to compare countries"
            )
          ),
          withSpinner(
            highchartOutput(
              "shs_historical_chart"
            ),
            type = 8,
            color = "#6794A7",
            hide.ui = TRUE
          )
        )
      )
    ),
    top_ten = card(
      style = "border-radius: 0; width: 100%; background: #ffffff",
      div(
        class = "content",
        form(
          class = "ui form",
          field(
            class = "inline fields",
            tags$h3(
              class = "ui header",
              icon("sort icon"),
              "SHS Country Ranking"
            ),
            multiple_radio(
              input_id = "order",
              label = "",
              choices = c(
                "Top 20",
                "Bottom 20"
              ),
              choices_value = c(
                "top_20",
                "bottom_20"
              ),
              selected = "top_20",
              position = "inline",
              type = "radio"
            )
          ),
          withSpinner(
            ui_element = highchartOutput(
              "shs_top_ten"
            ),
            type = 8,
            color = "#6794A7",
            hide.ui = TRUE
          )
        )
      )
    ),
    bubble = card(
      style = "border-radius: 0; width:100%; background: #ffffff",
      div(
        class = "content",
        form(
          class = "ui form",
          field(
            class = "inline fields",
            tags$h3(
              class = "ui header",
              icon("bowling ball icon"),
              "SHS Circle Packing"
            ),
            multiple_radio(
              input_id = "breakdown",
              label = "",
              choices = c(
                "Continents",
                "Regions",
                "Subregions",
                "Income"
              ),
              choices_value = c(
                "continent",
                "region_wb",
                "subregion",
                "income_group"
              ),
              selected = "continent",
              position = "inline",
              type = "radio"
            )
          ),
          withSpinner(
            ui_element = highchartOutput(
              "shs_bubble",
              height = "90vh"
            ),
            type = 8,
            color = "#6794A7",
            hide.ui = TRUE
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Map
  click_js <- JS("function(event) {Shiny.setInputValue('mapclick',event.point.name);}")

  observeEvent(input$controller, {
    updateTabsetPanel(session, "hidden_tabs", selected = paste0("panel", input$controller))
  })

  output$browser_height <- renderText({
    paste0(shinybrowser::get_height())
  })

  observeEvent(input$chartclick, {
    output$chartclick <- renderPrint({
      input$chartclick
    })
  })

  observeEvent(input$mapclick, {
    output$mapclick <- renderPrint({
      input$mapclick
    })
  })

  observeEvent(input$shiftclick, {
    output$shiftclick <- renderPrint({
      input$shiftclick
    })
  })

  # Reset settings button handler
  observeEvent(input$reset_settings, {
    # Reset all settings to defaults
    update_dropdown_input(session, "shs_measure_name", value = "Prevalence")
    update_dropdown_input(session, "shs_health_condition_name",
                          choices = health_conditions, value = "All health conditions")
    update_dropdown_input(session, "shs_age_group_name",
                          choices = ages, value = "Age-standardized")
    shinyWidgets::updateSliderTextInput(session, "shs_year_id", selected = "2019")
    # Reset sex radio to "Both" using JavaScript
    shinyjs::runjs("$('#shs_sex_label input[value=\"Both\"]').prop('checked', true).trigger('change');")
    # Reset indicator radio to "Rate" using JavaScript
    shinyjs::runjs("$('#radio_indicator input[value=\"Rate\"]').prop('checked', true).trigger('change');")
    # Disable SHS toggle (turn it off)
    shinyjs::runjs("$('#shs_toggle').checkbox('uncheck');")
    shinyjs::enable("shs_measure_name")
  })

  # SHS toggle observer - disable measure dropdown when SHS is on
  observeEvent(input$shs_toggle, {
    if (isTRUE(input$shs_toggle)) {
      shinyjs::disable("shs_measure_name")
      # Update health conditions to SHS-specific list
      update_dropdown_input(
        session,
        "shs_health_condition_name",
        choices = health_conditions_shs,
        value = if (input$shs_health_condition_name %in% health_conditions_shs) {
          input$shs_health_condition_name
        } else {
          "All health conditions"
        }
      )
    } else {
      shinyjs::enable("shs_measure_name")
      # Restore full health conditions list
      update_dropdown_input(
        session,
        "shs_health_condition_name",
        choices = health_conditions,
        value = if (input$shs_health_condition_name %in% health_conditions) {
          input$shs_health_condition_name
        } else {
          "All health conditions"
        }
      )
    }
  })

  observeConditions <- reactive({
    list(
      input$shs_measure_name,
      input$shs_health_condition_name
    )
  })

  observeEvent(observeConditions(), {
    if (input$shs_measure_name == "SHS" & input$shs_health_condition_name == "Thalassemias") {
      update_dropdown_input(
        session,
        "shs_age_group_name",
        choices = ages_thalassemias,
        value = case_when(
          input$shs_age_group_name %in% ages_thalassemias ~ input$shs_age_group_name,
          TRUE ~ "<1 year"
        )
      )
    } else {
      update_dropdown_input(
        session,
        "shs_age_group_name",
        choices = ages,
        value = input$shs_age_group_name
      )
    }
  })

  observeEvent(input$shs_measure_name, {
    if (input$shs_measure_name == "SHS") {
      update_dropdown_input(
        session,
        "shs_health_condition_name",
        choices = health_conditions_shs,
        value = case_when(
          input$shs_health_condition_name %in% c(
            "Drug-susceptible tuberculosis",
            "Multidrug-resistant tuberculosis without extensive drug resistance",
            "Extensively drug-resistant tuberculosis"
          ) ~ "Tuberculosis",
          TRUE ~ input$shs_health_condition_name
        )
      )
    } else {
      update_dropdown_input(
        session,
        "shs_health_condition_name",
        choices = health_conditions,
        value = case_when(
          input$shs_health_condition_name == "Tuberculosis" ~ "Drug-susceptible tuberculosis",
          TRUE ~ input$shs_health_condition_name
        )
      )
    }
  })

  observeEvent(input$shs_measure_name, {
    if (input$shs_measure_name == "SHS") {
      update_dropdown_input(
        session,
        "shs_health_condition_name",
        choices = health_conditions_shs,
        value = case_when(
          input$shs_health_condition_name %in% c(
            "Intracerebral hemorrhage",
            "Ischemic stroke",
            "Subarachnoid hemorrhage"
          ) ~ "Stroke",
          TRUE ~ input$shs_health_condition_name
        )
      )
    } else {
      update_dropdown_input(
        session,
        "shs_health_condition_name",
        choices = health_conditions,
        value = case_when(
          input$shs_health_condition_name == "Stroke" ~ "Ischemic stroke",
          TRUE ~ input$shs_health_condition_name
        )
      )
    }
  })

  observeEvent(input$shs_measure_name, {
    if (input$shs_measure_name == "SHS") {
      update_dropdown_input(
        session,
        "shs_health_condition_name",
        choices = health_conditions_shs,
        value = case_when(
          input$shs_health_condition_name %in% c(
            "Cirrhosis and other chronic liver diseases",
            "Other digestive diseases",
            "Schistosomiasis"
          ) ~ "Liver diseases",
          TRUE ~ input$shs_health_condition_name
        )
      )
    } else {
      update_dropdown_input(
        session,
        "shs_health_condition_name",
        choices = health_conditions,
        value = case_when(
          input$shs_health_condition_name == "Liver diseases" ~ "Cirrhosis and other chronic liver diseases",
          TRUE ~ input$shs_health_condition_name
        )
      )
    }
  })

  observeEvent(input$shs_measure_name, {
    if (input$shs_measure_name == "SHS") {
      update_dropdown_input(
        session,
        "shs_health_condition_name",
        choices = health_conditions_shs,
        value = case_when(
          input$shs_health_condition_name %in% c(
            "Acute glomerulonephritis",
            "Chronic kidney disease"
          ) ~ "Kidney diseases",
          TRUE ~ input$shs_health_condition_name
        )
      )
    } else {
      update_dropdown_input(
        session,
        "shs_health_condition_name",
        choices = health_conditions,
        value = case_when(
          input$shs_health_condition_name == "Kidney diseases" ~ "Chronic kidney disease",
          TRUE ~ input$shs_health_condition_name
        )
      )
    }
  })

  # printing output
  # we are using cat() instead of print function
  # so that we can use new line
  ip_country <- reactive({
    sub(
      pattern = ":.*",
      map.where(
        database = "world",
        x = input$long,
        y = input$lat
      ),
      replacement = ""
    )
  })

  output$ip_country <- renderPrint({
    ip_country()
  })

  select_location <- reactive({
    # Only depend on map click and initial IP country, not on input$shiftclick
    # This prevents the reactive from re-running when other inputs change
    click <- ifelse(is.null(input$mapclick),
      ip_country(),
      input$mapclick
    )

    # Return current selections if they exist and no new click was made
    if (length(selected_ids$ids) > 0) {
      return(selected_ids$ids)
    }

    click
  })

  # Track the last processed mapclick to avoid re-processing on map re-render
  last_processed_click <- reactiveVal(NULL)

  # Handle map clicks separately with observeEvent to properly manage state
  observeEvent(input$mapclick, {
    click <- input$mapclick

    # Skip if this is the same click as last time (can happen when map re-renders)
    if (!is.null(last_processed_click()) && identical(click, last_processed_click())) {
      return()
    }
    last_processed_click(click)

    clicked_ids$ids <- unique(c(
      click,
      first(clicked_ids$ids)
    ))

    if (isFALSE(input$shiftclick)) {
      # Regular click - reset to single country
      shift_clicked_ids$ids <- NULL
      selected_ids$ids <- NULL
    } else {
      # Shift+click - accumulate countries
      shift_clicked_ids$ids <- unique(c(
        clicked_ids$ids,
        shift_clicked_ids$ids
      ))

      selected_ids$ids <- unique(c(
        shift_clicked_ids$ids,
        clicked_ids$ids
      )) %>%
        na.omit()
    }
  }, ignoreInit = TRUE)

  output$select_location <- renderPrint({
    select_location()
  })



  output$health_condition <- renderPrint({
    input$shs_health_condition_name
  })

  output$age_name <- renderPrint({
    input$shs_age_group_name
  })

  output$sex_name <- renderPrint({
    input$shs_sex_label
  })

  output$year <- renderPrint({
    input$shs_year_id
  })

  output$metric <- renderPrint({
    input$radio_indicator
  })

  output$breakdown <- renderPrint({
    input$breakdown
  })

  # create empty vector to hold all click ids
  clicked_ids <- reactiveValues(ids = vector())
  selected_ids <- reactiveValues(ids = vector())
  shift_clicked_ids <- reactiveValues(ids = vector())

  toListen <- reactive({
    list(
      input$mapclick,
      input$getIP
    )
  })

  adapt_type <- reactive({
    case_when(
      length(selected_ids$ids) > 1 ~ "line",
      .default = "column"
    )
  })

  shs <- reactive({
    # Define all SHS causes
    all_shs_causes <- c(
      "Acute glomerulonephritis",
      "Alzheimer's disease and other dementias",
      "Chronic kidney disease",
      "Cirrhosis and other chronic liver diseases",
      "Congenital birth defects",
      "Diabetes mellitus",
      "Extensively drug-resistant tuberculosis",
      "HIV/AIDS",
      "Injuries",
      "Intracerebral hemorrhage",
      "Ischemic stroke",
      "Leukemia",
      "Multidrug-resistant tuberculosis without extensive drug resistance",
      "Multiple sclerosis",
      "Musculoskeletal disorders",
      "Neonatal encephalopathy due to birth asphyxia and trauma",
      "Neonatal preterm birth",
      "Neoplasms",
      "Other digestive diseases",
      "Other neglected tropical diseases",
      "Other neoplasms",
      "Parkinson's disease",
      "Schistosomiasis",
      "Sickle cell disorders",
      "Subarachnoid hemorrhage",
      "Tetanus",
      "Thalassemias"
    )

    # Determine which causes to include based on selected health condition
    selected_causes <- case_when(
      input$shs_health_condition_name == "All health conditions" ~ list(all_shs_causes),
      input$shs_health_condition_name == "Stroke" ~ list(c("Intracerebral hemorrhage", "Ischemic stroke", "Subarachnoid hemorrhage")),
      input$shs_health_condition_name == "Liver diseases" ~ list(c("Cirrhosis and other chronic liver diseases", "Other digestive diseases", "Schistosomiasis")),
      input$shs_health_condition_name == "Tuberculosis" ~ list(c("Multidrug-resistant tuberculosis without extensive drug resistance", "Extensively drug-resistant tuberculosis")),
      input$shs_health_condition_name == "Kidney diseases" ~ list(c("Acute glomerulonephritis", "Chronic kidney disease")),
      TRUE ~ list(input$shs_health_condition_name)
    )[[1]]

    if (isTRUE(input$shs_toggle)) {
      # SHS calculation mode - need all measures for the formula
      query_data(db_con, selected_causes, input$shs_sex_label)
    } else {
      # Standard measure mode
      query_data(db_con, selected_causes, input$shs_sex_label, input$shs_measure_name)
    }
  }) %>% bindCache(input$shs_health_condition_name, input$shs_sex_label, input$shs_measure_name, input$shs_toggle)

  # SHS calculation reactive - calculates SHS when toggle is on
  shs_calculated <- reactive({
    if (!isTRUE(input$shs_toggle)) {
      # Return filtered data as-is when SHS toggle is off
      return(shs())
    }

    # Determine which causes to include based on selected health condition
    selected_causes <- case_when(
      input$shs_health_condition_name == "All health conditions" ~ list(all_shs_causes),
      input$shs_health_condition_name == "Stroke" ~ list(c("Intracerebral hemorrhage", "Ischemic stroke", "Subarachnoid hemorrhage")),
      input$shs_health_condition_name == "Liver diseases" ~ list(c("Cirrhosis and other chronic liver diseases", "Other digestive diseases", "Schistosomiasis")),
      input$shs_health_condition_name == "Tuberculosis" ~ list(c("Multidrug-resistant tuberculosis without extensive drug resistance", "Extensively drug-resistant tuberculosis")),
      input$shs_health_condition_name == "Kidney diseases" ~ list(c("Acute glomerulonephritis", "Chronic kidney disease")),
      TRUE ~ list(input$shs_health_condition_name)
    )[[1]]

    # Use optimized SQL aggregation (GROUP BY in DuckDB, not R)
    query_shs_aggregated(db_con, selected_causes, input$shs_sex_label) %>%
      mutate(
        calc_val = case_when(
          cause_name == "Other neglected tropical diseases" ~ deaths_val / 3,
          cause_name %in% c(
            "Multidrug-resistant tuberculosis without extensive drug resistance",
            "Extensively drug-resistant tuberculosis"
          ) ~ inc_val - deaths_val / 6,
          cause_name == "HIV/AIDS" ~ prev_val / 3,
          cause_name == "Alzheimer's disease and other dementias" ~ prev_val / 3,
          cause_name == "Tetanus" ~ deaths_val / 3,
          cause_name == "Parkinson's disease" ~ prev_val / 3,
          cause_name == "Multiple sclerosis" ~ prev_val / 3,
          cause_name == "Congenital birth defects" ~ deaths_val / 3,
          cause_name == "Injuries" ~ deaths_val / 3,
          cause_name == "Musculoskeletal disorders" ~ deaths_val / 3,
          cause_name == "Sickle cell disorders" ~ prev_val / 3,
          cause_name %in% c(
            "Cirrhosis and other chronic liver diseases",
            "Other digestive diseases",
            "Schistosomiasis"
          ) ~ deaths_val / 9,
          cause_name %in% c(
            "Chronic kidney disease",
            "Acute glomerulonephritis"
          ) ~ deaths_val / 6,
          cause_name %in% c(
            "Intracerebral hemorrhage",
            "Ischemic stroke",
            "Subarachnoid hemorrhage"
          ) ~ deaths_val / 9,
          cause_name == "Diabetes mellitus" ~ prev_val / 3,
          cause_name == "Thalassemias" ~ prev_val / 3,
          cause_name == "Neonatal preterm birth" ~ prev_val / 3,
          cause_name == "Neonatal encephalopathy due to birth asphyxia and trauma" ~ prev_val / 3,
          TRUE ~ 0
        ),
        level_1_factor = case_when(
          cause_name == "Other neglected tropical diseases" ~ 0.05,
          cause_name %in% c(
            "Multidrug-resistant tuberculosis without extensive drug resistance",
            "Extensively drug-resistant tuberculosis"
          ) ~ 1,
          cause_name == "HIV/AIDS" ~ 1,
          cause_name == "Alzheimer's disease and other dementias" ~ 0.1,
          cause_name == "Tetanus" ~ 0.5,
          cause_name == "Parkinson's disease" ~ 0.1,
          cause_name == "Multiple sclerosis" ~ 0.017,
          cause_name == "Congenital birth defects" ~ 0.6,
          cause_name == "Injuries" ~ 0.6,
          cause_name == "Musculoskeletal disorders" ~ 1.4,
          cause_name == "Sickle cell disorders" & age_id %in% c(28, 5:8) ~ 0.7,
          cause_name == "Sickle cell disorders" & age_id %in% c(9:20, 30:235) ~ 0.5,
          cause_name %in% c("Cirrhosis and other chronic liver diseases", "Other digestive diseases", "Schistosomiasis") ~ 0.5,
          cause_name %in% c("Chronic kidney disease", "Acute glomerulonephritis") ~ 1,
          cause_name %in% c("Intracerebral hemorrhage", "Ischemic stroke", "Subarachnoid hemorrhage") ~ 1,
          cause_name == "Diabetes mellitus" ~ 0.1,
          cause_name == "Thalassemias" & age_id %in% c(28, 5) ~ 0.7,
          cause_name == "Thalassemias" & age_id %in% c(6:8) ~ 0.1,
          cause_name == "Neonatal preterm birth" ~ 0.01,
          cause_name == "Neonatal encephalopathy due to birth asphyxia and trauma" & age_id %in% c(28, 5) ~ 0.2,
          cause_name == "Neonatal encephalopathy due to birth asphyxia and trauma" & age_id %in% c(6:8) ~ 0.1,
          TRUE ~ 1
        ),
        level_2_factor = case_when(
          cause_name == "Multidrug-resistant tuberculosis without extensive drug resistance" ~ 0.5,
          cause_name == "Extensively drug-resistant tuberculosis" ~ 1,
          cause_name == "HIV/AIDS" ~ 0.5,
          cause_name %in% c("Intracerebral hemorrhage", "Ischemic stroke", "Subarachnoid hemorrhage") ~ 1.5,
          cause_name %in% c("Cirrhosis and other chronic liver diseases", "Other digestive diseases", "Schistosomiasis") ~
            if_else(age_id %in% c(5:8, 28), 0.95, 1.55),
          cause_name %in% c("Chronic kidney disease", "Acute glomerulonephritis") ~
            if_else(age_id %in% c(5:8, 28), 3, 0.9),
          TRUE ~ 1
        ),
        val = calc_val * level_1_factor * level_2_factor,
        measure_name = "SHS"
      ) %>%
      select(-calc_val, -level_1_factor, -level_2_factor, -deaths_val, -prev_val, -inc_val)
  }) %>% bindCache(input$shs_health_condition_name, input$shs_sex_label, input$shs_toggle)

  # Sunburst module ----

  viz_shs_sunburst <- function(shs, measure, year) {
    shs <- shs %>%
      filter(
        year_id == input$shs_year_id,
        age_group_name == case_when(
          input$shs_age_group_name == "Age-standardized" ~ "All ages",
          TRUE ~ input$shs_age_group_name
        ),
        metric_name == "Number"
      ) %>%
      group_by(
        continent,
        subregion,
        location_name
      ) %>%
      arrange(
        continent,
        subregion,
        location_name
      ) %>%
      summarise(val = sum(val))

    sunburst <- data_to_hierarchical(
      data = shs,
      group_vars = c(continent, subregion, location_name),
      size_var = val,
      colors = wes_palette(
        name = "Zissou1",
        n = length(unique(shs$continent)),
        type = "continuous"
      )
    )

    hchart(
      sunburst,
      type = input$sunburst_treemap,
      allowDrillToNode = TRUE,
      animationLimit = 1000,
      dataLabels = list(
        enabled = FALSE
      ),
      levels = list(
        level = 1,
        dataLabels = list(
          enabled = FALSE
        )
      )
    ) %>%
      hc_title(
        text = paste0(
          if (isTRUE(input$shs_toggle)) "SHS" else unique(input$shs_measure_name),
          ": ",
          unique(input$shs_health_condition_name)
        ),
        font = "Econ Sans Cnd",
        useHTML = TRUE
      ) %>%
      hc_subtitle(
        text = paste0("Age: ", unique(input$shs_age_group_name), ", ", "Sex: ", unique(input$shs_sex_label), ", ", "Year: ", unique(input$shs_year_id)),
        font = "Econ Sans Cnd",
        useHTML = TRUE
      ) %>%
      hc_tooltip(valueDecimals = 0) %>%
      hc_exporting(
        enabled = FALSE, # always enabled
        filename = "custom-file-name"
      ) %>%
      hc_credits(enabled = FALSE) %>%
      hc_loading() %>%
      hc_boost(enabled = TRUE)
  }

  shs_sunburst <- reactive({
    viz_shs_sunburst(
      shs_calculated(),
      "val",
      input$shs_year_id
    )
  })

  output$shs_sunburst <- renderHighchart({
    shs_sunburst()
  })

  # bubble module ----

  viz_shs_bubble <- function(shs, measure, year, breakdown) {
    shs <- shs %>%
      filter(
        year_id == input$shs_year_id,
        age_group_name == case_when(
          input$shs_age_group_name == "Age-standardized" ~ "All ages",
          TRUE ~ input$shs_age_group_name
        ),
        metric_name == "Number"
      ) %>%
      group_by(
        location_name,
        .data[[breakdown]]
      ) %>%
      arrange(
        location_name,
        cause_name
      ) %>%
      summarise(val = sum(val))

    highchart() %>%
      hc_add_series(
        data = shs,
        type = "packedbubble",
        hcaes(
          location_name,
          value = .data[[measure]],
          group = .data[[breakdown]]
        ),
        legendIndex = if (breakdown == "income_group") {
          c(4, 1, 2, 3)
        } else {
          NA
        }
      ) %>%
      hc_tooltip(
        useHTML = TRUE,
        pointFormat = "<b>{point.name}:</b> {point.value}"
      ) %>%
      hc_plotOptions(
        packedbubble = list(
          maxSize = "150%",
          zMin = 0,
          layoutAlgorithm = list(
            gravitationalConstant = 0.05,
            splitSeries = TRUE, # TRUE to group points
            seriesInteraction = FALSE,
            dragBetweenSeries = TRUE,
            parentNodeLimit = TRUE
          ),
          dataLabels = list(
            enabled = TRUE,
            format = "{point.name}",
            filter = list(
              property = "y",
              operator = ">",
              value = 250
            )
          )
        )
      ) %>%
      hc_title(
        text = paste0(
          if (isTRUE(input$shs_toggle)) "SHS" else unique(input$shs_measure_name),
          ": ",
          unique(input$shs_health_condition_name)
        ),
        font = "Econ Sans Cnd",
        useHTML = TRUE
      ) %>%
      hc_subtitle(
        text = paste0("Age: ", unique(input$shs_age_group_name), ", ", "Sex: ", unique(input$shs_sex_label), ", ", "Year: ", unique(input$shs_year_id)),
        font = "Econ Sans Cnd",
        useHTML = TRUE
      ) %>%
      hc_tooltip(valueDecimals = 0) %>%
      hc_exporting(
        enabled = FALSE, # always enabled
        filename = "custom-file-name"
      ) %>%
      hc_credits(enabled = FALSE) %>%
      hc_loading() %>%
      hc_boost(enabled = TRUE) %>%
      hc_colors(wes_palette(name = "Zissou1", n = length(unique(shs[[breakdown]])), type = "continuous")[1:length(unique(shs[[breakdown]]))])
  }

  shs_bubble <- reactive({
    viz_shs_bubble(
      shs_calculated(),
      "val",
      input$shs_year_id,
      input$breakdown
    )
  })

  output$shs_bubble <- renderHighchart({
    shs_bubble()
  })

  # map module ----

  viz_shs_map <- function(shs, measure, year, scale) {
    shs <- shs %>%
      filter(
        year_id == input$shs_year_id,
        age_group_name == input$shs_age_group_name,
        metric_name == "Rate"
      ) %>%
      group_by(location_name) %>%
      arrange(
        location_name,
        cause_name
      ) %>%
      summarise(
        val = sum(val)
      )

    hcmap(
      map = "custom/world-highres",
      data = shs,
      value = measure,
      joinBy = c(
        "name",
        "location_name"
      ),
      name = "SHS",
      download_map_data = TRUE
    ) %>%
      hc_colorAxis(
        stops = color_stops(
          colors = wes_palette("IsleofDogs2")
        ),
        type = scale,
        title = input$shs_measure_name
      ) %>%
      hc_title(
        text = paste0(
          if (isTRUE(input$shs_toggle)) "SHS" else unique(input$shs_measure_name),
          ": ",
          unique(input$shs_health_condition_name)
        ),
        font = "Econ Sans Cnd",
        useHTML = TRUE
      ) %>%
      hc_subtitle(
        text = paste0("Age: ", unique(input$shs_age_group_name), ", ", "Sex: ", unique(input$shs_sex_label), ", ", "Year: ", unique(input$shs_year_id)),
        font = "Econ Sans Cnd",
        useHTML = TRUE
      ) %>%
      hc_tooltip(valueDecimals = 3) %>%
      hc_mapNavigation(
        enabled = TRUE,
        enableMouseWheelZoom = TRUE,
        enableDoubleClickZoom = TRUE
      ) %>%
      hc_exporting(
        enabled = FALSE, # always enabled
        filename = "custom-file-name"
      ) %>%
      hc_plotOptions(
        series = list(
          point = list(
            events = list(
              click = JS("function(event) {Shiny.setInputValue('mapclick', event.point.name);}")
            )
          )
        ),
        allowSelectPoints = TRUE
      ) %>%
      hc_credits(enabled = FALSE) %>%
      hc_loading() %>%
      hc_boost(enabled = TRUE)
  }

  value <- reactive({
    # Always use "val" - shs_calculated() puts the computed value in this column
    "val"
  })


  shs_map <- reactive({
    viz_shs_map(
      shs_calculated(),
      value(),
      input$shs_year_id,
      input$radio_scale
    )
  })

  output$shs_map <- renderHighchart({
    shs_map()
  })

  # historical chart module ----
  viz_shs_history <- function(shs, type, measure, location, metric) {
    shs <- shs %>%
      filter(
        location_name %in% location,
        age_group_name == case_when(
          input$radio_indicator == "Number" & input$shs_age_group_name == "Age-standardized" ~ "All ages",
          TRUE ~ input$shs_age_group_name
        ),
        metric_name == input$radio_indicator
      ) %>%
      mutate(
        year = factor(year_id)
      ) %>%
      group_by(
        year,
        location_name
      ) %>%
      summarise(val = sum(val)) %>%
      arrange(
        location_name,
        year
      ) %>%
      dplyr::select(
        year,
        val,
        location_name
      )

    hchart(
      shs,
      type = type,
      showInLegend = TRUE,
      mapping = hcaes(
        x = as.character(year),
        y = .data[[measure]],
        group = location_name
      )
    ) %>%
      hc_title(
        text = paste0(
          if (isTRUE(input$shs_toggle)) "SHS" else unique(input$shs_measure_name),
          ": ",
          unique(input$shs_health_condition_name)
        ),
        font = "Econ Sans Cnd",
        useHTML = TRUE
      ) %>%
      hc_subtitle(
        text = paste0("Age: ", unique(input$shs_age_group_name), ", ", "Sex: ", unique(input$shs_sex_label)),
        font = "Econ Sans Cnd",
        useHTML = TRUE
      ) %>%
      hc_tooltip(valueDecimals = 3) %>%
      hc_xAxis(
        title = list(text = "Year"),
        categories = sort(unique(shs$year))
      ) %>%
      hc_exporting(
        enabled = FALSE, # always enabled
        filename = "custom-file-name"
      ) %>%
      hc_yAxis(
        title = list(
          text = case_when(
            metric == "Number" ~ "Absolute number of cases",
            metric == "Rate" ~ "Cases per 100,000 people"
          )
        )
      ) %>%
      hc_credits(enabled = FALSE) %>%
      hc_loading() %>%
      hc_boost(enabled = TRUE)
  }

  shs_historical_chart <- reactive({
    viz_shs_history(
      shs_calculated(),
      adapt_type(),
      "val",
      select_location(),
      input$radio_indicator
    )
  })

  output$shs_historical_chart <- renderHighchart({
    shs_historical_chart()
  })

  # Top Ten module ----

  viz_shs_top_ten <- function(shs, measure, year, metric) {
    shs <- shs %>%
      filter(
        age_group_name == case_when(
          input$radio_indicator == "Number" & input$shs_age_group_name == "Age-standardized" ~ "All ages",
          TRUE ~ input$shs_age_group_name
        ),
        year_id == year,
        metric_name == input$radio_indicator
      ) %>%
      group_by(location_name) %>%
      arrange(
        location_name,
        cause_name
      ) %>%
      summarise(val = sum(val)) %>%
      arrange(
        case_when(
          input$order == "top_20" ~ desc(val),
          TRUE ~ val
        )
      ) %>%
      head(20)

    hchart(
      object = shs,
      type = "bar",
      showInLegend = FALSE,
      mapping = hcaes(
        x = location_name,
        y = .data[[measure]]
      )
    ) %>%
      hc_title(
        text = paste0(
          if (isTRUE(input$shs_toggle)) "SHS" else unique(input$shs_measure_name),
          ": ",
          unique(input$shs_health_condition_name)
        ),
        font = "Econ Sans Cnd",
        useHTML = TRUE
      ) %>%
      hc_subtitle(
        text = paste0("Age: ", unique(input$shs_age_group_name), ", ", "Sex: ", unique(input$shs_sex_label), ", ", "Year: ", unique(input$shs_year_id)),
        font = "Econ Sans Cnd",
        useHTML = TRUE
      ) %>%
      hc_xAxis(
        type = "category",
        title = list(text = "Country or territory"),
        labels = list(
          align = "right",
          useHTML = TRUE,
          formatter = JS(
            "function(){
            return this.value + ' <i class=\"' + this.value.toLowerCase() + ' flag\"></i>';
            }"
          )
        )
      ) %>%
      hc_yAxis(
        title = list(
          text = case_when(
            metric == "Number" ~ "Absolute number of cases",
            metric == "Rate" ~ "Cases per 100,000 people"
          )
        )
      ) %>%
      hc_credits(enabled = FALSE) %>%
      hc_loading() %>%
      hc_exporting(
        enabled = FALSE, # always enabled
        filename = "custom-file-name"
      ) %>%
      hc_boost(enabled = TRUE)
  }

  shs_top_ten <- reactive({
    viz_shs_top_ten(
      shs_calculated(),
      "val",
      input$shs_year_id,
      input$radio_indicator
    )
  })

  output$shs_top_ten <- renderHighchart({
    shs_top_ten()
  })

  # Table module ----

  viz_shs_table_title <- function(measure, health_condition) {
    paste0(
      input$shs_measure_name,
      ": ",
      input$shs_health_condition_name
    )
  }

  shs_table_title <- reactive({
    viz_shs_table_title(
      input$shs_measure_name
    )
  })

  output$shs_table_title <- renderText({
    shs_table_title()
  })

  viz_shs_table_sub_title <- function(age, sex, year) {
    paste0(
      "Age: ",
      input$shs_age_group_name,
      ", ",
      "Sex: ",
      input$shs_sex_label,
      ", ",
      "Year: ",
      input$shs_year_id
    )
  }

  shs_table_sub_title <- reactive({
    viz_shs_table_sub_title(
      input$shs_age_group_name,
      input$shs_sex_label,
      input$shs_year_id
    )
  })

  output$shs_table_sub_title <- renderText(
    shs_table_sub_title()
  )

  viz_shs_table <- function(shs, measure, year, metric) {
    shs <- shs %>%
      filter(
        age_group_name == case_when(
          input$radio_indicator == "Number" & input$shs_age_group_name == "Age-standardized" ~ "All ages",
          TRUE ~ input$shs_age_group_name
        ),
        year_id == year,
        metric_name == input$radio_indicator
      ) %>%
      group_by(location_name) %>%
      arrange(
        location_name,
        cause_name
      ) %>%
      summarise(val = round(sum(val), digits = 3)) %>%
      datatable(
        options = list(
          pageLength = 10,
          lengthChange = FALSE
        ),
        colnames = c("Country or territory", "Value")
      )
  }

  shs_table <- reactive({
    viz_shs_table(
      shs_calculated(),
      "val",
      input$shs_year_id,
      input$radio_indicator
    )
  })

  output$shs_table <- DT::renderDataTable({
    shs_table()
  })


  n_cols <- reactive({
    length(unique(shs[[input$breakdown]]))
  })

  output$cols <- renderPrint({
    input$breakdown
  })

  iso_a2 <- reactive({
    ifelse(is.null(input$mapclick),
      mapdata %>%
        dplyr::filter(name == "Germany") %>%
        dplyr::select("iso-a2") %>%
        as.character(),
      mapdata %>%
        dplyr::filter(name == input$mapclick) %>%
        dplyr::select("iso-a2") %>%
        as.character()
    )
  })

  output$flag <- renderUI({
    flag(iso_a2())
  })

  output$condition <- reactive({
    length(select_location()) == 1
  })

  outputOptions(
    output,
    "condition",
    suspendWhenHidden = FALSE
  )

  # Clicked country
  output$country <- renderText({
    ifelse(is.null(input$mapclick),
      ip_country(),
      mapdata %>%
        dplyr::filter(name == input$mapclick) %>%
        dplyr::select("name") %>%
        as.character()
    )
  })


  output$responsive <- renderPrint({
    ifelse(
      input$isMobile,
      "grouped",
      "inline"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
