library(readr)
library(highcharter)
library(tidyr)
library(dplyr)
library(shiny)
library(shiny.semantic)
library(wesanderson)
library(shinycssloaders)
library(hrbrthemes)

ihme_data_population.R <- read_rds("ihme_data_population.rds")

# Set highcharter options
options(highcharter.theme = hc_theme_economist(tooltip = list(valueDecimals = 1)))

sexes <- as.matrix(sort(unique(as.factor(ihme_data_population.R$sex_label))))
ages <- as.matrix(sort(unique(as.factor(ihme_data_population.R$age_group_name))))
years <- as.list(sort(unique(ihme_data_population.R$year_id), decreasing = TRUE))
health_conditions <- as.list(sort(unique(ihme_data_population.R$cause_name)))

ui <- shinyUI(
  shiny.semantic::semanticPage(
    title = "Serious Health Related Suffering (SHS)",
    h1("Serious Health Related Suffering (SHS)"),
    sidebar_layout(
      shiny.semantic::sidebar_panel(
        p("Select variables:"),
        dropdown_input(
          input_id = "shs_sex_label",
          choices = sexes,
          default_text = "Sex",
          value = NULL,
          type = "selection fluid"
        ),
        dropdown_input(
          input_id = "shs_age_group_name",
          choices = ages,
          default_text = "Age group",
          value = NULL,
          type = "selection fluid"
        ),
        dropdown_input(
          input_id = "shs_year_id",
          choices = years,
          default_text = "Year",
          value = NULL,
          type = "selection fluid"
        ),
        dropdown_input(
          input_id = "shs_health_condition_name",
          choices = health_conditions,
          default_text = "Health condition",
          value = NULL,
          type = "selection fluid"
        ),
        textOutput("selected_sex"),
        textOutput("selected_age_group"),
        textOutput("selected_year"),
        textOutput("selected_health_condition")
      ),
      shiny.semantic::main_panel(
        # Output: interactive world map
        shinycssloaders::withSpinner(
          highchartOutput("map",
            height = "1000px"
          ),
          type = 8,
          color = wes_palette("Zissou1")[1],
          color.background = wes_palette("Zissou1")[3]
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$selected_year <- renderText(paste(input[["shs_year_id"]], collapse = ", "))
  output$selected_sex <- renderText(paste(input[["shs_sex_label"]], collapse = ", "))
  output$selected_age_group <- renderText(paste(input[["shs_age_group_name"]], collapse = ", "))
  output$selected_year_id <- renderText(paste(input[["shs_year_id"]], collapse = ", "))
  output$selected_health_condition <- renderText(paste(input[["shs_health_condition_name"]], collapse = ", "))




  # Load the world Map data
  worldgeojson <- download_map_data("https://code.highcharts.com/mapdata/custom/world.js") %>%
    get_data_from_map()

  # Map
  click_js <- JS("function(event) {Shiny.onInputChange('mapclick',event.point.name);}")



  output$map <- renderHighchart({
    shs <- ihme_data_population.R %>%
      filter(
        sex_label == input$shs_sex_label &
          age_group_name == input$shs_age_group_name &
          year_id == input$shs_year_id &
          cause_name == input$shs_health_condition_name &
          measure_name == "Prevalence"
      ) %>%
      select(
        location_name,
        rate
      )

    data <- reactiveValues(mapclick = NULL)

    observeEvent(input$mapclick, {
      data$mapclick <- input$mapclick
    })



    map <- hcmap(
      map = "custom/world.js",
      data = shs,
      value = "rate",
      joinBy = c("name", "location_name"),
      name = "SHS",
      download_map_data = F
    ) %>%
      hc_colorAxis(
        min = 50,
        max = 500,
        stops = color_stops(n = 10, colors = wes_palette("Zissou1"))
      ) %>%
      hc_title(
        text = "SHS",
        font = "Econ Sans Cnd",
        useHTML = TRUE
      ) %>%
      hc_subtitle(
        text = "Serious Health Related Suffering",
        font = "Econ Sans Cnd",
        color = "#22A884",
        useHTML = TRUE
      ) %>%
      hc_mapNavigation(
        enabled = TRUE,
        enableMouseWheelZoom = TRUE,
        enableDoubleClickZoom = TRUE
      ) %>%
      hc_exporting(
        enabled = TRUE, # always enabled
        filename = "custom-file-name"
      ) %>%
      hc_plotOptions(series = list(events = list(click = click_js)))
  })

  # Chart
  reactive({
    renderHighchart({
      output$chart <- hchart(
        ihme_data_population.R %>%
          filter(
            sex_label == input$shs_sex_label &
              age_group_name == input$shs_age_group_name &
              year_id == input$shs_year_id &
              cause_name == input$shs_health_condition_name &
              measure_name == "Prevalence" &
              location_name == input$mapclick
          ),
        "column",
        hcaes(
          x = year_id,
          y = rate
        )
      )
    })
  })
  # Clicked country
  output$country <- renderPrint({
    print(input$mapclick)
  })
}

shinyApp(ui = ui, server = server)
