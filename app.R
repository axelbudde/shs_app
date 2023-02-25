library(readr)
library(highcharter)
library(tidyr)
library(dplyr)
library(shiny)
library(shiny.semantic)
library(wesanderson)
library(shinycssloaders)
library(hrbrthemes)

myGridTemplate <- grid_template(
  default = list(
    areas = rbind(
      c(
        "title",
        "map"
      ),
      c(
        "user",
        "map"
      ),
      c(
        "user",
        "map"
      )
    ),
    cols_width = c(
      "500px",
      "1fr"
    ),
    rows_height = c(
      "100px",
      "auto",
      "300px"
    )
  ),
  mobile = list(
    areas = rbind(
      "title",
      "map",
      "user"
    ),
    rows_height = c(
      "100px",
      "300px",
      "auto"
    ),
    cols_width = c("100%")
  )
)

ihme_data_population.R <- read_rds("ihme_data_population.rds")

# Set highcharter options
options(highcharter.theme = hc_theme_economist(tooltip = list(valueDecimals = 3)))

sexes <- as.matrix(sort(unique(as.factor(ihme_data_population.R$sex_label)), decreasing = TRUE))
ages <- as.matrix(sort(unique(as.factor(ihme_data_population.R$age_group_name))))
years <- as.list(sort(unique(ihme_data_population.R$year_id), decreasing = TRUE))
health_conditions <- as.list(sort(unique(ihme_data_population.R$cause_name)))

ui <- semanticPage(
  suppress_bootstrap = TRUE,
  margin = "20px",
  grid(
    myGridTemplate,
    area_styles = list(
      title = "margin: 10px",
      user = "margin: 10px",
      info = "margin: 10px",
      map = "margin: 10px"
    ),
    title = header(
      title = "Serious Health Related Suffering",
      description = "SHS",
      icon = "procedures"
    ),
    map = highchartOutput(
      "map",
      height = "95%"
    ),
    user = div(
      tags$h3(
        class = "ui horizontal divider header",
        icon("cog icon"),
        "Settings"
      ),
      tags$h4(
        class = "ui header",
        icon("filter"),
        "Filter variables"
      ),
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
      div(class = "ui divider"),
      tags$h4(
        class = "ui header",
        icon("ruler horizontal icon"),
        div(
          class = "content",
          "Select scale type"
        )
      ),
      form(
        class = "ui form",
        field(
          class = "inline fields",
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
        )
      ),
      div(class = "ui divider")
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

    observeEvent(input$radio_scale, {
      data$radio_scale <- input$radio_scale
    })



    map <- hcmap(
      map = "custom/world.js",
      data = shs,
      value = "rate",
      joinBy = c("name", "location_name"),
      name = "SHS",
      download_map_data = FALSE
    ) %>%
      hc_colorAxis(
        stops = color_stops(
          colors = wes_palette("Zissou1")
        ),
        type = input$radio_scale,
        addTitle = "Prevalence"
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
