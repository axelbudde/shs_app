library(readr)
library(highcharter)
library(tidyr)
library(dplyr)
library(shiny)
library(shiny.semantic)
library(wesanderson)
library(shinycssloaders)
library(hrbrthemes)
library(shinyflags)

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
        "info",
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
      "600px"
    )
  ),
  mobile = list(
    areas = rbind(
      "title",
      "map",
      "user",
      "info"
    ),
    rows_height = c(
      "100px",
      "300px",
      "auto",
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
  tags$head(
    tags$style(HTML("//github.com/downloads/lafeber/world-flags-sprite/flags32.css"))
  ),
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
    ),
    info = div(
      tags$h3(
        class = "ui horizontal divider header",
        icon("bar chart"),
        "Historical country chart"
      ),
      # Clicked country
      flag(print("country")),
      h4(textOutput("country")),
      highchartOutput("chart")
    )
  )
)

server <- function(input, output, session) {
  output$selected_year <- renderText(paste(input[["shs_year_id"]], collapse = ", "))
  output$selected_sex <- renderText(paste(input[["shs_sex_label"]], collapse = ", "))
  output$selected_age_group <- renderText(paste(input[["shs_age_group_name"]], collapse = ", "))
  output$selected_year_id <- renderText(paste(input[["shs_year_id"]], collapse = ", "))
  output$selected_health_condition <- renderText(paste(input[["shs_health_condition_name"]], collapse = ", "))



  # Map
  click_js <- JS("function(event) {Shiny.onInputChange('mapclick',event.point.name);}")




  output$map <- renderHighchart({
    shs <- ihme_data_population.R %>%
      filter(
        sex_label == input$shs_sex_label &
          age_group_name == input$shs_age_group_name &
          cause_name == input$shs_health_condition_name &
          measure_name == "Prevalence"
      ) %>%
      select(
        location_name,
        rate
      )






    data <- reactiveValues(
      mapclick = NULL
    )

    observeEvent(input$mapclick, {
      data$mapclick <- input$mapclick
    })



    getContent <- function(url) {
      library(httr)
      content(GET(url))
    }

    world <- getContent("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json")
    # is text
    world <- jsonlite::fromJSON(world, simplifyVector = FALSE)

    map <- hcmap(
      map = "custom/world",
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
      hc_plotOptions(series = list(events = list(click = click_js))) %>%
      hc_credits(enabled = FALSE)
  })


  # Chart

  output$chart <- renderHighchart({
    hchart(
      ihme_data_population.R %>%
        filter(
          sex_label == input$shs_sex_label &
            age_group_name == input$shs_age_group_name &
            cause_name == input$shs_health_condition_name &
            measure_name == "Prevalence" &
            location_name == input$mapclick
        ),
      "column",
      hcaes(
        x = c(
          "1990",
          "2000",
          "2010",
          "2019"
        ),
        y = rate,
        group = location_name
      )
    ) %>%
      hc_title(text = input$shs_health_condition_name) %>%
      hc_subtitle(text = paste("Age group ", input$shs_age_group_name)) %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Prevalence")) %>%
      hc_add_theme(hc_theme_economist()) %>%
      hc_tooltip(
        valueDecimals = 3
      )
  })

  # Clicked country
  output$country <- renderText({
    input$mapclick
  })
}

shinyApp(ui = ui, server = server)
