library(shiny.semantic)

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
      "300px"
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

display_grid(myGridTemplate)
