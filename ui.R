# ui.R

ui <- page_navbar(
  title = "Fungicide ROI Calculator",
  id = "nav",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#006939",
    "navbar-bg" = "#006939"
  ),
  fillable = TRUE,
  header = tags$head(
    tags$link(
      rel = "shortcut icon",
      href = "seedbag.png",
      type = "image/png"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(src = "script.js"),
  ),

  # Corn
  nav_panel(
    title = "Corn",
    value = "corn",
    icon = icon("seedling"),
    # crop_ui("corn", corn_programs, OPTS$corn)
    OPTS$corn$ui()
  ),

  # Soybean
  nav_panel(
    title = "Soybean (White Mold)",
    value = "soybean",
    icon = icon("leaf"),
    OPTS$soy$ui()
  ),

  # Alfalfa
  nav_panel(
    title = "Alfalfa",
    value = "alfalfa",
    icon = icon("pagelines"),
    OPTS$alfalfa$ui()
  ),

  # add other crop tabs here #

  nav_spacer(),
  nav_item(
    actionLink(
      inputId = "about",
      label = "More information",
      icon = icon("circle-question"),
      style = "color: white; text-decoration: none; font-size: medium;"
    )
  )
)
