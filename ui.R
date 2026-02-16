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
    alfalfa_ui("alfalfa", alfalfa_programs)
  ),

  # add other crop tabs here #

  # About tab
  nav_panel(
    title = "About",
    icon = icon("circle-question"),
    card(
      card_header("About This Calculator"),
      card_body(
        includeMarkdown("data/about.md")
      )
    )
  )
)
