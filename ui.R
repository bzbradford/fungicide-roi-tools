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
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(src = "script.js"),
  ),

  # Corn
  nav_panel(
    title = "Corn (Tar Spot)",
    value = "corn",
    crop_ui("corn", corn_programs, OPTS$corn)
  ),

  # Soybean
  nav_panel(
    title = "Soybean (White Mold)",
    value = "soybean",
    crop_ui("soy", soy_programs, OPTS$soy)
  ),

  # Alfalfa
  nav_panel(
    title = paste("Alfalfa"),
    value = "alfalfa",
    alfalfa_ui("alfalfa", alfalfa_programs)
  ),

  # add other crop tabs here #

  # About tab
  nav_panel(
    title = "About",
    icon = bsicons::bs_icon("question-circle"),
    card(
      card_header("About This Calculator"),
      card_body(
        includeMarkdown("data/about.md")
      )
    )
  )
)
