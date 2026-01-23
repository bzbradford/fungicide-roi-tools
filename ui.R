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
    title = paste("🌽 Corn"),
    value = "corn",
    corn_ui()
  ),

  # Soybean
  nav_panel(
    title = paste("🫘 Soybean"),
    value = "soybean",
    p("Coming soon")
  ),

  # Alfalfa
  nav_panel(
    title = paste("🌿 Alfalfa"),
    value = "alfalfa",
    p("Coming soon")
  ),

  # add other crop tabs here #

  # About tab
  nav_panel(
    title = "About",
    icon = bsicons::bs_icon("question-circle"),
    card(
      card_header("About This Calculator"),
      card_body(
        h4("Fungicide ROI Calculator"),
        p(
          "This tool helps farmers and agronomists evaluate the economic potential of fungicide applications for field crops."
        ),

        h5("How to Use"),
        tags$ol(
          tags$li("Select your crop using the tabs at the top"),
          tags$li("Enter your expected yield and sale price"),
          tags$li("Select the anticipated disease severity level"),
          tags$li("Adjust treatment costs if different from defaults"),
          tags$li("Review the results in the chart and data table")
        ),

        h5("Interpreting Results"),
        tags$ul(
          tags$li(
            "Programs are ranked by expected net benefit (highest first)"
          ),
          tags$li("Blue points indicate the top 3 recommended programs"),
          tags$li("Error bars show the 95% confidence interval"),
          tags$li("Negative values (shaded area) indicate expected loss")
        ),

        h5("Data Sources"),
        p(
          "Economic parameters are derived from multi-year, multi-location field trials conducted across the Midwest."
        ),

        hr(),
        p(class = "text-muted", "Version 2.0 | Built with R Shiny and bslib")
      )
    )
  )
)
