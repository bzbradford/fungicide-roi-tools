# server.R

server <- function(input, output, session) {
  # start all the module servers
  crop_server("corn")
  crop_server("soy")
  alfalfa_server()

  # Show about modal ----
  observe({
    showModal(about_modal)
  }) |>
    bindEvent(input$about)
}
