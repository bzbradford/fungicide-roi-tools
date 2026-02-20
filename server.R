# server.R

server <- function(input, output, session) {
  # start all the module servers
  crop_server("corn")
  crop_server("soy")
  alfalfa_server()

  # Show about modal ----
  observe({
    showModal(modalDialog(
      title = "About This Calculator",
      includeMarkdown("data/about.md"),
      footer = modalButton("Close"),
      easyClose = TRUE,
      size = "l"
    ))
  }) |>
    bindEvent(input$about)
}
