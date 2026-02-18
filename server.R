# server.R

server <- function(input, output, session) {
  # start all the module servers
  OPTS$corn$server()
  OPTS$soy$server()
  OPTS$alfalfa$server()

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
