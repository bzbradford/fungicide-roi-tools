# server.R

server <- function(input, output, session) {
  # corn module server
  crop_server("corn", corn_programs, OPTS$corn)

  # soy module server
  crop_server("soy", soy_programs, OPTS$soy)
}
