# server.R

server <- function(input, output, session) {
  # corn module server
  # crop_server("corn", corn_programs, OPTS$corn)
  OPTS$corn$server()

  # soy module server
  OPTS$soy$server()

  # alfalfa module server
  alfalfa_server("alfalfa", alfalfa_programs)
}
