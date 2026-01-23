# server.R

server <- function(input, output, session) {
  corn_server("corn", corn_programs)
}
