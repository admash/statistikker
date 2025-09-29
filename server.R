
library(shiny)

server <- function(input, output, session) {

  distributionsTabServer("distributions")

  descriptivesTabServer("descriptives")

}
