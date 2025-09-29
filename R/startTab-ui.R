library(shiny)
library(bslib)

startTab <- function(id){
  ns <- NS(id) 
  nav_panel("Start",
    h1("Welcome to Statistikker! "),
    p("This application is intended to help students gain a deeper understanding of statistics through interactive demonstrations of core concepts." ),
    p("Click on the tabs above to access the statistics topics." ),
  )
}