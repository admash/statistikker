
library(shiny)
library(bslib)


ui <- page_navbar(
                  
  startTab("start"),

  variableTypesTab("variableTypes"),
  
  distributionsTab("distributions"),

  descriptivesTab("descriptives")

)
