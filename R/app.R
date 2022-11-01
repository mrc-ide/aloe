app <- function(){

  ui <- shiny::fluidPage(theme = shinythemes::shinytheme("slate"),
                         shiny::fluidRow(
                           shiny::column(6, mapUI("ITNs"))
                         )
  )

  server <- function(input, output, session) {
    mapServer("ITNs")
  }

  shiny::shinyApp(ui, server)
}
