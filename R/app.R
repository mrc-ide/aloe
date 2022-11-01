app <- function(){

  ui <- shiny::fluidPage(theme = shinythemes::shinytheme("slate"),
                         shiny::fluidRow(
                           shiny::column(6, mapUI("ITNs"))
                         ),
                         shiny::fluidRow(
                           shiny::column(6, mapUI("IRS"))
                         )
  )

  server <- function(input, output, session) {
    mapServer("ITNs")
    mapServer("IRS")
  }

  shiny::shinyApp(ui, server)
}
