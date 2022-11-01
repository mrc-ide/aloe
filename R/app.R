app <- function(){
  ui <- shiny::fluidPage(
    shiny::tabsetPanel(
      shiny::tabPanel("ITNs",
                      mapUI("ITNs")
      ),
      shiny::tabPanel("IRS",
                      mapUI("IRS")
      )
    )
  )

  server <- function(input, output, session) {
    mapServer("ITNs")
    mapServer("IRS")
  }

  shiny::shinyApp(ui, server)
}
