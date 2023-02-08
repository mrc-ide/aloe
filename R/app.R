#' App
#'
#' Launch shiny app
#'
#' @return NULL
#' @export
app <- function(){

  ui <- shiny::fluidPage(
    shiny::numericInput("N", label = "N", value = 0),
    shiny::textOutput("message"),
    shiny::actionButton("button","Update message")
  )

  server <- function(input, output, session) {

    shiny::observeEvent(input$button, {
      output$message <- shiny::renderText(paste("The number is", shiny::isolate(input$N)))
    })
  }

  shiny::shinyApp(ui, server)
}
