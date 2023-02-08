#' App
#'
#' Launch shiny app
#'
#' @return NULL
#' @export
app <- function(){

  ui <- shiny::fluidPage(

    )

  server <- function(input, output, session) {

  }

  shiny::shinyApp(ui, server)
}
