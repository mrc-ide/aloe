#' App
#'
#' Launch shiny app
#'
#' @return NULL
#' @export
app <- function(){

  ui <- shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::h4("Outcome"),
        shiny::actionButton("button","Optimise!"),
        shiny::textOutput("message"),
        shiny::plotOutput("Total_plot")
      )
    ),
    shiny::fluidRow(
      # Tabs for each intervention
      shiny::column(
        3,
        shiny::h4("Tabs"),
        shiny::tabsetPanel(
          shiny::tabPanel("N1", mapUI("N1")),
          shiny::tabPanel("N2", mapUI("N2"))
        )
      )
    )
  )

  server <- function(input, output, session) {

    # A trigger to flag a new optimisation
    rv <- shiny::reactiveValues(
      selection = list(N1 = 0, N2 = 0)
    )

    shiny::observeEvent(input$button, {
      # On button click we run optimisation and update current selection
      rv$selection$N1 <- 101
      rv$selection$N2 <- 102
    })

    # Named tab elements can use and modify reactive values
    mapServer("N1", rv)
    mapServer("N2", rv)

    # Print current selection
    output$message <- shiny::renderText(paste0("The selections are ", rv$selection$N1, ", ", rv$selection$N2))
    # Plot current total
    output$Total_plot <- shiny::renderPlot(plot(sum(unlist(rv$selection))))
  }

  shiny::shinyApp(ui, server)
}
