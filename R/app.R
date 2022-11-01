app <- function(){

  all <- unique(df$NAME_1)
  current <- sample(all, 3)
  optim <- list(
    ITNs = sample(all, 10),
    IRS = sample(all, 5)
  )

  ui <- shiny::fluidPage(
    shiny::tabsetPanel(
      shiny::tabPanel("ITNs", mapUI("ITNs")),
      shiny::tabPanel("IRS",  mapUI("IRS"))
    ),
    shiny::actionButton("select_optim", "Select optim")
  )

  server <- function(input, output, session) {

    rv <- reactiveValues(variable = NULL, trigger = 0)

    # Hitting a button provides an overwrite variable
    shiny::observeEvent(input$select_optim, ignoreInit = TRUE, {
      rv$variable <- optim
      rv$trigger <- rv$trigger + 1
    })

    shiny::observe({
      mapServer("ITNs", reactive(rv$variable), reactive(rv$trigger), all, current)
      mapServer("IRS", reactive(rv$variable), reactive(rv$trigger), all, current)
    })
  }

  shiny::shinyApp(ui, server)
}
