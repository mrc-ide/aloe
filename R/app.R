app <- function(){

  all <- unique(df$NAME_1)
  current <- sample(all, 3)

  best_cases <- optimise("Cases averted", sum(df$cost))
  best_deaths <- optimise("Deaths averted", sum(df$cost))

  ui <- shiny::fluidPage(
    shiny::h4("Interventions"),
    shiny::fluidRow(
      column(8,
             shiny::tabsetPanel(
               shiny::tabPanel("ITNs", mapUI("ITNs")),
               shiny::tabPanel("IRS",  mapUI("IRS"))
             )),
      column(4,

      )
    ),
    shiny::fluidRow(
      shiny::h4("Optimisation"),
      shiny::column(4, shiny::numericInput("budget", "Budget", min = 0, max = 10000, value = 10000)),
      shiny::column(4, shiny::radioButtons("target", "Target", choices = list("Cases averted", "Deaths averted"))),
      shiny::column(4, shiny::actionButton("optimise", "Optimise"))
    )
  )

  server <- function(input, output, session) {

    rv <- shiny::reactiveValues(variable = NULL, trigger = 0, target = NULL, budget = NULL)

    shiny::observeEvent(input$budget, {
      rv$budget <- input$budget
    })
    shiny::observeEvent(input$target, {
      rv$target <- input$target
    })
    shiny::observeEvent(input$optimise, ignoreInit = TRUE, {
      optim_df <- optimise(rv$target, rv$budget)
      rv$variable$ITNs <- optim_df[optim_df$itn == 1, "NAME_1"]
      rv$variable$IRS <- optim_df[optim_df$irs == 1, "NAME_1"]
      rv$trigger <- rv$trigger + 1
    })

    shiny::observe({
      mapServer("ITNs", reactive(rv$variable), reactive(rv$trigger), all, current)
      mapServer("IRS", reactive(rv$variable), reactive(rv$trigger), all, current)
    })
  }

  shiny::shinyApp(ui, server)
}
