#' App
#'
#' Launch shiny app
#'
#' @return NULL
#' @export
app <- function(interventions = c("itn", "irs")){

  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("slate"),

    shiny::fluidRow(
      # Tabs for each intervention
      shiny::column(
        8,
        shiny::h4("Interventions"),
        do.call(
          shiny::tabsetPanel,
          lapply(
            interventions,
            function(x){
              shiny::tabPanel(
                x, mapUI(x)
              )
            }
          )
        )
      ),
      # Outcomes plot
      shiny::column(
        4,
        shiny::h4("Outcome"),
        shiny::headerPanel(""),
        shiny::headerPanel(""),
        shiny::plotOutput("impact_plot")
      )
    ),
    # Options to optimise for a given budget
    shiny::fluidRow(
      shiny::h4("Optimisation"),
      shiny::column(2, shiny::numericInput("budget", "Budget", min = 0, max = 100000, value = 100000)),
      shiny::column(3, shiny::radioButtons("target", "Target", choices = list("Cases averted", "Deaths averted"))),
      shiny::column(2, shiny::actionButton("optimise", "Optimise"))
    )
  )

  server <- function(input, output, session) {

    all <- unique(df$NAME_1)
    current <- list(
      itn = sample(all, 6),
      irs = sample(all, 3)
    )

    # Initialise reactive values
    rv <- shiny::reactiveValues()
    selection <- list()
    for(i in interventions){
      selection[[i]] <- NULL
    }
    rv$selection <- selection

    # Optimisation
    shiny::observeEvent(input$optimise, {
      # Run optimisation
      optim_df <- optimise(shiny::isolate(input$target), shiny::isolate(input$budget))
      # Update selection
      for(i in interventions){
        rv$selection[[i]] <- optim_df[optim_df[[i]] == 1, "NAME_1"]
      }
    })

    mapServer("itn", rv, all, current)
    #mapServer("irs", rv, all, current)
  }

  shiny::shinyApp(ui, server)
}
