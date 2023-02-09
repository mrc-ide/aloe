#' App
#'
#' Launch shiny app
#'
#' @return NULL
#' @export
app <- function(interventions = c("itn", "irs")){

  cols <- map_cols(interventions)
  max_impact <- get_max_impact(df)
  max_budget <- sum(tapply(df$cost, df$NAME_1, max))

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
      shiny::column(2, shiny::numericInput("budget", "Budget", min = 0, max = max_budget, value = max_budget)),
      shiny::column(3, shiny::radioButtons("target", "Target", choices = list("Cases averted", "Deaths averted"))),
      shiny::column(2, shiny::actionButton("optimise", "Optimise"))
    )
  )

  server <- function(input, output, session) {

    # Index of all
    all <- unique(df$NAME_1)
    # List of present sub units for each intervention
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
    # To store best impact for a given budget and target
    rv$best <- max_impact
    rv$budget <- max_budget

    # Optimisation
    shiny::observeEvent(input$optimise, {
      # Run optimisation
      optim_df <- optimise(shiny::isolate(input$target), shiny::isolate(input$budget))
      # Update selection
      for(i in interventions){
        rv$selection[[i]] <- optim_df[optim_df[[i]] == 1, "NAME_1"]
      }
      rv$best <- c(sum(optim_df$cases_averted), sum(optim_df$deaths_averted))
      rv$budget <- isolate(input$budget)
    })

    # Map module
    for(i in interventions){
      mapServer(i, rv, all, current, cols[i])
    }

    # Impact
    pd <- shiny::reactive(df_pd(df, rv, interventions))
    output$impact_plot <- shiny::renderPlot(impact_plot(pd()))
  }

  shiny::shinyApp(ui, server)
}
