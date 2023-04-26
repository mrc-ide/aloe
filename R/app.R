#' App
#'
#' Launch shiny app
#'
#' @param spatial The sf spatial data
#' @param df The simulation bank
#' @param interventions Vector of intervention options
#' @param spatial_id Name of unique spatial unit identifier column
#'
#' @return NULL
#' @export
app <- function(spatial, df, interventions = c("itn", "irs"), spatial_id = "NAME_1"){

  rankings <- get_ce_order(df, interventions, spatial_id)

  cols <- map_cols(interventions)

  max_impact <- get_max_impact(df, spatial_id)
  max_budget <- sum(tapply(df$cost, df[[spatial_id]], max))

  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("slate"),

    shiny::h1("!! Prototype - outputs are not real !!"),
    shiny::fluidRow(
      shiny::column(
        7,
        # Tabs for each intervention
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

      shiny::column(
        5,
        # Outcomes plot
        shiny::h4("Outcome"),
        shiny::headerPanel(""),
        shiny::headerPanel(""),
        shiny::plotOutput("impact_plot")

      ),
    ),
    # Options to optimise for a given budget
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::br(),
        shiny::br(),
        shiny::h4("Optimisation"),
        shiny::column(2, shiny::numericInput("budget", "Budget", min = 0, max = max_budget, value = max_budget)),
        shiny::column(3, shiny::radioButtons("target", "Target", choices = list("Cases averted", "Deaths averted"))),
        shiny::column(2, shiny::actionButton("optimise", "Optimise"))
      ),
      shiny::column(
        6,
        shiny::br(),
        shiny::br(),
        shiny::h4("Instructions"),
        shiny::p("Choose where interventions are implemented by clicking on admin units in the interventions tabs."),
        shiny::p("Choose to select all admin units or select admin units where intervention is currenly used using the buttons below the map."),
        shiny::p("Hover over an admin unit to see the CE ranking of all intervention options."),
        shiny::p("Choose to optimise all interventions for a given budget, minimising cases or deaths in the Optimisation section."),
        shiny::p("View the cost and impact (as a % of maximum impact for the budget in the outcome plot")
      )
    )
  )


  server <- function(input, output, session) {

    # Index of all
    all <- unique(df[[spatial_id]])
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
      optim_df <- optimise(df, shiny::isolate(input$target), shiny::isolate(input$budget), spatial_id)
      # Update selection
      for(i in interventions){
        rv$selection[[i]] <- optim_df[optim_df[[i]] == 1, spatial_id]
      }
      rv$best <- c(sum(optim_df$cases_averted), sum(optim_df$deaths_averted))
      rv$budget <- shiny::isolate(input$budget)
    })

    # Map module
    for(i in interventions){
      mapServer(i, rv, all, current, cols[i], rankings, spatial, spatial_id)
    }

    # Impact
    pd <- shiny::reactive(df_pd(df, rv, interventions, spatial_id))
    output$impact_plot <- shiny::renderPlot(impact_plot(pd()))
  }

  shiny::shinyApp(ui, server)
}
