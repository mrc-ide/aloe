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

  check_input_df(df, spatial_id)
  check_input_spatial(spatial, spatial_id)

  n_strata <- max(df$strata)
  bbox <- sf::st_bbox(spatial) |>
    as.vector()

  # List of all (available) subunits for each intervention
  all <- lapply(interventions, function(x){
    options <- tapply(df[[x]], df[[spatial_id]], sum)
    names(options)[options > 0]
  })
  names(all) <- interventions

  # List of currently implemented sub units for each intervention
  current <- lapply(interventions, function(x){
    df[[spatial_id]][df$current == 1 & df[[x]] == 1]
  }
  )
  names(current) <- interventions

  # List of subunits under each strata
  strata_selection <- lapply(1:n_strata, function(x){
    unlist(unique(df[df$strata >= x, spatial_id]))
  })

  rankings <- get_ce_order(df, interventions, spatial_id)

  cols <- map_cols(interventions)

  max_impact <- get_max_impact(df, spatial_id)
  max_budget <- sum(tapply(df$cost, df[[spatial_id]], max))

  bau_impact <- get_bau_impact(df)
  bau_cost <- sum(df[df$current == 1, "cost"])

  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("slate"),

    shiny::h1("!! Prototype - outputs are not real !!"),
    shiny::fluidRow(
      shiny::tabsetPanel(
        shiny::tabPanel(
          "Instructions",
          shiny::column(
            10,
            shiny::br(),
            shiny::br(),
            shiny::h4("Instructions"),
            shiny::p("Choose where interventions are implemented by clicking on admin units in the interventions tabs."),
            shiny::p("Choose to select all admin units or select admin units where intervention is currenly used using the buttons below the map."),
            shiny::p("Hover over an admin unit to see the CE ranking of all intervention options."),
            shiny::p("Choose to optimise all interventions for a given budget, minimising cases or deaths in the Optimisation section."),
            shiny::p("View the cost and impact relative to the business as usual scenario in the outcome plot")
          )
        ),
        shiny::tabPanel(
          "Stratification",
          shiny::column(
            4,
            shiny::br(),
            shiny::br(),
            shiny::tabsetPanel(
              shiny::tabPanel(
                "Stratification map",
                # Map of selected spatial units
                leaflet::leafletOutput("stratification_map"),
                shiny::br(),
                shiny::br(),
                shiny::br(),
                shiny::br(),
                shiny::h4("Optimisation"),
                shiny::column(2, shiny::numericInput("budget", "Budget", min = 0, max = max_budget, value = max_budget)),
                shiny::column(3, shiny::radioButtons("target", "Target", choices = list("Cases averted", "Deaths averted"))),
                shiny::column(2, shiny::actionButton("optimise", "Optimise"))
              )
            )
          ),
          shiny::column(
            6,
            shiny::h4("Intervention targeting"),
            do.call(
              shiny::tabsetPanel,
              lapply(
                interventions,
                function(x){
                  shiny::tabPanel(
                    x, mapUI(x, n_strata, all[x])
                  )
                }
              )
            )
          )
        ),
        shiny::tabPanel(
          "Outcomes",
          shiny::column(
            6,
            # Outcomes plot
            shiny::h4("Outcome"),
            shiny::headerPanel(""),
            shiny::headerPanel(""),
            shiny::plotOutput("impact_plot")
          )
        )
      )
    )
  )



  server <- function(input, output, session) {

    # Initialise reactive values
    rv <- shiny::reactiveValues()
    selection <- list()
    for(i in interventions){
      selection[[i]] <- current[[i]]
    }
    rv$selection <- selection
    # To store best impact for a given budget and target
    rv$best <- max_impact
    rv$budget <- max_budget

    # Stratification map
    output$stratification_map <- leaflet::renderLeaflet({
      stratification_map(
        spatial = spatial,
        df = df,
        spatial_id = spatial_id,
        n_strata = n_strata,
        bbox = bbox)
    })

    # Optimisation
    shiny::observeEvent(input$optimise, {
      # Run optimisation
      optim_df <- optimise(df, shiny::isolate(input$target), shiny::isolate(input$budget), spatial_id)
      # Update selection
      for(i in interventions){
        rv$selection[[i]] <- optim_df[optim_df[[i]] == 1, spatial_id]
      }
      rv$best <- c(sum(optim_df$cases), sum(optim_df$deaths))
      rv$budget <- shiny::isolate(input$budget)
    })

    # Map module
    for(i in interventions){
      mapServer(i, rv, all, current, cols[i], rankings, spatial, spatial_id, n_strata, strata_selection, bbox, session)
    }

    # Impact
    pd <- shiny::reactive(df_pd(df, rv, interventions, spatial_id, bau_impact, bau_cost))
    output$impact_plot <- shiny::renderPlot(impact_plot(pd()))
  }

  shiny::shinyApp(ui, server)
}
