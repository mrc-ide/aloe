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
app <- function(spatial = mwi, df = df_mwi, interventions = c("itn", "smc"), spatial_id = "NAME_1"){

  check_input_df(df, spatial_id)
  check_input_spatial(spatial, spatial_id)

  # Set a consistent names column as the spatial id
  spatial$spatial_id <- spatial[[spatial_id]]
  df$spatial_id <- df[[spatial_id]]
  units <- unique(df$spatial_id)

  # Where interventions are currently used
  current_matrix <- create_current_matrix(
    df = df,
    units = units,
    interventions = interventions
  )
  # Block unavailable interventions
  for(x in interventions){
    available <- unique(df[df[x] > 0 & df$scenario == "bank", "spatial_id"])
    current_matrix[!rownames(current_matrix) %in% available, x] <- NA
  }
  # Where user input selections will be recorded
  choice_matrix <- current_matrix

  # Strata matrices
  n_strata <- max(df$strata)
  strata_matrix <- create_strata_matrix(
    df,
    current_matrix
  )

  # Coverage vector
  coverage <- rep(0, length(interventions))
  names(coverage) <- interventions

  coverage_choices <- lapply(interventions, function(x){
    unique(df[df$scenario == "bank", x])
  })
  names(coverage_choices) <- interventions

  bounding_box <- as.vector(sf::st_bbox(spatial))

  mix_options <- apply(unique(df[,interventions] > 0), 1, function(x){
    if(sum(x) == 0){
      out <- "none"
    } else {
      out <- paste(interventions[x > 0], collapse = " + ")
    }
    return(out)
  })
  mo <- setdiff(mix_options, "none")
  mo <- mo[order(nchar(mo), mo)]
  mix_options <- c("none", mo)
  mix_options <- factor(mix_options, levels = mix_options)

  intervention_mix_palette <- intervention_mix_colours[1:length(mix_options)]
  names(intervention_mix_palette) <- mix_options


  ui <- shiny::navbarPage(
    "SNT modelling",

    # General theme
    theme = shinythemes::shinytheme("flatly"),

    instructionsUI,

    shiny::tabPanel(
      "Interventions",

      shiny::fluidRow(
        shiny::column(
          12,
          shiny::h4("Intervention targeting"),
          # Intervention tab font colours
          shiny::tags$style(
            shiny::HTML(
              paste0(".tabbable > .nav > li > a[data-value='", interventions, "'] {color:", intervention_colours[interventions], "}")
            )
          ),
          # Intervention slider colours
          shinyWidgets::setSliderColor(intervention_colours[interventions], 1:length(interventions)),
          do.call(
            shiny::tabsetPanel,
            lapply(
              interventions,
              function(x){
                shiny::tabPanel(
                  x, mapUI(x, n_strata, available(choice_matrix, x), coverage_choices[[x]])
                )
              }
            )
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::h4("Stratification map"),
          # Map of selected spatial units
          leaflet::leafletOutput("stratification_map"),
        ),
        shiny::column(
          6,
          shiny::h4("Intervention mix map"),
          # Map of intervention mix for spatial units
          leaflet::leafletOutput("intervention_mix_map"),
        )
      )
    ),
    shiny::tabPanel(
      "Impact",
      impactUI(),
      #shiny::downloadButton("download")
    )
  )

  server <- function(input, output, session) {

    # Initialise reactive values
    selection <- shiny::reactiveVal(choice_matrix)
    coverage <- shiny::reactiveVal(coverage)

    # Stratification map
    output$stratification_map <- leaflet::renderLeaflet({
      stratification_map(
        spatial = spatial,
        df = df,
        n_strata = n_strata,
        bbox = bounding_box
      )
    })

    # Intervention_mix map
    output$intervention_mix_map <- leaflet::renderLeaflet({
      intervention_mix_map(
        spatial = spatial,
        bbox = bounding_box,
        mix_options = mix_options,
        cols = intervention_mix_palette
      )
    })
    shiny::outputOptions(output, "intervention_mix_map", suspendWhenHidden = FALSE)

    shiny::observeEvent(selection(), {
      update_intervention_mix_map(
        selection = selection,
        spatial = spatial,
        interventions = interventions,
        bbox = bounding_box,
        mix_options = mix_options,
        cols = intervention_mix_palette
      )
    })

    # Map module
    for(i in interventions){
      mapServer(
        id = i,
        selection = selection,
        coverage = coverage,
        current_matrix = current_matrix,
        strata_matrix = strata_matrix,
        spatial = spatial[spatial[["spatial_id"]] %in% available(current_matrix, i),],
        bbox = bounding_box,
        session
      )
    }

    # Impact
    shiny::observeEvent(input$impact_button, {
      impact <- link(selection(), coverage() / 100, df)

      time_series_plot <- reactive(
        plot_time_series(
          pd = impact,
          maxy = max(df$inc)
        )
      )
      output$time_series_plot <- shiny::renderPlot(
        time_series_plot()
      )

      impact_plot <- reactive(
        plot_impact(
          pd = impact
        )
      )
      output$impact_plot <- shiny::renderPlot(
        impact_plot()
      )
      shinyjs::show("impact_plot")
      shinyjs::show("time_series_plot")
    })
    shiny::observeEvent(selection() | coverage(), {
      shinyjs::hide("time_series_plot")
      shinyjs::hide("impact_plot")
    })

    output$download <- downloadHandler(
      filename = ,
      content = function(file) {
      }
    )
  }

  shiny::shinyApp(ui, server)
}
