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

  # Where user input selections will be recorded
  choice_matrix <- current_matrix

  n_strata <- max(df$strata)
  strata_matrix <- create_strata_matrix(
    df = df,
    units = units,
    interventions = interventions
  )

  available <- lapply(interventions, function(x){
    unique(df[df[x] ==1, "spatial_id"])
  })
  names(available) <- interventions

  bounding_box <- as.vector(sf::st_bbox(spatial))

  mix_options <- apply(unique(df[,interventions]), 1, function(x){
    if(sum(x) == 0){
      out <- "none"
    } else {
      out <- paste(interventions[x == 1], collapse = " + ")
    }
    return(out)
  })
  mix_options <- factor(mix_options, levels = mix_options)
  intervention_mix_palette <- intervention_mix_colours[1:length(mix_options)]
  names(intervention_mix_palette) <- mix_options

  cols <- map_cols(interventions)

  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("slate"),

    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h4("Intervention targeting"),
        do.call(
          shiny::tabsetPanel,
          lapply(
            interventions,
            function(x){
              shiny::tabPanel(
                x, mapUI(x, n_strata, units)
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
  )

  server <- function(input, output, session) {

    # Initialise reactive values
    rv <- shiny::reactiveVal(choice_matrix)

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
      base_map(
        spatial = spatial,
        bbox = bounding_box
      ) |>
        leaflet::addLegend(
          position = "bottomright",
          values = mix_options,
          colors = intervention_mix_palette[mix_options],
          labels = mix_options,
          opacity = 1,
          title = "Mix"
        )
    })
    shiny::observeEvent(rv(), {
      current_mix <- apply(rv(), 1, function(x){
        if(sum(x) == 0){
          out <- "none"
        } else {
          out <- paste(interventions[x], collapse = " + ")
        }
        return(out)
      })
      cols <- intervention_mix_palette[current_mix]
      names(cols) <- NULL

      leaflet::leafletProxy("intervention_mix_map") |>
        leaflet::addPolygons(
          data = spatial,
          stroke = TRUE,
          color = "black",
          smoothFactor = 0.5,
          opacity = 1,
          fill = TRUE,
          weight = 1,
          fillOpacity = 0.9,
          fillColor = ~cols,
          layerId = ~ spatial[["spatial_id"]]
        )
    })

    # Map module
    for(i in interventions){
      mapServer(
        id = i,
        rv = rv,
        current_matrix = current_matrix,
        strata_matrix = strata_matrix,
        col = cols[i],
        spatial = spatial[spatial[["spatial_id"]] %in% unlist(available[[i]]),],
        bbox = bounding_box,
        n_strata = n_strata,
        session
      )
    }

  }

  shiny::shinyApp(ui, server)
}
