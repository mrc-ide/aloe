#' Map module UI
#'
#' @param id Intervention ID
#' @param n_strata Number of stratification levels
mapUI <- function(id, n_strata){
  shiny::tagList(
    shiny::column(
      12,
      shiny::fluidRow(
        # Map of selected spatial units
        leaflet::leafletOutput(shiny::NS(id, "map"))
      ),
      shiny::fluidRow(
        shiny::h6("Select multiple"),
        # Button to select all spatial units for given intervention
        shiny::actionButton(shiny::NS(id, "select_all"), paste0("Select all ", id)),
        # Button to select currently targeted spatial units for given intervention
        shiny::actionButton(shiny::NS(id, "select_current"), paste0("Select current ", id)),
        # Button to clear current selection of spatial units for given intervention
        shiny::actionButton(shiny::NS(id, "clear_selection"), paste0("Clear ", id)),
      ),
      shiny::fluidRow(
        shiny::h6("Stratification selection"),
        # Buttons for selecting strata
        lapply(1:n_strata, function(x){
          shiny::actionButton(shiny::NS(id, paste(x, "+")), paste0(x, "+"))
        })
      )
    )
  )
}

#' Map module server
#'
#' @param id Intervention ID
#' @param rv Reactive values
#' @param all All subunits
#' @param current Current subunits
#' @param col Intervention colour
#' @param rankings CE ranking table
#' @param n_strata Number of stratification levels
#' @param strata_selection List of selected subunits corresponding to strata selection buttons
#' @inheritParams app
mapServer <- function(id, rv, all, current, col, rankings, spatial, spatial_id, n_strata, strata_selection){

  shiny::moduleServer(id, function(input, output, session){

    spatial_sub <- spatial[spatial[[spatial_id]] %in% all[[id]],]
    bbox <- sf::st_bbox(spatial) |>
      as.vector()

    # Plot the base map
    output$map <- base_map(spatial_sub, bbox)
    shiny::outputOptions(output, "map", suspendWhenHidden = FALSE)
    pal <- leaflet::colorNumeric(c("black", col), 1:2)

    # Selecting or deselecting a clicked polygon
    shiny::observeEvent(input$map_shape_click, {
      clicked <- input$map_shape_click$id
      if(clicked %in% rv$selection[[id]]){
        rv$selection[[id]] <- setdiff(rv$selection[[id]], clicked)
      } else {
        rv$selection[[id]] <- c(rv$selection[[id]], clicked)
      }
    })
    # Select all
    shiny::observeEvent(input$select_all, {
      rv$selection[[id]] <- all[[id]]
    })
    # Select current
    shiny::observeEvent(input$select_current, {
      rv$selection[[id]] <- current[[id]]
    })
    # Clear selection
    shiny::observeEvent(input$clear_selection, {
      rv$selection[[id]] <- NULL
    })
    # Strata selection
    lapply(1:n_strata, function(x){
      shiny::observeEvent(input[[paste(x, "+")]], {
        ss <- strata_selection[[x]]
        rv$selection[[id]] <- ss[ss %in% all[[id]]]
      })
    })

    # Update the map
    fill_pd <- shiny::reactive(
      ifelse(spatial_sub[[spatial_id]] %in% rv$selection[[id]], 2, 1)
    )
    shiny::observe({
      leaflet::leafletProxy("map") |>
        leaflet::addPolygons(data = spatial_sub, stroke = TRUE, smoothFactor = 0.5,
                             opacity = 1, fill = TRUE, weight = 1,
                             label = ~spatial_sub[[spatial_id]],
                             color = ~pal(fill_pd()), layerId = ~ spatial_sub[[spatial_id]])
    })

  })
}
