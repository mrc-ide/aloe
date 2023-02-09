#' Map module UI
#'
#' @param id Intervention ID
mapUI <- function(id){
  shiny::tagList(
    shiny::fluidRow(
      # Map of selected spatial units
      leaflet::leafletOutput(shiny::NS(id, "map"))
    ),
    shiny::fluidRow(
      # Button to select all spatial units for given intervention
      shiny::actionButton(shiny::NS(id, "select_all"), paste0("Select all ", id)),
      # Button to select currently targeted spatial units for given intervention
      shiny::actionButton(shiny::NS(id, "select_current"), paste0("Select current ", id))
    )
  )
}

#' Map module server
#'
#' @param id Intervention ID
#' @param overwrite Plotting input
#' @param trigger Trigger for map update
#' @param all All subunits
#' @param current Current subunits
mapServer <- function(id, rv, all, current){

  shiny::moduleServer(id, function(input, output, session){

    # Plot the base map
    output$map <- base_map(mwi)
    shiny::outputOptions(output, "map", suspendWhenHidden = FALSE)
    pal <- leaflet::colorNumeric(c("black", "deeppink"), 1:2)


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
      # Update selection
      rv$selection[[id]] <- all
    })
    # Select current
    shiny::observeEvent(input$select_current, {
      # Update selection
      rv$selection[[id]] <- current[[id]]
    })

    # Update the map
    fill_pd <- shiny::reactive(
      ifelse(mwi$NAME_1 %in% rv$selection[[id]], 2, 1)
    )
    shiny::observe({
      leaflet::leafletProxy("map") |>
        leaflet::addPolygons(data = mwi, stroke = TRUE, smoothFactor = 0.5,
                             opacity = 1, fill = TRUE, weight = 1,
                             color = ~pal(fill_pd()), layerId = ~ NAME_1)
    })

  })
}
