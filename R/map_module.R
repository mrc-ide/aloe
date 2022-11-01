mapUI <- function(id){
  shiny::tagList(
    leaflet::leafletOutput(NS(id, "map")),
    shiny::actionButton(NS(id, "select_all"), paste0("Select all ", id)),
    shiny::actionButton(NS(id, "select_current"), paste0("Select current ", id))
  )
}

mapServer <- function(id, overwrite, trigger, all, current){

  shiny::moduleServer(id, function(input, output, session){
    rv <- shiny::reactiveValues()
    rv$selection <- NULL
    rv$clicked <- NULL
    rv$colour <- NULL
    rv$to_overwrite <- NULL
    rv$overwrite_trigger <- 0

    output$map <- base_map(mwi)
    outputOptions(output, "map", suspendWhenHidden = FALSE)

    # Selecting or deselecting a polygon
    shiny::observeEvent(input$map_shape_click, {
      rv$clicked <- input$map_shape_click$id
      if(rv$clicked %in% rv$selection){
        rv$selection <- setdiff(rv$selection, rv$clicked)
        rv$colour <- "black"
      } else {
        rv$selection <- c(rv$selection, rv$clicked)
        rv$colour <- "deeppink"
      }
    })

    # Optimum choosen (from app)
    shiny::observeEvent(trigger(), ignoreInit = TRUE,  {
      rv$to_overwrite <- overwrite()
      rv$overwrite_trigger <- rv$overwrite_trigger + 1
    })
    # All choosen
    shiny::observeEvent(input$select_all, {
      rv$to_overwrite <- all
      rv$overwrite_trigger <- rv$overwrite_trigger + 1
    })
    # Current choosen
    shiny::observeEvent(input$select_current, {
      rv$to_overwrite <- current
      rv$overwrite_trigger <- rv$overwrite_trigger + 1
    })

    shiny::observeEvent(rv$overwrite_trigger, {
      # Clear anything not in the new selection
      to_clear <- dplyr::filter(mwi, NAME_1 %in% setdiff(rv$selection, rv$to_overwrite))
      overlap_map(leaflet::leafletProxy("map"), data = to_clear, colour = "black")
      # Highlight anything missing from new selection
      rv$clicked <- setdiff(rv$to_overwrite, rv$selection)
      rv$colour <- "deeppink"
      # Update new selection
      rv$selection <- rv$to_overwrite
    })

    map <- shiny::reactive({
      dplyr::filter(mwi, NAME_1 %in% rv$clicked)
    })

    shiny::observe({
      overlap_map(leaflet::leafletProxy("map"), data = map(), colour = rv$colour)
    })

  })
}
