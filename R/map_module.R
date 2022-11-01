mapUI <- function(id){
  shiny::tagList(
    leaflet::leafletOutput(NS(id, "map"))
  )
}

mapServer <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    rv <- shiny::reactiveValues()
    rv$selection <- NULL
    rv$clicked <- NULL
    rv$colour <- NULL

    output$map <- base_map(mwi)

    shiny::observeEvent(input$map_shape_click, {
      rv$clicked <- input$map_shape_click$id
      # Selecting or deselecting a polygon
      if(rv$clicked %in% rv$selection){
        rv$selection <- setdiff(rv$selection, rv$clicked)
        rv$colour <- "black"
      } else {
        rv$selection <- c(rv$selection, rv$clicked)
        rv$colour <- "deeppink"
      }
    })

    map <- shiny::reactive({
      dplyr::filter(mwi, NAME_1 %in% rv$clicked)
    })

    shiny::observe({
      overlap_map(leaflet::leafletProxy("map"), data = map(), colour = rv$colour)
    })

  })
}
