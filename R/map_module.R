#' Map module UI
#'
#' @param id Intervention ID
mapUI <- function(id){
  tagList(
    shiny::actionButton(shiny::NS(id, "mapbutton"),paste0("Clear ", id)),
    shiny::plotOutput(shiny::NS(id, "impact_plot"))
  )
}

#' Map module server
#'
#' @param id Intervention ID
#' @param overwrite Plotting input
#' @param trigger Trigger for map update
#' @param all All subunits
#' @param current Current subunits
mapServer <- function(id, rv){
  shiny::moduleServer(id, function(input, output, session){
    shiny::observe({
      output$impact_plot <- shiny::renderPlot(plot(rv$selection[[id]]))
    })
    shiny::observeEvent(input$mapbutton,{
      rv$selection[[id]] <- 0
    })
  })
}
