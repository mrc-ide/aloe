#' Map module UI
#'
#' @param id Intervention ID
#' @param n_strata Number of stratification levels
#' @param admin_units spatial spatial unit names
#' @param coverage_choices Reactive coverage choices
mapUI <- function(id, n_strata, admin_units, coverage_choices){
  col <- intervention_colours[id]
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        6,
        # Map of selected spatial units
        leaflet::leafletOutput(shiny::NS(id, "map"))
      ),
      shiny::column(
        6,
        shiny::fluidRow(
          shiny::h5("Multiple selection", style = paste0("color: ", col, ";")),
          # Button to select all spatial units for given intervention
          shiny::actionButton(shiny::NS(id, "select_all"), paste0("Select all ", id)),
          # Button to select currently targeted spatial units for given intervention
          shiny::actionButton(shiny::NS(id, "select_current"), paste0("Select current ", id)),
          # Button to clear current selection of spatial units for given intervention
          shiny::actionButton(shiny::NS(id, "clear_selection"), paste0("Clear ", id)),
        ),
        shiny::fluidRow(
          shiny::h5("Stratification selection", style = paste0("color: ", col, ";")),
          # Buttons for selecting strata
          lapply(1:n_strata, function(x){
            shiny::actionButton(shiny::NS(id, paste(x, "+")), paste0(x, "+"))
          })
        ),
        shiny::fluidRow(
          shiny::h5("List selection", style = paste0("color: ", col, ";")),
          # Dropdown to select multiple units by name
          shinyWidgets::pickerInput(
            inputId = shiny::NS(id, "select_list"),
            choices = admin_units,
            multiple = TRUE
          )
        ),
        shiny::fluidRow(
          shiny::h5("Coverage", style = paste0("color: ", col, ";")),
          shinyWidgets::sliderTextInput(
            inputId = shiny::NS(id, "coverage_slider"),
            choices = coverage_choices * 100,
            label = NULL,
            grid = TRUE,
            hide_min_max = TRUE
          )
        )
      )
    )
  )
}

#' Map module server
#'
#' @param id Intervention ID
#' @param selection Reactive values
#' @param coverage coverage reactive values
#' @param current_matrix Current implemented areas
#' @param strata_matrix Strata matrices
#' @param spatial Spatial data for intervention
#' @param bbox Map bounding box
#' @param session app session
mapServer <- function(id, selection, coverage, current_matrix, strata_matrix, spatial, bbox, session){
  shiny::moduleServer(id, function(input, output, session){
    spatial <- spatial

    # Plot the base map
    output$map <- leaflet::renderLeaflet({
      base_map(spatial, bbox)
    })
    shiny::outputOptions(output, "map", suspendWhenHidden = FALSE)
    pal <- c("black", intervention_colours[id])
    names(pal) <- NULL

    # Selecting or deselecting a clicked polygon
    shiny::observeEvent(input$map_shape_click, {
      new <- reverse(
        mat = selection(),
        intervention = id,
        units = input$map_shape_click$id
      )
      selection(new)
    })
    # Select all
    shiny::observeEvent(input$select_all, {
      new <- select(
        mat = selection(),
        intervention = id,
        units = unique(spatial$spatial_id)
      )
      selection(new)
    })
    # Select current
    shiny::observeEvent(input$select_current, {
      new <- selection()
      new[,id] <- current_matrix[,id]
      selection(new)
    })
    # Clear selection
    shiny::observeEvent(input$clear_selection, {
      new <- clear(
        mat = selection(),
        intervention = id
      )
      selection(new)
    })
    # Strata selection
    lapply(1:length(strata_matrix), function(x){
      shiny::observeEvent(input[[paste(x, "+")]], {
        new <- selection()
        new[,id] <- strata_matrix[[x]][,id]
        selection(new)
      })
    })
    # List selection
    shiny::observeEvent(input$select_list_open, {
      # Only update when list is closed
      if (!(input$select_list_open)) {
        new <- clear(
          mat = selection(),
          intervention = id
        ) |>
          select(
            intervention = id,
            units = input$select_list
          )
        selection(new)
      }
    })
    # Update selections in list in background
    shiny::observeEvent(selection(), {
      picked <- implemented(mat = selection(), intervention = id)
      if(is.null(picked)){
        picked <- ""
      }
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "select_list",
        selected = picked
      )
    })
    # Update coverage selection
    shiny::observeEvent(input$coverage_slider, {
      new_coverage <- coverage()
      new_coverage[id] <-input$coverage_slider
      coverage(new_coverage)
    },
    ignoreInit = TRUE
    )

    # Update the map
    shiny::observeEvent(selection(), {
      fill <- pal[ifelse(spatial[["spatial_id"]] %in% implemented(selection(), id), 2, 1)]
      leaflet::leafletProxy("map") |>
        leaflet::addPolygons(
          data = spatial,
          stroke = TRUE,
          smoothFactor = 0.5,
          opacity = 1,
          fill = TRUE,
          weight = 1,
          color = ~ fill,
          layerId = ~ spatial[["spatial_id"]]
        )
    })
  })
}
