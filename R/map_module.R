#' Map module UI
#'
#' @param id Intervention ID
#' @param n_strata Number of stratification levels
#' @param admin_units spatial spatial unit names
mapUI <- function(id, n_strata, admin_units){
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
          shiny::h5("Multiple selection"),
          # Button to select all spatial units for given intervention
          shiny::actionButton(shiny::NS(id, "select_all"), paste0("Select all ", id)),
          # Button to select currently targeted spatial units for given intervention
          shiny::actionButton(shiny::NS(id, "select_current"), paste0("Select current ", id)),
          # Button to clear current selection of spatial units for given intervention
          shiny::actionButton(shiny::NS(id, "clear_selection"), paste0("Clear ", id)),
        ),
        shiny::fluidRow(
          shiny::h5("Stratification selection"),
          # Buttons for selecting strata
          lapply(1:n_strata, function(x){
            shiny::actionButton(shiny::NS(id, paste(x, "+")), paste0(x, "+"))
          })
        ),
        shiny::fluidRow(
          shiny::h5("List selection"),
          # Dropdown to select multiple units by name
          shinyWidgets::pickerInput(
            inputId = shiny::NS(id, "select_list"),
            choices = admin_units,
            multiple = TRUE
          )
        )
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
#' @param bbox Map bounding box
#' @param session app session
#' @inheritParams app
mapServer <- function(id, rv, current_matrix, strata_matrix, col, spatial, bbox, n_strata, session){
  shiny::moduleServer(id, function(input, output, session){
    spatial <- spatial

    # Plot the base map
    output$map <- leaflet::renderLeaflet({
      base_map(spatial, bbox)
    })
    shiny::outputOptions(output, "map", suspendWhenHidden = FALSE)
    pal <- c("black", col)
    names(pal) <- NULL

    # Selecting or deselecting a clicked polygon
    shiny::observeEvent(input$map_shape_click, {
      new <- reverse(
        mat = rv(),
        intervention = id,
        units = input$map_shape_click$id
      )
      rv(new)
    })
    # Select all
    shiny::observeEvent(input$select_all, {
      new <- select(
        mat = rv(),
        intervention = id
      )
      rv(new)
    })
    # Select current
    shiny::observeEvent(input$select_current, {
      new <- rv()
      new[,id] <- current_matrix[,id]
      rv(new)
    })
    # Clear selection
    shiny::observeEvent(input$clear_selection, {
      new <- clear(
        mat = rv(),
        intervention = id
      )
      rv(new)
    })
    # Strata selection
    lapply(1:n_strata, function(x){
      shiny::observeEvent(input[[paste(x, "+")]], {
        new <- rv()
        new[,id] <- strata_matrix[[x]][,id]
        rv(new)
      })
    })
    # List selection
    shiny::observeEvent(input$select_list_open, {
      # Only update when list is closed
      if (!(input$select_list_open)) {
        new <- clear(
          mat = rv(),
          intervention = id
        ) |>
          select(
            intervention = id,
            units = input$select_list
          )
        rv(new)
      }
    })
    # Update selections in list in background
    shiny::observeEvent(rv(), {
      picked <- implemented(mat = rv(), intervention = id)
      if(is.null(picked)){
        picked <- ""
      }
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "select_list",
        selected = picked
      )
    })

    # Update the map
    shiny::observeEvent(rv(), {
      fill <- pal[ifelse(spatial[["spatial_id"]] %in% implemented(rv(), id), 2, 1)]
      leaflet::leafletProxy("map") |>
        leaflet::addPolygons(
          data = spatial,
          stroke = TRUE,
          smoothFactor = 0.5,
          opacity = 1,
          fill = TRUE,
          weight = 1,
          color = ~fill,
          layerId = ~ spatial[["spatial_id"]]
        )
    })
  })
}
