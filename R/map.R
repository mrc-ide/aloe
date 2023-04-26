#' Render the base map layer
#'
#' Current fixed to render NAME_1 level
#'
#' @param spatial sf data
base_map <- function(spatial){
  bbox <- sf::st_bbox(spatial) |>
    as.vector()

  leaflet::renderLeaflet({
    leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
  })
}

#' Draw map colours
#'
#' Each of up to a maximum of 8 interventions is assigned a named colour
#'
#' @param interventions Vector of interventions
#'
#' @return A colour vector
map_cols <- function(interventions){
  cols <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")[1:length(interventions)]
  names(cols) <- interventions
  return(cols)
}
