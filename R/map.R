#' Render the base map layer
#'
#' Current fixed to render NAME_1 level
#'
#' @param spatial sf data
base_map <- function(spatial){
  bbox <- sf::st_bbox(mwi) |>
    as.vector()

  leaflet::renderLeaflet({
    leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
  })
}

