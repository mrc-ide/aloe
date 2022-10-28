#' Render the base map layer
#'
#' Current fixed to render NAME_1 level
#'
#' @param spatial sf data
base_map <- function(spatial){
  leaflet::renderLeaflet({
    leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::addPolygons(data = spatial, stroke = TRUE, smoothFactor = 0.2,
                           opacity = 1, fill = TRUE, weight = 1,
                           color = "black", layerId = ~ NAME_1)
  })
}

overlap_map <- function(data, colour){
  leaflet::leafletProxy("mymap") |>
    leaflet::addPolygons(data = data, stroke = TRUE, smoothFactor = 0.2,
                         opacity = 1, fill = TRUE, weight = 1,
                         color = colour, layerId = ~ NAME_1)
}
