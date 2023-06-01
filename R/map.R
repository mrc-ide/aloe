#' Render the base map layer
#'
#' Current fixed to render NAME_1 level
#'
#' @param spatial sf data
#' @param bbox BOunding box extent
base_map <- function(spatial, bbox){
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

#' Create strata map
#'
#' @param spatial The sf spatial data
#' @param df The simulation bank
#' @param spatial_id Name of unique spatial unit identifier column
#' @param n_strata Number of strata
#' @param bbox Bounding box for map
stratification_map <- function(spatial, df, spatial_id, n_strata, bbox){
  stratification_data <- spatial |>
    dplyr::left_join(unique(df[,c(spatial_id, "strata")]), by = spatial_id) |>
    dplyr::mutate(strata = factor(.data$strata))
  stratification_palette <- leaflet::colorFactor(stratification_colours, 1:n_strata)

  leaflet::leaflet() |>
    leaflet::addTiles()  |>
    leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) |>
    leaflet::addPolygons(
      data = stratification_data,
      stroke = TRUE,
      smoothFactor = 0.5,
      opacity = 1,
      fill = TRUE,
      weight = 1,
      fillOpacity = 0.9,
      fillColor = ~stratification_palette(stratification_data$strata),
      layerId = ~ stratification_data[[spatial_id]]
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      values = factor(1:n_strata),
      pal = stratification_palette,
      opacity = 1,
      title = "Strata"
    )

}
