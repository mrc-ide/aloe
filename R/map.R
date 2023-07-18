#' Render the base map layer
#'
#' @param spatial sf data
#' @param bbox Bounding box extent
base_map <- function(spatial, bbox){
  leaflet::leaflet() |>
    leaflet::addTiles() |>
    leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
}

#' Create strata map
#'
#' @param spatial The sf spatial data
#' @param df The simulation bank
#' @param n_strata Number of strata
#' @param bbox Bounding box for map
stratification_map <- function(spatial, df, n_strata, bbox){
  stratification_data <- spatial |>
    dplyr::left_join(unique(df[,c("spatial_id", "strata")]), by = "spatial_id") |>
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
      layerId = ~ stratification_data[["spatial_id"]]
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      values = factor(1:n_strata),
      pal = stratification_palette,
      opacity = 1,
      title = "Strata"
    )
}

#' Create the intervention mix map
#'
#' @param spatial The sf spatial data
#' @param bbox Bounding box for map
#' @param mix_options Vector of possible intervention mixes
#' @param cols Vector of mix option colours
intervention_mix_map <- function(spatial, bbox, mix_options, cols){
  base_map(
    spatial = spatial,
    bbox = bbox
  ) |>
    leaflet::addLegend(
      position = "bottomright",
      values = mix_options,
      colors = cols[mix_options],
      labels = mix_options,
      opacity = 1,
      title = "Mix"
    )
}

#' Update the intervention mix map
#'
#' @param selection The reactive choice matrix
#' @param spatial The sf spatial data
#' @param interventions Interventions vector
#' @param bbox Bounding box for map
#' @param mix_options Vector of possible intervention mixes
#' @param cols Vector of mix option colours
update_intervention_mix_map <- function(selection, spatial, interventions, bbox, mix_options, cols){

  current_mix <- apply(selection(), 1, function(x){
    if(sum(x, na.rm = TRUE) == 0){
      out <- "none"
    } else {
      out <- paste(interventions[x & !is.na(x)], collapse = " + ")
    }
    return(out)
  })
  cols <- cols[current_mix]
  names(cols) <- NULL

  leaflet::leafletProxy("intervention_mix_map") |>
    leaflet::addPolygons(
      data = spatial,
      stroke = TRUE,
      color = "black",
      smoothFactor = 0.5,
      opacity = 1,
      fill = TRUE,
      weight = 1,
      fillOpacity = 0.9,
      fillColor = ~cols,
      layerId = ~ spatial[["spatial_id"]]
    )
}

