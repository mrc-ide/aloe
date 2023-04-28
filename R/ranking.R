#' Cost effectiveness ranking
#'
#' @param df Input data
#' @inheritParams app
#'
#' @return Ranking of intervention options by $ per case averted
get_ce_order <- function(df, interventions, spatial_id){
  option_names <- apply(df[,interventions], 1, function(x){
    paste(interventions[x == 1], collapse = " & ")
  })
  option_names[option_names == ""] <- "None"

  ranked <- df |>
    dplyr::mutate(efficiency = .data$cases / .data$cost) |>
    dplyr::mutate(rank = rank(.data$efficiency, ties.method = "random"), .by = spatial_id) |>
    dplyr::mutate(options = paste0(.data$rank, ":", option_names)) |>
    dplyr::arrange(.data[[spatial_id]], .data$rank) |>
    dplyr::select(spatial_id, "options")
  return(ranked)
}
