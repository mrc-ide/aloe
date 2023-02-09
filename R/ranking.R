#' Cost effectiveness ranking
#'
#' @param df Input data
#' @inheritParams app
#'
#' @return Ranking of intervention options by $ per case averted
get_ce_order <- function(df, interventions){
  option_names <- apply(df[,interventions], 1, function(x){
    paste(interventions[x == 1], collapse = " & ")
  })
  option_names[option_names == ""] <- "None"


  ranked <- df |>
    dplyr::mutate(efficiency = .data$cost / .data$cases_averted) |>
    dplyr::mutate(rank = rank(.data$efficiency, ties.method = "random"), .by = "NAME_1") |>
    dplyr::mutate(options = paste0(.data$rank, ":", option_names)) |>
    dplyr::arrange(.data$NAME_1, .data$rank) |>
    dplyr::select("NAME_1", "options")
  return(ranked)
}
