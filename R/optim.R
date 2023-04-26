#' Create matrix for optimisation
#'
#' @param unit Sub unit name
#' @param y Matrix input
#'
#' @return Matrix in format for optimisation using the om package
mat <- function(unit, y){
  u <- unique(unit)
  empty <- matrix(NA, nrow = length(u), ncol = max(table(unit)))
  for(i in seq_along(u)){
    r <- y[unit == u[i]]
    empty[i, 1:length(r)] <- r
  }
  return(empty)
}

#' Run optimisation
#'
#' @param target Target for optimisation, can be "cases_averted" or "deaths_averted".
#' @param budget Budget
#' @inheritParams app
optimise <- function(df, target, budget, spatial_id){
  cost_matrix <- mat(df[[spatial_id]], df$cost)
  if(target == "Cases averted"){
    z_matrix <- mat(df[[spatial_id]], df[["cases_averted"]])
  } else {
    z_matrix <- mat(df[[spatial_id]], df[["deaths_averted"]])
  }
  optimised <- om::om(z = z_matrix, cost = cost_matrix, budget = budget) |>
    as.data.frame() |>
    dplyr::select(.data$i, .data$j) |>
    dplyr::left_join(df, by = c("i", "j"))
  return(optimised)
}
