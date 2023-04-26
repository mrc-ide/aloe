#' Select current solution from simulation bank
#'
#' @param df Simulation bank
#' @param rv Reactive values
#' @inheritParams app
#'
#' @return Impact plot data
df_pd <- function(df, rv, interventions, spatial_id){
  out <- data.frame(
    x = unique(df[[spatial_id]])
  )
  colnames(out) <- spatial_id

  for(i in interventions){
    out[,i] <- ifelse(out[[spatial_id]] %in% rv$selection[[i]], 1, 0)
  }
  out <- out |>
    dplyr::left_join(df, by = c(spatial_id, interventions))

  cases_averted <- sum(out$cases_averted)
  cases_averted_pc <- 100 * (cases_averted / rv$best[1])
  deaths_averted <- sum(out$deaths_averted)
  deaths_averted_pc <- 100 * (deaths_averted / rv$best[2])
  cost <- sum(out$cost)
  cost_pc <- 100 * (cost / rv$budget)

  pd <- data.frame(x = c(
    "Budget\nspent",
    "Cases\naverted",
    "Deaths\naverted"),
    y = c(
      cost_pc,
      cases_averted_pc,
      deaths_averted_pc
    )
  )
  return(pd)
}

#' Maximum impact possible
#'
#' @param df Input data
#' @inheritParams app
#'
#' @return Vector of maximum cases and deaths averted
get_max_impact <- function(df, spatial_id){
  max_cases_averted <- sum(tapply(df$cases_averted, df[[spatial_id]], max))
  max_deaths_averted <- sum(tapply(df$deaths_averted, df[[spatial_id]], max))
  return(c(max_cases_averted, max_deaths_averted))
}

