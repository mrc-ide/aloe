#' Select current solution from simulation bank
#'
#' @param df Simulation bank
#' @param rv Reactive values
#' @param bau_impact BAU impact
#' @param bau_cost BAU cost
#' @inheritParams app
#'
#' @return Impact plot data
df_pd <- function(df, rv, interventions, spatial_id, bau_impact, bau_cost){
  out <- data.frame(
    x = unique(df[[spatial_id]])
  )
  colnames(out) <- spatial_id

  for(i in interventions){
    out[,i] <- ifelse(out[[spatial_id]] %in% rv$selection[[i]], 1, 0)
  }
  out <- out |>
    dplyr::left_join(df, by = c(spatial_id, interventions))

  cases <- sum(out$cases)
  cases_change <- 100 * ((cases - bau_impact["cases"]) / bau_impact["cases"])
  deaths <- sum(out$deaths)
  deaths_change <- 100 * ((deaths - bau_impact["deaths"]) / bau_impact["deaths"])
  cost <- sum(out$cost)
  cost_change <- 100 * ((cost - bau_cost) / bau_cost)

  pd <- data.frame(x = c(
    "Budget",
    "Cases",
    "Deaths"),
    y = c(
      cost_change,
      cases_change,
      deaths_change
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
  min_cases <- sum(tapply(df$cases, df[[spatial_id]], min))
  min_deaths <- sum(tapply(df$deaths, df[[spatial_id]], min))
  return(c(min_cases, min_deaths))
}

#' Current impact (BAU)
#'
#' @param df Input data
#'
#' @return Vector of maximum cases and deaths averted
get_bau_impact <- function(df){
  df_bau <- df[df$current == 1, ]
  bau_cases <- sum(df_bau$cases)
  bau_deaths <- sum(df_bau$deaths)
  return(c(cases = bau_cases, deaths = bau_deaths))
}

