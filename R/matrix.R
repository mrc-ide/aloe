init_matrix <- function(units, interventions){
  mat <- matrix(
    data = FALSE,
    nrow = length(units),
    ncol = length(interventions),
    dimnames = list(
      units,
      interventions
    )
  )
}

clear <- function(mat, intervention, units = NULL){
  stopifnot(length(intervention) == 1)
  if(is.null(units)){
    units <- rownames(mat)
  }
  mat[cbind(units, intervention)] <- FALSE
  return(mat)
}

select <- function(mat, intervention, units = NULL){
  stopifnot(length(intervention) == 1)
  if(is.null(units)){
    units <- rownames(mat)
  }
  mat[cbind(units, intervention)] <- TRUE
  return(mat)
}

reverse <- function(mat, intervention, units = NULL){
  stopifnot(length(intervention) == 1)
  if(is.null(units)){
    units <- rownames(mat)
  }
  mat[cbind(units, intervention)] <- !mat[cbind(units, intervention)]
  return(mat)
}

# Pull out names of spatial units where an intervention is implemented
implemented <- function(mat, intervention){
  rownames(mat)[mat[,intervention]]
}

# Multiply the boolean choice matrix by the coverage vector
apply_coverage <- function(rv, coverage){
  sweep(rv, 2, coverage, "*")
}

# This will include a * coverage step
link <- function(mat, coverage, df){
  cov_mat <- apply_coverage(mat, coverage)

  choice_df <- data.frame(
    spatial_id = row.names(cov_mat),
    cov_mat,
    scenario = "bank"
  )
  rownames(choice_df) <- NULL

  impact <- choice_df |>
    dplyr::left_join(df, by = colnames(choice_df)) |>
    dplyr::bind_rows(dplyr::filter(df, scenario != "bank")) |>
    dplyr::summarise(inc = mean(inc), .by = c("scenario", "year"))

  return(impact)
}

# Create the current matrix
create_current_matrix <- function(df, units, interventions){
  current_mat <- init_matrix(
    units = units,
    interventions = interventions
  )
  df_bau <- df[df$scenario == "bau", ]
  for(i in interventions){
    current_mat <- select(current_mat, i, unique(df_bau[df_bau[[i]] > 0, ][["spatial_id"]]))
  }
  return(current_mat)
}


# Create the strata matrices
create_strata_matrix <- function(df, units, interventions, available){
  n_strata <- max(df$strata)
  strata_mat <- lapply(1:n_strata, function(x){
    strata_mat <- init_matrix(
      units = units,
      interventions = interventions
    )
    for(i in interventions){
      to_select <- intersect(unique(df[df[["strata"]] >= x, ][["spatial_id"]]), available[[i]])
      strata_mat <- select(strata_mat, i, to_select)
    }
    strata_mat
  })
  return(strata_mat)
}
