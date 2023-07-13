#' Initialise a spatial unit x interventions boolean matrix
#'
#' @param units Vector of spatial unit names
#' @param interventions Vector of intervention names
#'
#' @return Boolean matrix
init_matrix <- function(units, interventions){
  stopifnot(length(units) > 0)
  stopifnot(length(interventions) > 0)
  stopifnot(is.character(units))
  stopifnot(is.character(interventions))

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

#' Clear the intervention matrix for selected intervention and spatial unit
#' combinations
#' @param mat choice matrix
#' @param intervention Intervention name
#' @param units Vector of spatial unit names
clear <- function(mat, intervention, units = NULL){
  stopifnot(length(intervention) == 1)
  if(is.null(units)){
    units <- rownames(mat)
  }
  mat[cbind(units, intervention)] <- FALSE
  return(mat)
}

#' Select the intervention matrix for selected intervention and spatial unit
#' combinations
#' @param mat choice matrix
#' @param intervention Intervention name
#' @param units Vector of spatial unit names
select <- function(mat, intervention, units = NULL){
  stopifnot(length(intervention) == 1)
  if(is.null(units)){
    units <- rownames(mat)
  }
  mat[cbind(units, intervention)] <- TRUE
  return(mat)
}

#' Reverse the intervention matrix for selected intervention and spatial unit
#' combinations
#'
#' @param mat choice matrix
#' @param intervention Intervention name
#' @param units Vector of spatial unit names
reverse <- function(mat, intervention, units = NULL){
  stopifnot(length(intervention) == 1)
  if(is.null(units)){
    units <- rownames(mat)
  }
  mat[cbind(units, intervention)] <- !mat[cbind(units, intervention)]
  return(mat)
}

#' Pull out names of spatial units where an intervention is implemented
#' @param mat choice matrix
#' @param intervention Intervention name
implemented <- function(mat, intervention){
  rownames(mat)[mat[,intervention] & !is.na(mat[,intervention])]
}

#' Pull out names of spatial units where an intervention is available
#' @param mat choice matrix
#' @param intervention Intervention name
available <- function(mat, intervention){
  rownames(mat)[!is.na(mat[,intervention])]
}

#' Multiply the boolean choice matrix by the coverage vector
#' @param rv Reactive coverage vector
#' @param coverage Reactive choice matrix
apply_coverage <- function(rv, coverage){
  sweep(rv, 2, coverage, "*")
}

# Link the choice matrix to the input data.frame
link <- function(mat, coverage, df){
  cov_mat <- apply_coverage(mat, coverage)

  choice_df <- data.frame(
    spatial_id = row.names(cov_mat),
    cov_mat,
    scenario = "bank"
  )
  rownames(choice_df) <- NULL
  choice_df[is.na(choice_df)] <- 0

  impact <- choice_df |>
    dplyr::left_join(df, by = colnames(choice_df)) |>
    dplyr::bind_rows(dplyr::filter(df, .data$scenario != "bank")) |>
    dplyr::summarise(inc = mean(.data$inc), .by = c("scenario", "year"))

  return(impact)
}

# Create the matrix of currently implemented areas
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
create_strata_matrix <- function(df, current_matrix){
  n_strata <- max(df$strata)
  interventions <- colnames(current_matrix)
  units <- rownames(current_matrix)

  strata_mat <- lapply(1:n_strata, function(x){
    strata_mat <- init_matrix(
      units = units,
      interventions = interventions
    )
    for(i in colnames(current_matrix)){
      to_select <- intersect(unique(df[df[["strata"]] >= x, ][["spatial_id"]]), available(current_matrix, i))
      strata_mat <- select(strata_mat, i, to_select)
    }
    strata_mat
  })
  return(strata_mat)
}
