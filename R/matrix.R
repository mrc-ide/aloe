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

implemented <- function(mat, intervention){
  rownames(mat)[mat[,intervention]]
}

# This will include a * coverage step
link <- function(mat, df){
  choice_df <- data.frame(
    spatial_id = row.names(mat),
    mat + 0
  )
  dplyr::left_join(choice_df, df, by = colnames(choice_df))
}

# Create the current matrix
create_current_matrix <- function(df, units, interventions){
  current_mat <- init_matrix(
    units = units,
    interventions = interventions
  )
  for(i in interventions){
    current_mat <- select(current_mat, i, df[df[["current"]] == 1 & df[[i]] == 1, ][["spatial_id"]])
  }
  return(current_mat)
}


# Create the strata matrices
create_strata_matrix <- function(df, units, interventions){
  n_strata <- max(df$strata)
  strata_mat <- lapply(1:n_strata, function(x){
    strata_mat <- init_matrix(
      units = units,
      interventions = interventions
    )
    for(i in interventions){
      strata_mat <- select(strata_mat, i, df[df[["strata"]] >= x, ][["spatial_id"]])
    }
    strata_mat
  })
  return(strata_mat)
}
