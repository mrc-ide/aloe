#' Column check
#'
#' @param col Column name
#' @param opts Present column names
col_check <- function(col, opts){
  if(!col %in% opts){
    stop("Input data must include the column: ", col)
  }
}

#' Column checks input data.frame
#'
#' @param input_df Input data.frame
#' @inheritParams app
check_input_df <- function(input_df, spatial_id){
  df_names <- names(input_df)
  col_check("strata", df_names)
  col_check(spatial_id, df_names)
  col_check("scenario", df_names)
}

#' Column checks spatial input
#'
#' @param input_spatial Input spatial data
#' @inheritParams app
check_input_spatial <- function(input_spatial, spatial_id){
  spatial_names <- names(input_spatial)
  col_check(spatial_id, spatial_names)
}
