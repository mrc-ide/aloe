df_selection <- function(df, selected_itn, selected_irs){
  data.frame(
    NAME_1 = unique(df$NAME_1)) |>
    dplyr::mutate(
      itn = ifelse(NAME_1 %in% selected_itn, 1, 0),
      irs = ifelse(NAME_1 %in% selected_irs, 1, 0)
    ) |>
    dplyr::left_join(df, by = c("NAME_1", "itn", "irs"))
}
