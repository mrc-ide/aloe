#' Impact plot
#'
#' @param pd plot data
#'
#' @return Impact plot
impact_plot <- function(pd){
  pd <- pd |>
    tidyr::pivot_wider(id_cols = "year", names_from = "scenario", values_from = "inc") |>
    dplyr::mutate(dif = 100 * (bank - bau) / bau)

  # Crop upper extent to 100
  pd$dif[pd$dif >100] <- 100

  ggplot2::ggplot(data = pd, ggplot2::aes(x = year, y = dif, fill = dif)) +
    ggplot2::geom_bar(stat = "identity", col = "black") +
    ggplot2::scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(10, "PiYG"))) +
    ggplot2::scale_y_continuous(
      name = "% Change vs BAU",
      breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100),
      labels = c("-100", "-75", "-50", "-25", "0", "25", "50", "75", "100 +"),
      limits = c(-100, 100)) +
    ggplot2::theme_bw() +
    ggplot2::guides(fill = "none")
}

time_series_plot <- function(pd, maxy){
  ggplot2::ggplot(data = pd, ggplot2::aes(x = year, y = inc, col = scenario)) +
    ggplot2::geom_line() +
    ggplot2::ylim(0, maxy) +
    ggplot2::theme_bw()
}
