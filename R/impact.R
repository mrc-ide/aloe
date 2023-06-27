#' Impact plot
#'
#' @param pd plot data
plot_impact <- function(pd){
  pd <- pd |>
    tidyr::pivot_wider(id_cols = "year", names_from = "scenario", values_from = "inc") |>
    dplyr::mutate(dif = 100 * (.data$bank - .data$bau) / .data$bau)

  # Crop upper extent to 100
  pd$dif[pd$dif >100] <- 100

  ggplot2::ggplot(data = pd, ggplot2::aes(x = .data$year, y = .data$dif, fill = .data$dif)) +
    ggplot2::geom_bar(stat = "identity", col = "black") +
    ggplot2::scale_fill_gradientn(colours = impact_colours) +
    ggplot2::scale_y_continuous(
      breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100),
      labels = c("-100", "-75", "-50", "-25", "0", "25", "50", "75", "100 +"),
      limits = c(-100, 100)) +
    ggplot2::guides(fill = "none") +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   legend.position = "bottom")
}

#' Time series plot plot
#'
#' @param pd plot data
#' @param maxy max y value
plot_time_series <- function(pd, maxy){
  ggplot2::ggplot(data = pd, ggplot2::aes(x = .data$year, y = .data$inc, col = .data$scenario)) +
    ggplot2::geom_line(linewidth = 1.5) +
    ggplot2::scale_colour_manual(values = c("orange", "grey40")) +
    ggplot2::ylim(0, maxy) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   legend.position = c(0.1, 0.1))
}
