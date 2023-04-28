#' Impact plot
#'
#' @param pd plot data
#'
#' @return Impact plot
impact_plot <- function(pd){

  # Crop upper extent to 100
  pd$y[pd$y >100] <- 100

  ip <- ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 0, lty = 2) +
    ggplot2::xlab("") +
    ggplot2::scale_y_continuous(
      name = "% Change vs BAU",
      breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100),
      labels = c("-100", "-75", "-50", "-25", "0", "25", "50", "75", "100 +"),
      limits = c(-100, 100)) +
    ggplot2::theme_bw() +
    ggplot2::geom_bar(data = pd, ggplot2::aes(x = .data$x, y = .data$y, fill = .data$x), stat = "identity", col = "black") +
    ggplot2::scale_fill_manual(values = c("#86cae7", "#e3afc7", "#bed4aa")) +
    ggplot2::theme(text = ggplot2::element_text(size = 20)) +
    ggplot2::guides(fill = "none")

  return(ip)
}
