#' Impact plot
#'
#' @param pd plot data
#'
#' @return Impact plot
impact_plot <- function(pd){
  ip<-  ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 100, lty = 2) +
    ggplot2::xlab("") +
    ggplot2::scale_y_continuous(
      name = "% of maximum",
      breaks = c(0, 25, 50, 75, 100)) +
    ggplot2::theme_bw() +
    ggplot2::geom_bar(data = pd, ggplot2::aes(x = .data$x, y = .data$y, fill = .data$y), stat = "identity") +
    ggplot2::scale_fill_gradient(low = "darkred", high = "green2", limits = c(0, 100), guide = "none", na.value = "orange")

  if(pd[pd$x == "Budget\nspent", "y"] > 100){
    ip <- ip +
      ggplot2::ggtitle("Warning! Over budget") +
      ggplot2::theme(title = ggplot2::element_text(colour = "red"))
  }
  return(ip)
}
