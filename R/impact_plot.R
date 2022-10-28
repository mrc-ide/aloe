#' Base impact plot
#'
#' @param cases_averted_max Maximum cases averted (optimal)
#'
#' @return Base impact plot
impact_plot_base <- function(){
  ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = 100, lty = 2) +
    ggplot2::geom_bar(
      ggplot2::aes(
        x = c("Cases\naverted", "Deaths\naverted", "Budget\nspent"),
        y = c(0, 0, 0)
      ),
      fill = NA, col = NA, stat = "identity"
    ) +
    ggplot2::xlab("") +
    ggplot2::scale_y_continuous(
      name = "% of maximum",
      breaks = c(0, 25, 50, 75, 100)) +
    ggplot2::theme_bw()
}
