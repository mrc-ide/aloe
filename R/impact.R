impactUI <- function(){
  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::fluidRow(
      shiny::br(),
      shiny::column(
        6,
        shiny::actionButton("impact_button", "Generate impact report")
      )
    ),
    shiny::fluidRow(
      shiny::br(),
      shiny::br(),
      shiny::column(
        6,
        shiny::plotOutput("time_series_plot", width = "600px")
      ),
      shiny::column(
        6,
        shiny::plotOutput("impact_plot", width = "500px")
      )
    )
  )
}

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
      name = "% Change vs BAU",
      breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100),
      labels = c("-100", "-75", "-50", "-25", "0", "25", "50", "75", "100 +"),
      limits = c(-100, 100)) +
    ggplot2::theme_bw() +
    ggplot2::guides(fill = "none")
}

#' Time series plot plot
#'
#' @param pd plot data
#' @param maxy max y value
plot_time_series <- function(pd, maxy){
  ggplot2::ggplot(data = pd, ggplot2::aes(x = .data$year, y = .data$inc, col = .data$scenario)) +
    ggplot2::geom_line() +
    ggplot2::ylim(0, maxy) +
    ggplot2::theme_bw()
}
