#' App
#'
#' Launch shiny app
#'
#' @return NULL
#' @export
app <- function(){

  all <- unique(df$NAME_1)
  current <- sample(all, 3)

  ui <- shiny::fluidPage(theme = shinythemes::shinytheme("slate"),

    shiny::fluidRow(
      shiny::column(8,
             shiny::h4("Interventions"),
             shiny::tabsetPanel(
               shiny::tabPanel("ITNs", mapUI("ITNs")),
               shiny::tabPanel("IRS",  mapUI("IRS"))
             )),
      shiny::column(4,
             shiny::h4("Outcome"),
             shiny::headerPanel(""), shiny::headerPanel(""),
             shiny::plotOutput("impact_plot"))
    ),
    shiny::fluidRow(
      shiny::h4("Optimisation"),
      shiny::column(2, shiny::numericInput("budget", "Budget", min = 0, max = 100000, value = 100000)),
      shiny::column(3, shiny::radioButtons("target", "Target", choices = list("Cases averted", "Deaths averted"))),
      shiny::column(2, shiny::actionButton("optimise", "Optimise"))
    )
  )

  server <- function(input, output, session) {

    rv <- shiny::reactiveValues(variable = NULL, trigger = 0,
                                target = NULL, budget = NULL,
                                best_cases = NULL, best_deaths = NULL,
                                itn_selected = NULL, irs_selected = NULL)

    output$impact_plot <- shiny::renderPlot({
      impact_plot_base()
    })

    shiny::observeEvent(input$budget, {
      rv$budget <- input$budget
      rv$best_cases <- sum(optimise("Cases averted", rv$budget)$cases_averted)
      rv$best_deaths <- sum(optimise("Deaths averted", rv$budget)$deaths_averted)
    })
    shiny::observeEvent(input$target, {
      rv$target <- input$target
    })
    shiny::observeEvent(input$optimise, ignoreInit = TRUE, {
      optim_df <- optimise(rv$target, rv$budget)
      rv$variable$ITNs <- optim_df[optim_df$itn == 1, "NAME_1"]
      rv$variable$IRS <- optim_df[optim_df$irs == 1, "NAME_1"]
      rv$trigger <- rv$trigger + 1
    })

    shiny::observe({
      rv$itn_selected <-  mapServer("ITNs", shiny::reactive(rv$variable), shiny::reactive(rv$trigger), all, current)
      rv$irs_selected <- mapServer("IRS", shiny::reactive(rv$variable), shiny::reactive(rv$trigger), all, current)
    })

    cur_df <- shiny::reactive(df_selection(df, rv$itn_selected(), rv$irs_selected()))
    cur_ca <- shiny::reactive(sum(cur_df()$cases_averted))
    cur_ca_pc <- shiny::reactive(round(100 * cur_ca() / rv$best_cases))
    cur_da <- shiny::reactive(sum(cur_df()$deaths_averted))
    cur_da_pc <- shiny::reactive(round(100 * cur_da() / rv$best_deaths))
    cur_bs <- shiny::reactive(sum(cur_df()$cost))
    cur_bs_pc <- shiny::reactive(round(100 * cur_bs() / rv$budget))

    output$impact_plot <- shiny::renderPlot({
      pd <- data.frame(x = c(
        "Budget\nspent",
        "Cases\naverted",
        "Deaths\naverted"),
        y = c(
          cur_bs_pc(),
          cur_ca_pc(),
          cur_da_pc()
        )
      )

      bp <- impact_plot_base() +
        ggplot2::geom_bar(data = pd, ggplot2::aes(x = .data$x, y = .data$y, fill = .data$y), stat = "identity") +
        ggplot2::scale_fill_gradient(low = "darkred", high = "green2", limits = c(0, 100), guide = "none", na.value = "orange")
      if(cur_bs_pc() > 100){
        bp <- bp +
          ggplot2::ggtitle("Warning! Over budget") +
          ggplot2::theme(title = ggplot2::element_text(colour = "red"))
      }
      bp
    })

  }

  shiny::shinyApp(ui, server)
}
