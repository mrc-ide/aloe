app <- function(){

  all <- unique(df$NAME_1)
  current <- sample(all, 3)

  ui <- shiny::fluidPage(
    shiny::h4("Interventions"),
    shiny::fluidRow(
      column(8,
             shiny::tabsetPanel(
               shiny::tabPanel("ITNs", mapUI("ITNs")),
               shiny::tabPanel("IRS",  mapUI("IRS"))
             )),
      column(4, shiny::plotOutput("impact_plot"))
    ),
    shiny::fluidRow(
      shiny::h4("Optimisation"),
      shiny::column(4, shiny::numericInput("budget", "Budget", min = 0, max = 10000, value = 10000)),
      shiny::column(4, shiny::radioButtons("target", "Target", choices = list("Cases averted", "Deaths averted"))),
      shiny::column(4, shiny::actionButton("optimise", "Optimise"))
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
      rv$itn_selected <-  mapServer("ITNs", reactive(rv$variable), reactive(rv$trigger), all, current)
      rv$irs_selected <- mapServer("IRS", reactive(rv$variable), reactive(rv$trigger), all, current)
    })

    cur_df <- shiny::reactive(df_selection(df, rv$itn_selected(), rv$irs_selected()))
    cur_ca <- shiny::reactive(sum(cur_df()$cases_averted))
    cur_ca_pc <- shiny::reactive(round(100 * cur_ca() / rv$best_cases))
    cur_da <- shiny::reactive(sum(cur_df()$deaths_averted))
    cur_da_pc <- shiny::reactive(round(100 * cur_da() / rv$best_deaths))
    cur_bs <- shiny::reactive(sum(cur_df()$cost))
    cur_bs_pc <- shiny::reactive(round(100 * cur_bs() / rv$budget))

    output$impact_plot <- shiny::renderPlot({
      impact_plot_base() +
        ggplot2::geom_bar(ggplot2::aes(x = "Budget\nspent", y = cur_bs_pc()), stat = "identity") +
        ggplot2::geom_bar(ggplot2::aes(x = "Cases\naverted", y = cur_ca_pc()), stat = "identity") +
        ggplot2::geom_bar(ggplot2::aes(x = "Deaths\naverted", y = cur_da_pc()), stat = "identity")
    })

  }

  shiny::shinyApp(ui, server)
}
