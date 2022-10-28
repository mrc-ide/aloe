app <- function(){

  best_plot <- impact_plot_base()

  cost_matrix <- mat(df$NAME_1, df$cost)
  cases_averted_matrix <- mat(df$NAME_1, df$cases_averted)
  deaths_averted_matrix <- mat(df$NAME_1, df$deaths_averted)
  current_selection <- setdiff(mwi$NAME_1, c("Balaka", "Ntchisi", "Phalombe", "Rumphi"))
  max_needed <- df |>
    dplyr::group_by(NAME_1) |>
    dplyr::slice_max(order_by = cost, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::pull(cost) |>
    sum()
  min_needed <- df |>
    dplyr::group_by(NAME_1) |>
    dplyr::slice_min(order_by = cost, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::pull(cost) |>
    sum()

  ui <- shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(8,
                    shiny::tabsetPanel(
                      shiny::tabPanel("ITNs",
                                      leaflet::leafletOutput("mymap")
                      ),
                      shiny::tabPanel("SMC",
                                      leaflet::leafletOutput("mymap2")
                      )
                    )),
      shiny::column(4,
                    shiny::tabsetPanel(
                      shiny::tabPanel("Outcome",
                                      shiny::plotOutput("impact_plot")
                      )
                    )
      )
    ),

    shiny::fluidRow(
      shiny::sliderInput("budget" ,"Budget", value = 10000, min = min_needed, max = max_needed),
      shiny::actionButton("opt_cases", "Show Optimum (Cases)"),
      shiny::actionButton("opt_deaths", "Show Optimum (Deaths)"),
      shiny::actionButton("cur", "Show Current"),
      shiny::htmlOutput("impact")
    ),
    shiny::fluidRow(
      shiny::htmlOutput("selected")
    )
  )

  server <- function(input, output, session) {
    rv <- shiny::reactiveValues()
    rv$selection <- NULL
    rv$clicked <- NULL
    rv$colour <- NULL
    rv$budget <- NULL
    rv$optimised_cases <- NULL
    rv$optimised_deaths <- NULL

    # Define the basemap
    output$mymap <- base_map(mwi)
    output$mymap2 <- base_map(mwi)

    # Budget optimisation
    shiny::observeEvent(input$budget,{
      rv$budget <- input$budget
      rv$optimised_cases <- om::om(z = cases_averted_matrix, cost = cost_matrix, budget = input$budget)
      rv$optimised_deaths <- om::om(z = deaths_averted_matrix, cost = cost_matrix, budget = input$budget)
    })

    cases_select <- shiny::reactive({
      unique(df$NAME_1)[rv$optimised_cases[,2] == 2]
    })
    deaths_select <- shiny::reactive({
      unique(df$NAME_1)[rv$optimised_deaths[,2] == 2]
    })
    max_cases_averted <- shiny::reactive({
      sum(rv$optimised_cases[,4])
    })
    max_deaths_averted <- shiny::reactive({
      sum(rv$optimised_deaths[,4])
    })

    # On selection assign predefined optimum
    shiny::observeEvent(input$opt_cases, {
      rv$clicked <- cases_select()
      rv$colour <- "deeppink"
      overlap_map(data = unmap(), colour = "black")
      rv$selection <- cases_select()
    })
    # On selection assign predefined optimum
    shiny::observeEvent(input$opt_deaths, {
      rv$clicked <- deaths_select()
      rv$colour <- "deeppink"
      overlap_map(data = unmap(), colour = "black")
      rv$selection <- deaths_select()
    })
    # On selection assign predefined current
    shiny::observeEvent(input$cur, {
      rv$selection <- current_selection
      rv$clicked <- current_selection
      rv$colour <- "deeppink"

      # Clear the map
      overlap_map(data = unmap(), colour = "black")
    })

    # On map click assign the clicked admin unit
    shiny::observeEvent(input$mymap_shape_click, {
      rv$clicked <- input$mymap_shape_click$id
      # Selecting or deselecting a polygon
      if(input$mymap_shape_click$id %in% rv$selection){
        broswer()
        rv$colour <- "black"
        rv$selection <- setdiff(rv$selection, input$mymap_shape_click$id)
      } else {
        rv$selection <- c(rv$selection, input$mymap_shape_click$id)
        rv$colour <- "deeppink"
      }
    })

    # Get polygon(s) to modify
    map <- shiny::reactive({
      dplyr::filter(mwi, NAME_1 %in% rv$clicked)
    })
    unmap <- shiny::reactive({
      dplyr::filter(mwi, NAME_1 %in% rv$selection)
    })

    shiny::observe({
      overlap_map(data = map(), colour = rv$colour)
    })

    cases_averted <- shiny::reactive({
      df |>
        dplyr::filter((NAME_1 %in% rv$selection & itn == 1) |
                        (!NAME_1 %in% rv$selection & itn == 0)) |>
        dplyr::pull(cases_averted) |>
        sum() |>
        unlist()
    })
    cases_averted_pc <- shiny::reactive({
      round(100 * cases_averted() / max_cases_averted())
    })
    deaths_averted <- shiny::reactive({
      df |>
        dplyr::filter((NAME_1 %in% rv$selection & itn == 1) |
                        (!NAME_1 %in% rv$selection & itn == 0)) |>
        dplyr::pull(deaths_averted) |>
        sum() |>
        unlist()
    })
    deaths_averted_pc <- shiny::reactive({
      round(100 * deaths_averted() / max_deaths_averted())
    })
    budget_spent <- shiny::reactive({
      df |>
        dplyr::filter((NAME_1 %in% rv$selection & itn == 1) |
                        (!NAME_1 %in% rv$selection & itn == 0)) |>
        dplyr::pull(cost) |>
        sum()
    })
    budget_spent_pc <- shiny::reactive({
      round(100 * budget_spent() / rv$budget)
    })

    output$impact <- shiny::renderUI(paste(
      "Cases averted:", cases_averted(),
      ", deaths averted:", deaths_averted()))

    output$impact_plot <- shiny::renderPlot({
      bp <- best_plot +
        ggplot2::geom_bar(ggplot2::aes(x = c("Cases\naverted", "Deaths\naverted", "Budget\nspent"),
                                       y = c(cases_averted_pc(), deaths_averted_pc(), budget_spent_pc()),
                                       fill = c(cases_averted_pc(), deaths_averted_pc(), 100-budget_spent_pc())),
                          col = NA, stat = "identity") +
        ggplot2::scale_fill_gradient(low = "darkred", high = "green2", limits = c(0, 100), guide = "none")
      if(budget_spent_pc() > 100){
        bp <- bp +
          ggplot2::ggtitle("Warning! Over budget") +
          ggplot2::theme(title = ggplot2::element_text(colour = "red"))
      }
      bp
    })

    selected_names <- shiny::reactive({
      paste("Selected areas:", paste(rv$selection, collapse = ", "))
    })
    output$selected <- shiny::renderUI(selected_names())
  }

  shiny::shinyApp(ui, server)



}
