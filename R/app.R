app <- function(){

  best_plot <- impact_plot_base()

  cost_matrix <- mat(df$NAME_1, df$cost)
  cases_averted_matrix <- mat(df$NAME_1, df$cases_averted)
  deaths_averted_matrix <- mat(df$NAME_1, df$deaths_averted)
  current_selection <- setdiff(mwi$NAME_1, c("Balaka", "Ntchisi", "Phalombe", "Rumphi"))
  max_needed <- sum(apply(cost_matrix, 1, max))
  min_needed <- sum(apply(cost_matrix, 1, min))

  ui <- shiny::fluidPage(theme = shinythemes::shinytheme("slate"),
    shiny::fluidRow(
      shiny::column(8,
                    shiny::tabsetPanel(
                      shiny::tabPanel("ITNs",
                                      leaflet::leafletOutput("mymap_itn")
                      ),
                      shiny::tabPanel("IRS",
                                      leaflet::leafletOutput("mymap_irs")
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
      shiny::numericInput("budget" ,"Budget", value = max_needed, min = min_needed, max = max_needed),
      shiny::actionButton("opt_cases", "Show Optimum (Cases)"),
      shiny::actionButton("opt_deaths", "Show Optimum (Deaths)"),
      shiny::actionButton("cur", "Show Current"),
      shiny::htmlOutput("impact")
    )
  )

  server <- function(input, output, session) {
    rv <- shiny::reactiveValues()
    rv$selection_itn <- NULL
    rv$clicked_itn <- NULL
    rv$colour_itn <- NULL
    rv$selection_irs <- NULL
    rv$clicked_irs <- NULL
    rv$colour_irs <- NULL

    rv$colour <- NULL
    rv$budget <- NULL
    rv$optimised_cases <- NULL
    rv$optimised_deaths <- NULL

    # Define the basemaps
    output$mymap_itn <- base_map(mwi)
    outputOptions(output, "mymap_itn", suspendWhenHidden = FALSE)
    output$mymap_irs <- base_map(mwi)
    outputOptions(output, "mymap_irs", suspendWhenHidden = FALSE)

    # Budget optimisation
    shiny::observeEvent(input$budget,{
      rv$budget <- input$budget
      rv$optimised_cases <- om::om(z = cases_averted_matrix, cost = cost_matrix, budget = input$budget) |>
        as.data.frame() |>
        dplyr::select(i, j) |>
        dplyr::left_join(df, by = c("i", "j"))
      rv$optimised_deaths <- om::om(z = deaths_averted_matrix, cost = cost_matrix, budget = input$budget) |>
        as.data.frame() |>
        dplyr::left_join(df, by = c("i", "j"))
    })

    max_cases_averted <- shiny::reactive({
      sum(rv$optimised_cases$z)
    })
    max_deaths_averted <- shiny::reactive({
      sum(rv$optimised_deaths$z)
    })
    cases_select_itn <- shiny::reactive({
      rv$optimised_cases |>
        dplyr::filter(itn == 1) |>
        dplyr::pull(NAME_1)
    })
    deaths_select_itn <- shiny::reactive({
      rv$optimised_deaths |>
        dplyr::filter(itn == 1) |>
        dplyr::pull(NAME_1)
    })
    cases_select_irs <- shiny::reactive({
      rv$optimised_cases |>
        dplyr::filter(irs == 1) |>
        dplyr::pull(NAME_1)
    })
    deaths_select_irs <- shiny::reactive({
      rv$optimised_deaths |>
        dplyr::filter(irs == 1) |>
        dplyr::pull(NAME_1)
    })

    # On selection assign predefined optimum
    shiny::observeEvent(input$opt_cases, {
      rv$clicked_itn <- cases_select_itn()
      rv$colour_itn <- "deeppink"
      overlap_map(leaflet::leafletProxy("mymap_itn"), data = unmap_itn(), colour = "black")
      rv$selection_itn <- cases_select_itn()

      rv$clicked_irs <- cases_select_irs()
      rv$colour_irs <- "deeppink"
      overlap_map(leaflet::leafletProxy("mymap_irs"), data = unmap_irs(), colour = "black")
      rv$selection_irs <- cases_select_irs()
    })

    # On selection assign predefined optimum
    shiny::observeEvent(input$opt_deaths, {
      rv$clicked_itn <- deaths_select_itn()
      rv$colour_itn <- "deeppink"
      overlap_map(leaflet::leafletProxy("mymap_itn"), data = unmap_itn(), colour = "black")
      rv$selection_itn <- deaths_select_itn()

      rv$clicked_irs <- deaths_select_irs()
      rv$colour_irs <- "deeppink"
      overlap_map(leaflet::leafletProxy("mymap_irs"), data = unmap_irs(), colour = "black")
      rv$selection_irs <- deaths_select_irs()
    })
    # On selection assign predefined current
    shiny::observeEvent(input$cur, {
      rv$selection_itn <- current_selection
      rv$clicked_itn <- current_selection
      rv$colour_itn <- "deeppink"

      rv$selection_irs <- current_selection
      rv$clicked_irs <- current_selection
      rv$colour_irs <- "deeppink"

      # Clear the map
      overlap_map(leaflet::leafletProxy("mymap_itn"), data = unmap_itn(), colour = "black")
      overlap_map(leaflet::leafletProxy("mymap_irs"), data = unmap_irs(), colour = "black")
    })

    # On map click assign the clicked admin unit
    shiny::observeEvent(input$mymap_itn_shape_click, {
      rv$clicked_itn <- input$mymap_itn_shape_click$id
      # Selecting or deselecting a polygon
      if(input$mymap_itn_shape_click$id %in% rv$selection_itn){
        rv$colour_itn <- "black"
        rv$selection_itn <- setdiff(rv$selection_itn, input$mymap_itn_shape_click$id)
      } else {
        rv$selection_itn <- c(rv$selection_itn, input$mymap_itn_shape_click$id)
        rv$colour_itn <- "deeppink"
      }
    })
    shiny::observeEvent(input$mymap_irs_shape_click, {
      rv$clicked_irs <- input$mymap_irs_shape_click$id
      # Selecting or deselecting a polygon
      if(input$mymap_irs_shape_click$id %in% rv$selection_irs){
        rv$colour_irs <- "black"
        rv$selection_irs <- setdiff(rv$selection_irs, input$mymap_irs_shape_click$id)
      } else {
        rv$selection_irs <- c(rv$selection_irs, input$mymap_irs_shape_click$id)
        rv$colour_irs <- "deeppink"
      }
    })

    # Get polygon(s) to modify
    map_itn <- shiny::reactive({
      dplyr::filter(mwi, NAME_1 %in% rv$clicked_itn)
    })
    unmap_itn <- shiny::reactive({
      dplyr::filter(mwi, NAME_1 %in% rv$selection_itn)
    })

    shiny::observe({
      overlap_map(leaflet::leafletProxy("mymap_itn"), data = map_itn(), colour = rv$colour_itn)
    })

    map_irs <- shiny::reactive({
      dplyr::filter(mwi, NAME_1 %in% rv$clicked_irs)
    })
    unmap_irs <- shiny::reactive({
      dplyr::filter(mwi, NAME_1 %in% rv$selection_irs)
    })

    shiny::observe({
      overlap_map(leaflet::leafletProxy("mymap_irs"), data = map_irs(), colour = rv$colour_irs)
    })

    selected_df <- shiny::reactive({
      browser()
      df_selection(df, rv$selection_itn, rv$selection_irs)
    })

    cases_averted <- shiny::reactive({
      sum(selected_df()$cases_averted)
    })
    cases_averted_pc <- shiny::reactive({
      browser()
      round(100 * cases_averted() / max_cases_averted())
    })
    deaths_averted <- shiny::reactive({
      sum(selected_df()$deaths_averted)
    })
    deaths_averted_pc <- shiny::reactive({
      round(100 * deaths_averted() / max_deaths_averted())
    })
    budget_spent <- shiny::reactive({
      sum(selected_df()$cost)
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
  }

  shiny::shinyApp(ui, server)



}
