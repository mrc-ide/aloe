impactUI <- function(id){

  shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::fluidRow(
      shiny::br(),
      shiny::column(
        6,
        shiny::actionButton(shiny::NS(id, "impact_button"), "Generate impact report")
      )
    ),
    shiny::fluidRow(
      shiny::br(),
      shiny::br(),
      shiny::column(
        6,
        shiny::plotOutput(shiny::NS(id, "time_series_plot"), width = "600px")
      ),
      shiny::column(
        6,
        shiny::plotOutput(shiny::NS(id, "impact_plot"), width = "500px")
      )
    )
  )
}

impactServer <- function(id, rv, coverage, df){
  shiny::moduleServer(id, function(input, output, session){
    shiny::observeEvent(input$impact_button, {
      impact <- link(rv(), coverage() / 100, df)

      output$time_series_plot <- shiny::renderPlot(
        time_series_plot(
          pd = impact,
          maxy = max(df$inc)
        )
      )

      output$impact_plot <- shiny::renderPlot(
        impact_plot(
          pd = impact
        )
      )

      shinyjs::show("impact_plot")
      shinyjs::show("time_series_plot")
    })

    shiny::observeEvent(rv() | coverage(), {
      shinyjs::hide("time_series_plot")
      shinyjs::hide("impact_plot")
    })

  })
}
