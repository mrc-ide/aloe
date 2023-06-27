instructionsUI <- shiny::tabPanel(
  "Instructions",
  shiny::br(),
  shiny::h4("Interventions"),
  shiny::p("For each intervention choose where interventions are implemented"),
  shiny::p("Selections can be made by clicking on the map, using the buttons or via drop down list"),
  shiny::br(),
  shiny::h4("Impact"),
  shiny::p("Click the generate impact button to view theimpact of the current selection")
)
