
### Module for creating data based on selection criteria

#'@export
module_datadump_UI <- function(id){
  ns <- NS(id)
  tagList(
    dateRangeInput( # first select - var1
      inputId = "date",
      label = "Tidsintervall:",
      start = "2023-01-02",
      end = "2024-09-02",
      min = "2023-01-01",
      max = "2025-09-02",
      format = "mm/dd/yy",
      separator = " - "),

    selectInput( # second select - var2
      inputId = "kjønn_var",
      label = "Utvalg basert på kjønn",
      choices = c("begge", "mann", "kvinne"),
      selected = "begge"),

    sliderInput( # third select - var3
      inputId = "alder_var",
      label = "Aldersintervall:",
      min = 0,
      max = 100,
      value = c(10, 20),
      dragRange = TRUE),

    downloadButton(
      outputId= "d1",
      label = "Download",
      class = "btn-lg btn-success")
  )
}


#'@export
module_datadump_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){

      # do the cleaning
      clean_datadump_reactive <- reactive({
        data <- deformitet::clean_datadump(regdata, input$date[1], input$data[2], input$kjønn_var, input$alder_var[1], input$alder_var[2])
      })
  })
}
