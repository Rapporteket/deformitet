#'@title Module registreringer
#'@export


module_registreringer_UI <- function (id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::fluidPage(
      shiny::textOutput(outputId = ns("Test"))
    )
  )


}

#'@title Server sammenligningsmodul
#'@export

module_registreringer_server <- function (id, userRole, userUnitId, data, raw_data) {
  moduleServer(
    id,
    function(input, output, session){

      output$Test <- renderText(
        "Her kommer en oversikt over registreringer"
      )

    }
  )
}
