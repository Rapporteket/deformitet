#'@title Module registreringer
#'@export


module_registreringer_UI <- function (id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::fluidPage(
      shiny::htmlOutput(outputId = ns("Test"), inline = TRUE)

    )
  )


}

#'@title Server sammenligningsmodul
#'@export

module_registreringer_server <- function (id, userRole, userUnitId, data, raw_data) {
  moduleServer(
    id,
    function(input, output, session){

      output$Test <- shiny::renderUI({
        rapbase::renderRmd(
          system.file("registreringer.Rmd", package = "deformitet"),
          outputType = "html_fragment"
        )
      }
      )

    }
  )
}

