#' UI part of "startside" for Rapporteket for deformitet
#'
#' @export
#'

startside_UI <- function(id){
  ns <- NS(id)
  shiny::bootstrapPage(
    div(class="container",
        div(class = "panel-heading", style = "background-color: #E0E0E0",
            h2('Velkommen til Rapporteket for deformitet', align = 'center')))
  )
}
