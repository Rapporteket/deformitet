#' Run the Deformitet Shiny Application
#'
#' @return An object representeing the Deformitet app
#' @export

kjor_app_deform <- function() {
  shiny::shinyApp(ui = ui_deform, server = server_deform)
}
