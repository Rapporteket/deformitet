#' Run the deformitet Shiny Application
#'
#' @return An object representeing the deformitet app
#' @export

run_app <- function() {
  shiny::shinyApp(ui = app_ui, server = app_server)
}
