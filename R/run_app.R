#' Run the imongr Shiny Application
#'
#' @return An object representeing the imongr app
#' @export

run_app <- function() {
  library(deformitet)
  shiny::shinyApp(ui = app_ui, server = app_server)
}

