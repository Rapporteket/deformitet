#' Run the imongr Shiny Application
#'
#' @return An object representeing the imongr app
#' @export

kjor_appDeform <- function() {
  shiny::shinyApp(ui = ui_deform, server = server_deform)}

# Dette angir funksjon og ikke fil

