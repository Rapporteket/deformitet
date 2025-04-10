#'@title Module registreringer
#'@export


module_registreringer_UI <- function (id) {
  ns <- NS(id)

}

#'@title Server sammenligningsmodul
#'@export

module_registreringer_server <- function (id, userRole, userUnitId, data, raw_data) {
  moduleServer(
    id,
    function(input, output, session){
    }
  )
}
