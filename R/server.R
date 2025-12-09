#' Server logic for the deformitet app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

### + eksport i server

app_server <- function(input, output, session) {

  ######### DATA TIDYING------------------------------------------------------
  ### Read in data:
  raw_regdata <- les_og_flate_ut()

  #### Clean and tidy data:

  regdata <- pre_pros(raw_regdata)

  ######## USER INFO----------------------------------------------------------

  # Make a df that can be used for mapping between resh-ids and hospital names
  # Must be organized as df with two columns: UnitId and orgname
  # in order for navbarWidgetServer2 to work properly

  map_db_resh <- regdata |>
    dplyr::select("Sykehus", "CENTREID") |> # select required columns
    unique() |> # keep only unique variables
    dplyr::mutate(UnitId = CENTREID, # make new column with new name
                  orgname = Sykehus) |> # make new column with new name
    dplyr::select(-c(Sykehus, CENTREID)) # take out old columns


  user <- rapbase::navbarWidgetServer("deformitetNavbarWidget",
                                      "deformitet",
                                      map_orgname = shiny::req(map_db_resh))


  #####################################################################
  ##### TAB: Startside ################################################

  # Veiledning
  output$veiledning <- shiny::renderUI({
    rapbase::renderRmd(
      system.file("veiledning.Rmd", package = "deformitet"),
      outputType = "html_fragment"
    )
  })

  #######################################################################
  ##### TAB: Fordelingsfigur- og tabell #################################

  module_fordeling_server("fordeling",
                          data = regdata,
                          raw_data = raw_regdata,
                          userRole = user$role,
                          userUnitId = user$org,
                          map_data = map_db_resh)

  #########################################################################
  ##### TAB: gjennomsnitt #################################################


  module_gjennomsnitt_server("gjen1",
                             data = regdata,
                             userRole = user$role,
                             userUnitId = user$org,
                             map_data = map_db_resh)

  #########################################################################
  ##### TAB: Kvalitetsindikatorer #########################################


  module_kvalitetsindikator_server("kval1",
                                   data = regdata,
                                   map_data = map_db_resh,
                                   userRole = user$role,
                                   userUnitId = user$org)

  #######################################################################
  ##### TAB: Sammenligning ##############################################

  module_sammenligning_server("sam1",
                              data = regdata,
                              userRole = user$role,
                              userUnitId = user$org)

  #######################################################################
  ##### TAB: Registreringer #############################################


  module_registreringer_server("reg1",
                               data = regdata,
                               userRole = user$role,
                               userUnitId = user$org)

  #######################################################################
  ##### TAB: SPC ########################################################

  # Add ready-made module here if requested by registry


  #######################################################################
  ##### TAB: Nestlasting av datadump ####################################


  module_datadump_server("module_1",
                         data = regdata,
                         userRole = user$role,
                         userUnitId = user$org)

  #########################################################################
  ###### TAB: Exporting data ##############################################

  shiny::observeEvent(
    shiny::req(user$role()), {
      if (user$role() != "SC") {
        shiny::hideTab("tabs", target = "Eksport")
      } else {
        shiny::showTab("tabs", target = "Eksport")
      }
    }
  )

  # Brukerkontroller

  rapbase::exportUCServer("deformitetExport", "deformitet")

  # Veiledning

  rapbase::exportGuideServer("deformitetExportGuide", "deformitet")


}
