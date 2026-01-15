# Resultattjeneste for Deformitet

#' Brukergrensesnitt (ui) til appen
#'
#' @return Brukergrensesnittet (ui) til appen
#' @export

ui_deform <- function() {


  shiny::tagList( # Needed for "about the user" tags
    shiny::navbarPage( # type of page

      ###### Graphics ----------------------------------------------------------
      title = rapbase::title("Rapporteket for deformitet"),
      windowTitle = "Rapporteket for deformitet",
      theme = rapbase::theme(version = 5),
      id = "tabs",

      ################################################################################
      ######## TAB user info----------------------------------------------------------

      ##### Startside (info about Rapporteket and the registry)-----------------------

      shiny::tabPanel( # First tab
        title = "Startside",
        shiny::mainPanel(
          width = 12,
          shiny::htmlOutput("veiledning", inline = TRUE), # This file is found in folder "inst"
          rapbase::navbarWidgetInput("deformitetNavbarWidget", selectOrganization = TRUE),
          shiny::tags$head(shiny::tags$link(rel = "shortcut icon", href = "rap/favicon.ico"))
        )
      ),

      ################################################################################
      ##### TAB: Fordelingsfigur og -tabell ##########################################

      ### Fordelingsfigur og -tabell--------------------------------------------------

      shiny::tabPanel(
        title = "Fordelingsfigur og -tabell",
        module_fordeling_UI("fordeling")
      ),

      ################################################################################
      ##### TAB: Kvalitetsindikatorer ################################################

      shiny::tabPanel(
        title = "Kvalitetsindikatorer",
        module_kvalitetsindikator_UI("kval1")
      ),



      ################################################################################
      ##### TAB: Sammenligning  ######################################################

      shiny::tabPanel(
        title = "Sammenligning",
        module_sammenligning_ui("sam1")
      ),

      ################################################################################
      ##### TAB: Registreringer  #####################################################

      shiny::tabPanel(
        title = "Registreringer",
        module_registreringer_UI("reg1")
      ),

      ################################################################################
      ##### TAB: spc  ################################################################

      # Add ready-made spc-module here if requested by the registry


      ################################################################################
      ##### TAB: Nestlasting av datadump #############################################


      shiny::tabPanel( # third tab
        title = "Registeradm",
        shiny::fluidPage(
          module_datadump_UI(
            id = "mod_datadump"
          )
        )
      ),

      shiny::tabPanel(
        title = "Eksport",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::exportUCInput("deformitetExport")
          ),
          shiny::mainPanel(
            rapbase::exportGuideUI("deformitetExportGuide")
          )
        )
      )


    ) # navbarPage
  ) # tagList
}

#' Server logic for the deformitet app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

server_deform <- function(input, output, session) {


  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(DT)
  library(rapbase)
  library(bslib)
  library(shinyWidgets)
  library(lubridate)
  library(stringr)
  library(usethis)

  ######### DATA TIDYING----------------------------------------------------------
  ### Read in data:
  raw_regdata <- alleRegData()

  #### Clean and tidy data:

  regData <- pre_pros(raw_regdata)

  ######## USER INFO--------------------------------------------------------------

  # Make a df that can be used for mapping between resh-ids and hospital names
  # Must be organized as df with two columns: UnitId and orgname
  # in order for navbarWidgetServer2 to work properly

  map_db_resh <- data.frame(  #map_avdeling <-
    UnitId = unique(regData$CENTREID),
    orgname = regData$Sykehus[match(
      unique(regData$CENTREID),
      regData$CENTREID
    )]
  )

  user <- rapbase::navbarWidgetServer2(
    id = "deformitetNavbarWidget",
    orgName = "deformitet",
    caller = "deformitet",
    map_orgname = shiny::req(map_db_resh)
  )


  ################################################################################
  ##### TAB: Startside ###########################################################

  # Veiledning
  output$veiledning <- shiny::renderUI({
    rapbase::renderRmd(
      system.file("veiledning.Rmd", package = "deformitet"),
      outputType = "html_fragment"
    )
  })

  ################################################################################
  ##### TAB: Fordelingsfigur- og tabell ##########################################

  module_fordeling_server(
    "fordeling",
    data = regData,
    raw_data = raw_regdata,
    userRole = user$role,
    userUnitId = user$org(),
    map_data = map_db_resh
  )

  ################################################################################
  ##### TAB: Kvalitetsindikatorer ################################################


  module_kvalitetsindikator_server(
    "kval1",
    data = regData,
    map_data = map_db_resh,
    userRole = user$role,
    userUnitId = user$org
  )

  ################################################################################
  ##### TAB: Sammenligning #####################################################


  module_sammenligning_server(
    "sam1",
    data = regData,
    userRole = user$role,
    userUnitId = user$org
  )

  ################################################################################
  ##### TAB: Registreringer #####################################################


  module_registreringer_server(
    "reg1",
    data = regData,
    userRole = user$role,
    userUnitId = user$org()
  )

  ################################################################################
  ##### TAB: SPC #################################################################

  # Add ready-made module here if requested by registry


  ################################################################################
  ##### TAB: Nestlasting av datadump #############################################


  module_datadump_server(
    "mod_datadump",
    data = regData,
    userRole = user$role,
    userUnitId = user$org()
  )

  ################################################################################
  ###### TAB: Exporting data #####################################################

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
