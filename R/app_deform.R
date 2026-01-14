# Resultattjeneste for Deformitet

#' Brukergrensesnitt (ui) til appen
#'
#' @return Brukergrensesnittet (ui) til appen
#' @export

ui_deform <- function() {

  startDato <- paste0(as.numeric(format(Sys.Date()-200, "%Y")), '-01-01')


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
            shiny::htmlOutput("veiledning", inline = TRUE), # load in the htmloutput wanted. This file is found in folder "inst"
            rapbase::navbarWidgetInput("deformitetNavbarWidget", selectOrganization = TRUE),
            tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico"))
          )
        ),

        ################################################################################
        ##### TAB: Fordelingsfigur og -tabell ##########################################

        ### Fordelingsfigur og -tabell--------------------------------------------------

        shiny::tabPanel(
          title = "Fordelingsfigur og -tabell",
          deformitet::module_fordeling_UI("fordeling")
        ),

        ################################################################################
        ##### TAB: Gjennomsnitt  #######################################################

        # shiny::tabPanel(
        #   title = "Gjennomsnitt",
        #   deformitet::module_gjennomsnitt_UI("gjen1")
        # ),


        ################################################################################
        ##### TAB: Kvalitetsindikatorer ################################################

        shiny::tabPanel(
          title = "Kvalitetsindikatorer",
          deformitet::module_kvalitetsindikator_UI("kval1")
        ),



        ################################################################################
        ##### TAB: Sammenligning  ######################################################

        shiny::tabPanel(
          title = "Sammenligning",
          deformitet::module_sammenligning_UI("sam1")
        ),

        ################################################################################
        ##### TAB: Registreringer  #####################################################

        shiny::tabPanel(
          title = "Registreringer",
          deformitet::module_registreringer_UI("reg1")
        ),

        ################################################################################
        ##### TAB: spc  ################################################################

        # Add ready-made spc-module here if requested by the registry


        ################################################################################
        ##### TAB: Nestlasting av datadump #############################################


        shiny::tabPanel( # third tab
          title = "Registeradm",
          shiny::fluidPage(
            deformitet::module_datadump_UI(
              id = "mod_datadump")
            )),

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
  library(deformitet)
  library(tidyr)
  library(ggplot2)
  library(DT)
  library(shiny)
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

  RegData <- pre_pros(raw_regdata)

  ######## USER INFO--------------------------------------------------------------

  # Make a df that can be used for mapping between resh-ids and hospital names
  # Must be organized as df with two columns: UnitId and orgname
  # in order for navbarWidgetServer2 to work properly

  # map_db_resh <- RegData %>%
  #   dplyr::select(Sykehus, CENTREID) %>% # select required columns
  #   unique() %>% # keep only unique variables
  #   mutate(UnitId = CENTREID, # make new column with new name
  #          orgname = Sykehus) %>% # make new column with new name
  #   select(-c(Sykehus, CENTREID)) # take out old columns

  map_db_resh <- data.frame(  #map_avdeling <-
    UnitId = unique(RegData$CENTREID),
    orgname = RegData$Sykehus[match(unique(RegData$CENTREID),
                                   RegData$CENTREID)])

  user <- rapbase::navbarWidgetServer2(id = "deformitetNavbarWidget",
                                      orgName = "deformitet",
                                      caller = "deformitet",
                                      map_orgname = shiny::req(map_db_resh))


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

  deformitet::module_fordeling_server("fordeling",
                                      data = RegData,
                                      raw_data = raw_regdata,
                                      userRole = user$role,
                                      userUnitId = user$org(),
                                      map_data = map_db_resh)

  ################################################################################
  ##### TAB: gjennomsnitt ########################################################


  # deformitet::module_gjennomsnitt_server("gjen1",
  #                                        data = RegData,
  #                                        userRole = user$role,
  #                                        userUnitId = user$org,
  #                                        map_data = map_db_resh)

  ################################################################################
  ##### TAB: Kvalitetsindikatorer ################################################


  deformitet::module_kvalitetsindikator_server("kval1",
                                               data = RegData,
                                               map_data = map_db_resh,
                                               userRole = user$role,
                                               userUnitId = user$org)

  ################################################################################
  ##### TAB: Sammenligning #####################################################


  deformitet::module_sammenligning_server("sam1",
                                          data = RegData,
                                          userRole = user$role,
                                          userUnitId = user$org)

  ################################################################################
  ##### TAB: Registreringer #####################################################


  deformitet::module_registreringer_server("reg1",
                                          data = RegData,
                                          userRole = user$role,
                                          userUnitId = user$org())

  ################################################################################
  ##### TAB: SPC #################################################################

  # Add ready-made module here if requested by registry


  ################################################################################
  ##### TAB: Nestlasting av datadump #############################################


  deformitet::module_datadump_server("mod_datadump",
                                     data = RegData,
                                     userRole = user$role,
                                     userUnitId = user$org())

  ################################################################################
  ###### TAB: Exporting data #####################################################

  shiny::observeEvent(
    shiny::req(user$role()), {
      if (user$role() != "SC") {
        shiny::hideTab("tabs", target = "Eksport")
      } else {
        shiny::showTab("tabs", target = "Eksport")
      }
    })

  # Brukerkontroller

  rapbase::exportUCServer("deformitetExport", "deformitet")

  # Veiledning

  rapbase::exportGuideServer("deformitetExportGuide", "deformitet")


}
