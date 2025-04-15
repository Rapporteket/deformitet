#' Client (ui) for the deformitet app
#'
#' @return A shiny app ui object
#' @export

## LEGG INN EKSPORT I UI-biten

app_ui <- function() {
  shiny::addResourcePath("rap", system.file("www", package = "rapbase"))

  shiny::tagList( # Needed for "about the user" tags
    shiny::navbarPage( # type of page

      ###### Graphics ----------------------------------------------------------
      title = shiny::div(shiny::a(shiny::includeHTML(system.file('www/logo.svg', package = 'rapbase'))), # add the logo
                         "Rapporteket for deformitet"),
      windowTitle = "Rapporteket for deformitet",
      theme = "rap/bootstrap.css", # theme of app
      id = "tabs",

      ################################################################################
      ######## TAB user info----------------------------------------------------------

      ##### Startside (info about Rapporteket and the registry)-----------------------

      shiny::tabPanel( # First tab
        title = "Startside",
        shiny::mainPanel(
          width = 12,
          shiny::htmlOutput("veiledning", inline = TRUE), # load in the htmloutput wanted. This file is found in folder "inst"
          rapbase::navbarWidgetInput("deformitetNavbarWidget", selectOrganization = TRUE)
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
  title = "Datautvalg",
  shiny::fluidPage(
    deformitet::module_datadump_UI(
      id = "module_1")
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
