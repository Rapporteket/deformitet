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

      shiny::tabPanel( # Second tab
        title = "Fordelingsfigur og -tabell",
        shiny::sidebarLayout(

          # Inputs: select variables to plot
          shiny::sidebarPanel(
            width = 3,


            # Select variable for x-axis
            selectInput( # First select
              inputId = "x_var",
              label = "Variabel:",
              choices = c("Helsetilstand" = "Helsetilstand",
                          "Helsetilstand 3-6 mnd" = "Helsetilstand_3mnd",
                          "Helsetilstand 12 mnd" = "Helsetilstand_12mnd",
                          #"Helsetilstand 5 år" = "Helsetilstand_60mnd",
                          "SRS22 'Samme behandling på nytt?' 3-6 mnd" = "SRS22_spm22_3mnd",
                          "SRS22 'Samme behandling på nytt?' 12 mnd" = "SRS22_spm22_12mnd",
                          #"SRS22 'Samme behandling på nytt?' 5 år" = "SRS22_spm22_60mnd",
                          "SRS22 'Fornøyd med resultatet?' 3-6 mnd" =  "SRS22_spm21_3mnd",
                          "SRS22 'Fornøyd med resultatet?' 12 mdn" = "SRS22_spm21_12mnd",
                          #"SRS22 'Fornøyd med resultatet?' 5 år" = "SRS22_spm21_60mnd",
                          "BMI-kategori" = "BMI_kategori",
                          "Alder" = "Alder",
                          "Pre-operativ kurve" = "Kurve_pre",
                          "Post-operativ kurve" = "Kurve_post",
                          "Prosent korreksjon kurve" = "Diff_prosent_kurve",
                          "Liggetid" = "Liggetid",
                          "Knvitid" = "Knivtid",
                          "Blodtap pr. 100 ml" = "Blodtap_100",
                          "Blodtap pr. 200 ml" = "Blodtap_200",
                          "SRS22 totalscore preoperativt" = "SRS22_total",
                          "SRS22 totalscore 3-6 mnd" = "SRS22_total_3mnd",
                          "SRS22 totalscore 12 mnd" = "SRS22_total_12mnd",
                          #"SRS22 totalscore 5 år" = "SRS22_total_60mnd",
                          "SRS22 funksjon preoperativt" = "SRS22_funksjon",
                          "SRS22 funksjon, 3-6 mnd" = "SRS22_funksjon_3mnd",
                          "SRS22 funksjon, 12 mnd" = "SRS22_funksjon_12mnd",
                          #"SRS22 funksjon, 5 år" = "SRS22_funksjon_60mnd",
                          "SRS22 smerte preoperativt" = "SRS22_smerte",
                          "SRS22 smerte, 3-6 mnd" = "SRS22_smerte_3mnd",
                          "SRS22 smerte, 12 mnd" = "SRS22_smerte_12mnd",
                          #"SRS22 smerte, 5 år" = "SRS22_smerte_60mnd",
                          "SRS22 selvbilde preoperativt" = "SRS22_selvbilde",
                          "SRS22 selvbilde, 3-6 mnd" = "SRS22_selvbilde_3mnd",
                          "SRS22 selvbilde, 12 mnd" = "SRS22_selvbilde_12mnd",
                          #"SRS22 selvbilde, 5 år" = "SRS22_selvbilde_60mnd",
                          "SRS22 mental helse preoperativt" = "SRS22_mhelse",
                          "SRS22 mental helse, 3-6 mnd" = "SRS22_mhelse_3mnd",
                          "SRS22 mental helse, 12 mnd" = "SRS22_mhelse_12mnd",
                          #"SRS22 mental helse, 5 år" = "SRS22_mhelse_60mnd",
                          "SRS22 tilfredshet, 3-6 mnd" = "SRS22_fornoyd_3mnd",
                          "SRS22 tilfredshet, 12 mnd" = "SRS22_fornoyd_12mnd",
                          #"SRS22 tilfredshet, 5 år" = "SRS22_fornoyd_60mnd",
                          "Komplikasjoner, 3-6 mnd" = "Komplikasjoner_3mnd",
                          "Komplikasjoner, 12 mnd" = "Komplikasjoner_12mnd"
                          #"Komplikasjoner, 60 mnd" = "Komplikasjoner_60mnd",
                          #"Komplikasjonstyper, 3-6 mnd" = "Komplikasjonstype",
                          #"Komplikasjonstyper, 12 mnd" = "Komplikasjonstype_12mnd"
                          #"Komplikasjonstyper, 60 mnd" = "Komplikasjonstype_60mnd"
              ),
              selected = "BMI_kategori"),


            selectInput( # second select
              inputId = "kjønn_var",
              label = "Utvalg basert på kjønn",
              choices = c("begge", "mann", "kvinne"),
              selected = "begge"),

            #shinyWidgets::chooseSliderSkin("Flat", color = "#112446"),
            sliderInput( # fourth select
              inputId = "alder_var",
              label = "Aldersintervall:",
              min = 0,
              max = 100,
              value = c(10, 20),
              dragRange = TRUE),

            shinyjs::hidden(uiOutput(outputId = 'reshid')),

            radioButtons( # sixth select
              inputId = "type_op",
              label = "Type operasjon",
              choices = c("Primæroperasjon", "Reoperasjon", "Begge"),
              selected = "Primæroperasjon"
            ),

            shinyjs::hidden(uiOutput(outputId = 'view_type')),

            dateRangeInput( # third select
              inputId = "date",
              label = "Tidsintervall:",
              start = "2023-01-02",
              end = "2024-09-02",
              min = "2023-01-01",
              max = "2025-09-02",
              format = "dd-mm-yyyy",
              separator = " - ")
            ),


            # Output: Show plot
            mainPanel(
              bslib::navset_card_underline(
                title = "Visualiseringer",
                bslib::nav_panel("Figur", plotOutput(outputId = "plot")),
                bslib::nav_panel("Tabell", DT::DTOutput(outputId = "table"))
              )
            )
          )
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
