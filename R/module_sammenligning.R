# Modul for sammenligning av promdata over tid
# Modulen lag brukeren gjøre en rekke valg for datasettet, samt velge hvordan
# dataen skal fremstilles. Dataene kan fremstilles med boxplot og som density plot


#'@title Ui sammenligningsmodul
#'
#'@export

module_sammenligning_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(
          inputId = ns("sam_var"),
          label = "Velg variabel",
          choices = c(
            "SRS22 totalskår",
            "Funksjon",
            "Selvbilde",
            "Mental helse",
            "Smerte",
            "Helsetilstand",
            "Tilfredshet"
          ),
          selected = "SRS22 totalskår"),

        shiny::selectInput(
          inputId = ns("plot_valg"),
          label = "Velg plot-type",
          choices = c(
            "Tetthetsplot",
            "Boksplot"
          ),
          selected = "Boksplot"
        ),

        # Vises kun hvis tetthetsplot er valgt og "tilfredshet" er valgt
        shiny::conditionalPanel( # Panel som kun vises om "Tetthetsplot" velges
          condition = "input.plot_valg == 'Tetthetsplot' && input.sam_var == 'Tilfredshet'",
          shiny::selectInput(
            inputId = ns("valg_sammenligning"),
            label = "Velg sammenligning",
            choices = c(
              "3 mnd - 12 mnd",
              "3 mnd - 5 år",
              "12 mnd - 5 år"
            )
          ),
          selected = "Før operasjon - 3 mnd",
          ns = ns(id)
        ),

        # Vises kun hvis tetthetsplot er valgt og "tilfredshet" ikke er valgt
        shiny::conditionalPanel( # Panel som kun vises om "Tetthetsplot" velges
          condition = "input.plot_valg == 'Tetthetsplot' && input.sam_var != 'Tilfredshet'",
          shiny::selectInput(
            inputId = ns("valg_sammenligning"),
            label = "Velg sammenligning",
            choices = c(
              "Før operasjon - 3 mnd",
              "Før operasjon - 12 mnd",
              "Før operasjon - 5 år",
              "3 mnd - 12 mnd",
              "3 mnd - 5 år",
              "12 mnd - 5 år"
            )
          ),
          selected = "Før operasjon - 3 mnd",
          ns = ns(id)
        ),

        shiny::radioButtons(
          inputId = ns("kjoenn_var"),
          label = "Dele på kjønn?",
          choices = c(
            "kvinne" = "kvinne",
            "mann" = "mann",
            "begge" = "begge"
          ),
          selected = "begge"
        ),

        shiny::sliderInput(
          inputId = ns("alder_var"),
          label = "Aldersintervall:",
          min = 0,
          max = 100,
          value = c(10, 20),
          dragRange = TRUE
        ),

        shinyjs::hidden(shiny::uiOutput(outputId = ns("reshid"))),

        shiny::dateRangeInput(
          inputId = ns("dato"),
          label = "Tidsintervall:",
          start = "2023-01-02",
          end = "2025-12-12",
          min = "2023-01-01",
          max = "2026-12-12",
          format = "dd-mm-yyyy",
          separator = " - "
        )
      ),

      shiny::mainPanel(
        bslib::navset_card_underline(
          bslib::nav_panel(
            "Figur",
            shiny::plotOutput(outputId = ns("sam_plot")),
            shiny::downloadButton(ns("nedlastning_sam_plot"), "Last ned figur"),
          ),
          bslib::nav_panel(
            "Tabell",
            DT::DTOutput(outputId = ns("konf_tbl"))
          )
        )
      )
    )
  )
}

#'@title Server sammenligningsmodul
#'
#'@export

module_sammenligning_server <- function(id, data, userRole, userUnitId) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      reshid <- shiny::reactiveValues(reshId_var = 111961) # Lagre dette som en reaktiv verdi


      output$reshid <- shiny::renderUI({ # Åttende valg som kun vises dersom brukeren har SC-rolle
        ns <- session$ns
        if (userRole() == "SC") {
          shiny::selectInput(
            inputId = ns("reshId_var"),
            label = "Enhet",
            choices = c(
              "Haukeland" = 111961,
              "Rikshospitalet" = 103240,
              "St.Olav" = 102467
            ),
            selected = "Haukeland"
          )
        }
      })

      shiny::observe({ # Dersom brukerens rolle endrer seg endrer også den reaktive verdien seg
        shiny::req(input$reshId_var)
        reshid$reshId_var <- input$reshId_var
      })


      ##### Gjør hovedutvalg av dataen #########################################

      data_sam_reactive <- shiny::reactive({

        if (userRole() == "SC") {
          x <- deformitet::utvalg_basic(
            data,
            reshid$reshId_var,
            input$kjoenn_var,
            "Primæroperasjon",
            input$dato[1],
            input$dato[2],
            input$alder_var[1],
            input$alder_var[2],
            "filtrer_reshId"
          )
        } else {
          x <- deformitet::utvalg_basic(
            data,
            userUnitId(),
            input$kjoenn_var,
            "Primæroperasjon",
            input$dato[1],
            input$dato[2],
            input$alder_var[1],
            input$alder_var[2],
            "filtrer_reshId"
          )
        }
      })

      # Lagre brukervalg i et datasett
      brukervalg_reactive <- shiny::reactive({
        x <- format(input$dato, "%d/%m/%y")
        brukervalg <- tidyr::tibble(
          variabel = input$sam_var,
          kjoenn = input$kjoenn_var,
          dato1 = x[1],
          dato2 = x[2],
          alder1 = input$alder_var[1],
          alder2 = input$alder_var[2],
          "Primæroperasjon"
        )
        return(brukervalg)
      })


      # Lag tabell til sammenligning
      sam_tabell_reactive <- shiny::reactive({
        deformitet::lag_sam_tabell(data_sam_reactive(), input$sam_var)
      })

      # Lag nye navn
      nye_navn_reactive <- shiny::reactive({
        deformitet::nye_navn(sam_tabell_reactive())
      })

      # Vask datasett
      ren_sam_tabell_reactive <- shiny::reactive({
        deformitet::vask_sam_tabell(nye_navn_reactive(), input$sam_var)
      })

      # Finn konfidensintervall for variabler til density plot
      sam_finn_konf_reactive <- shiny::reactive({
        deformitet::finn_sam_konfidensint(ren_sam_tabell_reactive())
      })

      # Lag fine navn til ggplot
      gg_data_sam_reactive <- shiny::reactive({
        deformitet::ggdata_sam_plot(input$sam_var)
      })

      # Finn variabler til density plot
      sam_variabler_reactive <- shiny::reactive({
        deformitet::finn_sam_variabler(ren_sam_tabell_reactive(), input$valg_sammenligning)
      })


      # Lag plot
      sam_plot_reactive <- shiny::reactive({
        if (input$plot_valg == "Boksplot") {
          deformitet::boxplot_sam(ren_sam_tabell_reactive(), gg_data_sam_reactive(), brukervalg_reactive())
        } else {
          deformitet::density_sam(sam_variabler_reactive(), gg_data_sam_reactive(), brukervalg_reactive())
        }
      })

      # Render plot
      output$sam_plot <- shiny::renderPlot({
        sam_plot_reactive()
      })

      output$konf_tbl <- DT::renderDataTable({
        sam_finn_konf_reactive()
      })

      # Lag nedlastning
      output$nedlastning_sam_plot <-  shiny::downloadHandler(
        filename = function() {
          paste("plot_sammenligning", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file) {
          pdf(file, onefile = TRUE, width = 15, height = 9)
          plot(sam_plot_reactive())
          dev.off()
        }
      )


    }
  )
}
