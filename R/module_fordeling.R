#'@title Fordelingsmodul
#'@export

module_fordeling_UI <- function (id) {
  ns <- NS(id)
  shiny::tagList(
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          width = 3,


          # Velg variabel for x-aksen
          selectInput( # Første valg
            inputId = ns("x_var"),
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
                        "Komplikasjoner, 12 mnd" = "Komplikasjoner_12mnd",
                        #"Komplikasjoner, 60 mnd" = "Komplikasjoner_60mnd",
                        "Komplikasjonstyper, 3-6 mnd" = "Komplikasjonstype",
                        "Komplikasjonstyper, 12 mnd" = "Komplikasjonstype_12mnd"
                        #"Komplikasjonstyper, 60 mnd" = "Komplikasjonstype_60mnd"
            ),
            selected = "BMI_kategori"),


          selectInput( # andre valg
            inputId = ns("kjønn_var"),
            label = "Utvalg basert på kjønn",
            choices = c("begge", "mann", "kvinne"),
            selected = "begge"),

          sliderInput( # tredje valg
            inputId = ns("alder_var"),
            label = "Aldersintervall:",
            min = 0,
            max = 100,
            value = c(10, 20),
            dragRange = TRUE),

          shinyjs::hidden(uiOutput(outputId = ns('reshid'))),

          radioButtons( # fjerde valg
            inputId = ns("type_op"),
            label = "Type operasjon",
            choices = c("Primæroperasjon", "Reoperasjon", "Begge"),
            selected = "Primæroperasjon"
            ),

          shinyjs::hidden(uiOutput(outputId = ns('visning_type'))),

          dateRangeInput( # femte valg
            inputId = ns("dato"),
            label = "Tidsintervall:",
            start = "2023-01-02",
            end = "2026-09-02",
            min = "2023-01-01",
            max = "2026-09-02",
            format = "dd-mm-yyyy",
            separator = " - ")
          ),



    mainPanel(
      tabsetPanel(id = ns("tab"),
                  tabPanel("Figur", value = "fig",
                           plotOutput(outputId = ns("figur"), height = "auto"),
                           downloadButton(ns("download_fordelingsfig"),
                                          "Last ned figur")),
                  tabPanel("Tabell", value = "tab",
                           bslib::card_body(
                             bslib::card_header(
                               textOutput(outputId = ns("tittel_tabell")
                               )
                             )
                           ),
                           DT::DTOutput(outputId = ns("tabell")),
                           downloadButton(ns("download_fordelingstbl"),
                                          "Last ned tabell")
                  ),
                  tabPanel("Gjennomsnitt", value = "gjen",
                           DT::DTOutput(outputId = ns("gjen_tabell")),
                           downloadButton(ns("download_fordelingsgjentabell"),
                                          "Last ned tabell"),
                           bslib::card_body(
                             bslib::card_title("Om tabellen"),
                             bslib::card_body("Tabellen viser gjennomsnitt og median per sykehus og for hele landet.
                                              Bruker bestemmer selv hovedvariabel, kjønn, alder, type operasjon og tidsintervall
                                              som skal brukes i beregningen. Alle tilfeller av manglende verdier er tatt ut (både manglende
                                              registreringer av oppfølginger og tilfeller der pasienten enda ikke har vært til oppfølging). Antall
                                              pasienter som er inkludert i beregningen er oppgitt under 'antall'.")))
      )
   )
  )
  )
}




#'@title Server fordeling
#'
#'@export

module_fordeling_server <- function (id, userRole, userUnitId, data, raw_data, map_data) {
  moduleServer(
    id,
    function(input, output, session){


      # Definere konstant for komplikasjonstyper
      komplikasjon_typer <- c("Komplikasjonstype", "Komplikasjonstype_12mnd", "Komplikasjonstype_60mnd")

      output$reshid <- renderUI({
        ns <- session$ns
        if (userRole() == 'SC') { # sjette valg
          shiny::selectInput(
            inputId = ns("reshId_var"),
            label = "Enhet",
            choices = c("Haukeland" = 111961, "Rikshospitalet" = 103240, "St.Olav" = 102467),
            selected = "Haukeland"
          )
        }
      })

      output$visning_type <- renderUI({
        ns <- session$ns
        if(userRole() == 'SC') {
          shiny::radioButtons( # sjuende valg
            inputId = ns("visning_type"),
            label = "Vis rapport for:",
            choices = c("Hele landet" = "hele landet",
                        "Hele landet, uten sammenligning" = "hele landet, uten sammenligning",
                        "Hver enhet" = "hver enhet",
                        "Egen enhet" = "egen enhet"
            ))
        } else {
          shiny::radioButtons( # sjuende valg
            inputId = ns("visning_type"),
            label = "Vis rapport for:",
            choices = c("Hele landet" = "hele landet",
                        "Hele landet, uten sammenligning" = "hele landet, uten sammenligning",
                        "Egen enhet" = "egen enhet"
            ))
        }
      })

      # Klargjøring av data
      # data som sendes til modulen går gjennom prepVar()-funksjonen

      prepVar_reactive <- reactive({
        deformitet::prepVar(
          data,
          input$x_var,
          input$kjønn_var,
          input$dato[1],
          input$dato[2],
          input$alder_var[1],
          input$alder_var[2],
          input$type_op
        )
      })

      # Lagring av ui-valg i dataramme

      my_data_reactive <- reactive({
        x <- format(input$dato, "%d/%m/%y")
        my_data <- data.frame(c(input$x_var, input$kjønn_var, x[1], x[2], input$alder_var[1], input$alder_var[2], input$type_op))
      })


      # prepVar() returnerer ei liste
      # Pakk ut del 1 av lista: data som har blitt filtrert

      data_reactive <- reactive({
        data <- data.frame(prepVar_reactive()[1])
      })

      # Pakk ut del 2 av lista: gg-data - fine titler osv

      gg_data_reactive <- reactive({
        gg_data <- data.frame(prepVar_reactive()[2])
      })


      ######## Aggreger data ---------------------------------------------------

      # Alle variabler utenom komplikasjonstype
      # Lagre data i tabellformat - bruker funksjonen lagTabell()

      tabell_reactive <- reactive({
       if (userRole() == 'SC') {
         reshid = input$reshId_var
       } else {
         reshid = userUnitId()
       }
        req(input$visning_type)
        deformitet::lagTabell(data_reactive(), reshid, input$visning_type)
      })

      # Komplikasjonstyper:
      # Filtrer data

        kompl_data_reative <- reactive({
          deformitet::kompl_data(data,
                                 input$x_var,
                                 input$kjønn_var,
                                 input$dato[1],
                                 input$dato[2],
                                 input$alder_var[1],
                                 input$alder_var[2],
                                 input$type_op,
                                 map_data)
      })

        # Få oversikt over antall pr. komplikasjonstype:

        kompl_prepVar_reactive <- reactive({
          if (input$x_var == "Komplikasjonstype") {
            var = "Komplikasjoner_3mnd"
          } else {
            if (input$x_var == "Komplikasjonstype_12mnd") {
              var = "Komplikasjoner_12mnd"
            } else {
              var = "Komplikasjoner_60mnd"
            }
          }

          data_prep <- deformitet::prepVar(
            data,
            var,
            input$kjønn_var,
            input$dato[1],
            input$dato[2],
            input$alder_var[1],
            input$alder_var[2],
            input$type_op
          )

          data <- data.frame(data_prep[1])
          })

        # Lagre det i tabellformat:

        kompl_tbl_reactive <- reactive({
            if (userRole() == 'SC') {
              reshid = input$reshId_var
            } else {
              reshid = userUnitId()
            }

          deformitet::kompl_tbl(
            kompl_prepVar_reactive(),
            kompl_data_reative(),
            input$kjønn_var,
            input$visning_type,
            reshid
            )
          })



      ########### VIS DATA -----------------------------------------------------

        ### Tabell
        # Tittel på tabellen:

      text_reactive <- reactive({
        if (! input$x_var %in% c("Komplikasjonstype", "Komplikasjonstype_12mnd")) {
          gg_data_4tbl <- data.frame(prepVar_reactive()[2])
          gg_data_4tbl$tittel
        } else {
          if (input$x_var == "Komplikasjonstype") {
            "Selvrapportert komplikasjonstype 3-6 måneders oppfølging"
          } else {
            "Selvrapportert komplikasjonstype 12 måneders oppfølging"
          }
        }
      })

      output$tittel_tabell <- renderText(
       text_reactive()
      )

      # Lag tabellen:

      tabell <- reactive ({
        if(input$x_var %in% komplikasjon_typer) { # hvis "komplikasjonstype" er valgt, bruk kompl_reactive()
          x <- kompl_tbl_reactive()
        }
        else{
          x <- tabell_reactive()
        }
      })

      # Vis tabellen:

      output$tabell <- DT::renderDT({
        datatable(tabell())
      })

      ### FIGUR ###
      # Lag figuren:

      figur <- reactive ({
        if(input$x_var %in% komplikasjon_typer) {
           deformitet::kompl_plot(kompl_tbl_reactive(),
                                      input$x_var,
                                      my_data_reactive())
                   }
        else{
          gg_data <- data.frame(gg_data_reactive())
          deformitet::lag_ggplot_fordeling(tabell_reactive(),
                                  gg_data,
                                  my_data_reactive(),
                                  input$visning_type)
        }
      })

      # Vis figuren:

      output$figur <- renderPlot({
        figur()
      }, width = 800, height = 600)

      ####### Gjennomsnitt #####################################################

      # Tabell som viser gjennomsnitt
      # Denne bruker rådataen som IKKE har vært gjennom preprosessering

      # Finne variabelen som bruker velger i datasettet:

      navn_reactive <- reactive({
       navn <- deformitet::mapping_navn(raw_data, input$x_var)
       })

      gjen_added_reactive <- reactive({

        if (input$x_var %in% c("Alder", "Knivtid", "Diff_prosent_kurve")) {

          gjen_added <- deformitet::gjen_var_til_data(raw_data, data, input$x_var)

        } else {

          gjen_added <- deformitet::gjen_var_til_data(raw_data, data, navn_reactive())
        }

      })

      # Filtrer basert på brukerens:

      gjen_prepVar_reactive <- reactive({
        deformitet::prepVar(
          gjen_added_reactive(),
          "gjen_var",
          input$kjønn_var,
          input$dato[1],
          input$dato[2],
          input$alder_var[1],
          input$alder_var[2],
          input$type_op
          )
        })

      # Pakk ut lista som returnerers av prepVar()

      gjen_data_reactive <- reactive({
        data <- data.frame(gjen_prepVar_reactive()[1])
      })

      # Lag tabell:

      gjen_tabell_reactive <- reactive ({
        gjen_data <- deformitet::lag_gjen_tabell(gjen_data_reactive())
      })

      # Vis tabell:

      output$gjen_tabell <- DT::renderDT({
        ns <- session$ns
        datatable(gjen_tabell_reactive())
      })


      ###### NEDLASTING ########################################################
      # Figur:
      output$download_fordelingsfig <-  downloadHandler(
        filename = function(){
          paste("Figur_", input$x_var,"_", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file){
          pdf(file, onefile = TRUE, width = 15, height = 9)
          plot(figur())
          dev.off()
        }
      )

      # Fordelingstabell:
      output$download_fordelingstbl <- downloadHandler(
        filename = function(){
          paste("Tabell_", input$x_var, "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file){
          write.csv(tabell(), file)
        }
      )

      # Tabell nr. 2:
      output$dowload_fordelingsgjentabell <- downloadHandler(
        filename = function(){
          paste("Gjennomsnittstabell_", input$x_var, "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file){
          write.csv(gjen_tabell_reactive(), file)
        }
      )
    }
    )
}
