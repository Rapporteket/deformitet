#'@title Module fordeling
#'@export

module_fordeling_UI <- function (id) {
  ns <- NS(id)
  shiny::tagList(
      shiny::sidebarLayout(

        # Inputs: select variables to plot
        shiny::sidebarPanel(
          width = 3,


          # Select variable for x-axis
          selectInput( # First select
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


          selectInput( # second select
            inputId = ns("kjønn_var"),
            label = "Utvalg basert på kjønn",
            choices = c("begge", "mann", "kvinne"),
            selected = "begge"),

          #shinyWidgets::chooseSliderSkin("Flat", color = "#112446"),
          sliderInput( # fourth select
            inputId = ns("alder_var"),
            label = "Aldersintervall:",
            min = 0,
            max = 100,
            value = c(10, 20),
            dragRange = TRUE),

          shinyjs::hidden(uiOutput(outputId = ns('reshid'))),

          radioButtons( # sixth select
            inputId = ns("type_op"),
            label = "Type operasjon",
            choices = c("Primæroperasjon", "Reoperasjon", "Begge"),
            selected = "Primæroperasjon"
            ),

          shinyjs::hidden(uiOutput(outputId = ns('view_type'))),

          dateRangeInput( # third select
            inputId = ns("date"),
            label = "Tidsintervall:",
            start = "2023-01-02",
            end = "2024-09-02",
            min = "2023-01-01",
            max = "2025-09-02",
            format = "dd-mm-yyyy",
            separator = " - ")
          ),



    mainPanel(
      tabsetPanel(id = ns("tab"),
                  tabPanel("Figur", value = "fig",
                           plotOutput(outputId = ns("my_plot"), height = "auto"),
                           downloadButton(ns("download_fordelingsfig"),
                                          "Last ned figur")),
                  tabPanel("Tabell", value = "tab",
                           bslib::card_body(
                             bslib::card_header(
                               textOutput(outputId = ns("title_table")
                               )
                             )
                           ),
                           DT::DTOutput(outputId = ns("table")),
                           downloadButton(ns("download_fordelingstbl"),
                                          "Last ned tabell")
                  ),
                  tabPanel("Frekvens", value = "freq",
                           DT::DTOutput(outputId = ns("freq_table")),
                           downloadButton(ns("download_fordelingsfreqtable"),
                                          "Last ned tabell"),
                           bslib::card_body(
                             bslib::card_title("Om frekvenstabellen"),
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


      # Define constants for complication types
      COMPLICATION_TYPES <- c("Komplikasjonstype", "Komplikasjonstype_12mnd", "Komplikasjonstype_60mnd")

      output$reshid <- renderUI({
        ns <- session$ns
        if (userRole() == 'SC') { # fifth select
          shiny::selectInput(
            inputId = ns("reshId_var"),
            label = "Enhet",
            choices = c("Haukeland" = 111961, "Rikshospitalet" = 103240, "St.Olav" = 102467),
            selected = "Haukeland"
          )
        }
      })

      output$view_type <- renderUI({
        ns <- session$ns
        if(userRole() == 'SC') {
          shiny::radioButtons( # seventh select
            inputId = ns("type_view"),
            label = "Vis rapport for:",
            choices = c("Hele landet" = "hele landet",
                        "Hele landet, uten sammenligning" = "hele landet, uten sammenligning",
                        "Hver enhet" = "hver enhet",
                        "Egen enhet" = "egen enhet"
            ))
        } else {
          shiny::radioButtons( # seventh select
            inputId = ns("type_view"),
            label = "Vis rapport for:",
            choices = c("Hele landet" = "hele landet",
                        "Hele landet, uten sammenligning" = "hele landet, uten sammenligning",
                        "Egen enhet" = "egen enhet"
            ))
        }
      })

      prepVar_reactive <- reactive({
        deformitet::prepVar(
          data,
          input$x_var,
          input$kjønn_var,
          input$date[1],
          input$date[2],
          input$alder_var[1],
          input$alder_var[2],
          input$type_op
        )
      })

      # Make data frame where UI choices are stored

      ### ALSO DOCUMENT ALL ACCESS EACH USER ROLE HAS

      my_data_reactive <- reactive({
        x <- format(input$date, "%d/%m/%y")
        my_data <- data.frame(c(input$x_var, input$kjønn_var, x[1], x[2], input$alder_var[1], input$alder_var[2], input$type_op))
      })


      # prepVar() returns a list
      # Unpack part 1 of list: data

      data_reactive <- reactive({
        data <- data.frame(prepVar_reactive()[1])
      })

      # Unpack part 2 of list: gg-data

      gg_data_reactive <- reactive({
        gg_data <- data.frame(prepVar_reactive()[2])
      })


      ######## AGGREGATE DATA-------------------------------------------------------

      #Aggregate data in table format

      table_reactive <- reactive({
       if (userRole() == 'SC') {
         reshid = input$reshId_var
       } else {
         reshid = userUnitId()
       }
        req(input$type_view)
        deformitet::makeTable(data_reactive(), reshid, input$type_view)
      })


        # First get a dataset based on UI choices

        kompl_data_reative <- reactive({
          deformitet::kompl_data(data,
                                 input$x_var,
                                 input$kjønn_var,
                                 input$date[1],
                                 input$date[2],
                                 input$alder_var[1],
                                 input$alder_var[2],
                                 input$type_op,
                                 map_data)
      })

      # Next, get a dataset with number of patients who have registered whether
      # or not they have had a complication (to calculate rates)

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
            input$date[1],
            input$date[2],
            input$alder_var[1],
            input$alder_var[2],
            input$type_op
          )

          data <- data.frame(data_prep[1])
          })

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
            input$type_view,
            reshid
            )
          })



      ########### DISPLAY DATA-------------------------------------------------------

      ### TABLE

      ###### FIKSE SÅ JEG KAN OGSÅ SE KOMPLIKASJONSTYPER ETTER 12 MNDer!!

      text_reactive <- reactive({
        if (! input$x_var %in% c("Komplikasjonstype", "Komplikasjonstype_12mnd")) {
          gg_data_4tbl <- data.frame(prepVar_reactive()[2])
          gg_data_4tbl$title
        } else {
          if (input$x_var == "Komplikasjonstype") {
            "Selvrapportert komplikasjonstype 3-6 måneders oppfølging"
          } else {
            "Selvrapportert komplikasjonstype 12 måneders oppfølging"
          }
        }
      })

      output$title_table <- renderText(
       text_reactive()
      )

      table <- reactive ({
        if(input$x_var %in% COMPLICATION_TYPES) { # if "komplikasjonstype" is chosen, use kompl_reactive
          x <- kompl_tbl_reactive()
        }
        else{
          x <- table_reactive()
        }
      })


      output$table <- DT::renderDT({
        datatable(table())
      })

      ### FIGURE ###

      my_plot <- reactive ({
        if(input$x_var %in% COMPLICATION_TYPES) {
           deformitet::kompl_plot(kompl_tbl_reactive(),
                                      input$x_var,
                                      my_data_reactive())
                   }
        else{
          gg_data <- data.frame(gg_data_reactive())
          # Generate the plot using the selected table, ggplot data, and user input.
          # The `input$type_view` parameter determines the type of view (e.g., "Hele landet", "Egen enhet") for the plot.
          deformitet::makePlot_gg(table_reactive(),
                                  gg_data,
                                  my_data_reactive(),
                                  input$type_view)
        }
      })

      output$my_plot <- renderPlot({
        my_plot()
      }, width = 800, height = 600)

      ####### FREKVENSTABELL ##########################################################

      name_reactive <- reactive({
       name <- deformitet::mapping_old_name_new_name(raw_data, input$x_var)
       })

      freq_added_reactive <- reactive({

        if (input$x_var %in% c("Alder", "Knivtid", "Diff_prosent_kurve")) {

          freq_added <- deformitet::add_freq_var_to_dataframe(raw_data, data, input$x_var)

        } else {

          freq_added <- deformitet::add_freq_var_to_dataframe(raw_data, data, name_reactive())
        }

      })

      freq_prepVar_reactive <- reactive({
        deformitet::prepVar(
          freq_added_reactive(),
          "freq_var",
          input$kjønn_var,
          input$date[1],
          input$date[2],
          input$alder_var[1],
          input$alder_var[2],
          input$type_op
          )
        })


      freq_data_reactive <- reactive({
        data <- data.frame(freq_prepVar_reactive()[1])
      })
      # prepVar() returns a list
      # Unpack part 1 of list: data

      freq_table_reactive <- reactive ({
        if (input$x_var %in% COMPLICATION_TYPES) {
          freq_data <- data.frame("Variabel" = "Det er ikke mulig å regne gjennomsnitt for denne variabelen")
      } else {
        freq_data <- deformitet::make_freq_table(freq_data_reactive())
      }
      })

      output$freq_table <- DT::renderDT({
        ns <- session$ns
        datatable(freq_table_reactive())
      })


      # ##### NEDLASTING ###############################################################
      output$download_fordelingsfig <-  downloadHandler(
        filename = function(){
          paste("Figur_", input$x_var,"_", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file){
          pdf(file, onefile = TRUE, width = 15, height = 9)
          plot(my_plot())
          dev.off()
        }
      )

      output$download_fordelingstbl <- downloadHandler(
        filename = function(){
          paste("Tabell_", input$x_var, "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file){
          write.csv(table(), file)
        }
      )

      output$dowload_fordelingsfreqtable <- downloadHandler(
        filename = function(){
          paste("Frekvenstabell_", input$x_var, "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file){
          write.csv(freq_table_reactive(), file)
        }
      )
    }
    )
}


