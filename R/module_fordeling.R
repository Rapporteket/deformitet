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
                        "Komplikasjoner, 12 mnd" = "Komplikasjoner_12mnd"
                        #"Komplikasjoner, 60 mnd" = "Komplikasjoner_60mnd",
                        #"Komplikasjonstyper, 3-6 mnd" = "Komplikasjonstype",
                        #"Komplikasjonstyper, 12 mnd" = "Komplikasjonstype_12mnd"
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

        # Output: Show plot
        mainPanel(
          bslib::navset_card_underline(
            title = "Visualiseringer",
            bslib::nav_panel("Figur",
                             shiny::plotOutput(outputId = ns("plot")),
                             bslib::card_body(
                               shiny::downloadButton(ns("download_fordelingsfig"), "Last ned figur"))),
            bslib::nav_panel("Tabell",
                             DT::DTOutput(outputId = ns("table")),
                             bslib::card_body(
                               shiny::downloadButton(ns("download_fordelingstbl"), "Last ned tabell"))),
            bslib::nav_panel("Frekvenstabeller",
                             DT::DTOutput(outputId = ns("freq_table")),
                             bslib::card_body(
                               shiny::downloadButton(ns("dowload_fordelingsfreqtable"), "Last ned frekvenstabell")),
                             bslib::card_body(
                               bslib::card_title("Om frekvenstabellen"),
                               bslib::card_body("Tabellen viser gjennomsnitt pr. sykehus og for hele landet.
                                                Bruker bestemmer selv hovedvariabel, kjønn, alder, type operasjon og tidsintervall
                                                som skal brukes i beregningen. Alle tilfeller av manglende verdier er tatt ut (både manglende
                                                registreringer av oppfølginger og tilfeller der pasienten enda ikke har vært til oppfølging).")
                             ))
          )

    )
   )
  )

}


#'@title Server sammenligningsmodul
#'
#'@export

module_fordeling_server <- function (id, userRole, userUnitId, data, raw_data) {
  moduleServer(
    id,
    function(input, output, session){


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
        deformitet::makeTable(data_reactive(), reshid, input$type_view)
      })

      # Make table of komplikasjonstyper
      ### Komplikasjonstyper is aggregated separately from the rest of the variables

      kompl_reactive <- reactive({
        if (userRole() == 'SC') {
          reshid = input$reshId_var
        } else {
          reshid = userUnitId()
        }
        test <- deformitet::kompl_data(data, reshid)
      })


      # ########## DISPLAY DATA-------------------------------------------------------

      ## TABLE

      output$table <- DT::renderDT({
        ns <- session$ns
        if(input$x_var == "Komplikasjonstype"){ # if "komplikasjonstype is chosen, use kompl_reactive
          kompl_reactive()
        }
        else{datatable(table_reactive())
        }
      })


      # ## FIGURE

      output$plot <- renderPlot({
        if(input$x_var != "Komplikasjonstype"){
          gg_data <- data.frame(gg_data_reactive())
          deformitet::makePlot_gg(table_reactive(),
                                  gg_data,
                                  my_data_reactive(),
                                  input$type_view)
        }
        else{
          gg_kompl <- data.frame(c("title" = "Operasjoner pr komplikasjonstype",
                                   "xlab" = "Komplikasjonstype"))
          deformitet::makePlot_gg(kompl_reactive(),
                                  gg_kompl,
                                  my_data_reactive(),
                                  input$type_view)}
      })

      ####### FREKVENSTABELL ##########################################################

      name_reactive <- reactive({
        name <- deformitet::mapping_old_name_new_name(raw_data, input$x_var)
      })

      freq_added_reactive <- reactive({
        freq_added <- deformitet::add_freq_var_to_dataframe(raw_data, data, name_reactive())
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


      # prepVar() returns a list
      # Unpack part 1 of list: data

      freq_data_reactive <- reactive({
        data <- data.frame(freq_prepVar_reactive()[1])
      })


      freq_table_reactive <- reactive({
        freq_data <- deformitet::make_freq_table(freq_data_reactive())
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
          plot(plot())
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




    }
    )
  }
