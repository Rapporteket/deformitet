####### MODULE FOR KVALITETSINDIKATORER ########################################
#'@title kvalitetsindikator UI
#'@export

module_kvalitetsindikator_UI <- function(id){
  ns <- NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
      selectInput( # First select
        inputId = ns("kval_var"),
        label = "Velg Kvalitetsindikator:",
        choices = c("SRS22 'Hvor fornøyd er du med behandlingen?' 3-6 mnd" = "SRS22_spm21_3mnd",
                    #"SRS22 'Hvor fornøyd er du med behandlingen?' 12 mnd" = "SRS22_spm21_12mnd",
                    #"SRS22 'Hvor fornøyd er du med behandlingen?' 60 mnd" = "SRS22_spm21_60mnd",
                    "Pre-operativ kurve" = "PRE_MAIN_CURVE",
                    #"Prosent korreksjon kurve" = "Diff_prosent_kurve",
                    "Liggetid" = "Liggetid",
                    "Komplikasjoner, 3-6 mnd" = "Komplikasjoner_3mnd",
                    # Komplikasjoner, 12 mnd
                    # Komplikasjoner, 5 år
                    "Reoperasjonsrate" = "CURRENT_SURGERY"
        ),
        selected = "SRS22_spm22_3mnd"),


      shiny::radioButtons( # second select
        inputId = ns("kjønn_var"),
        label = "Dele på kjønn?",
        choices = c("kvinne" = "kvinne",
                    "mann" = "mann",
                    "begge" = "begge",
                    "se alle fordelinger" = "nei"),
        selected = "begge"),


      conditionalPanel(
        condition = "input.kval_var == 'CURRENT_SURGERY'",
        shiny::radioButtons( # third select, not visible when looking at rate of re-operations
          inputId = ns("type_op"),
          label = "Type operasjon",
          choices = c("Begge"),
          selected = "Begge"),
        ns = NS(id)
        ),


      conditionalPanel(
        condition = "input.kval_var != 'CURRENT_SURGERY'",
        shiny::radioButtons( # third select, not visible when looking at rate of re-operations
          inputId = ns("type_op"),
          label = "Type operasjon",
          choices = c("Primæroperasjon", "Reoperasjon", "Begge"),
          selected = "Begge"),
        ns = NS(id)
      )),


      shiny::mainPanel(
        bslib::navset_card_underline(
          bslib::nav_panel("Figur",
                           shiny::plotOutput(outputId = ns("kval_plot")),
                           shiny::downloadButton(ns("download_fig"), "Last ned figur")),
          bslib::nav_panel("Tabell",
                           DT::DTOutput(outputId = ns("kval_table")),
                           shiny::downloadButton(ns("download_tbl"), "Last ned tabell", class = "butt2"))
          #bslib::nav_panel("Over tid", plotOutput(outputID = "kval_over_tid"))
        ),
        bslib::navset_card_underline(
          title = h4("Slik er kvalitetsindikatoren regnet ut:"),
          bslib::card_header(
            tags$em(
              shiny::textOutput(
                outputId = ns("text_header")))),
          bslib::card_body(
              shiny::htmlOutput(
                outputId = ns("text_body")))
        )
      )
    )
  )
}


#'@export

module_kvalitetsindikator_server <- function(id, data, userRole, userUnitId, map_data){
  moduleServer(
    id,
    function(input, output, session){


      date1_reactive <- reactive({
        date1 <- min(data$SURGERY_DATE)
      })

      date2_reactive <- reactive({
        date2 <- max(data$SURGERY_DATE)
      })

      age1_reactive <- reactive({
        age1 <- min(data$Alder_num)
      })

      age2_reactive <-reactive({
        age2 = max(data$Alder_num)
      })



      # Make gg-data for plot
      gg_data <- data.frame(title = "") # Jeg skal legge alt dette inn i en funksjon

      gg_data_reactive <- reactive({
        gg_data <- gg_data %>%
          dplyr::mutate(
            title = dplyr::case_match(
              input$kval_var,
              "SRS22_spm21_3mnd" ~ "Pasienter som har svart at de er fornøyd med behandlingen (3-6 mnd)",
              "PRE_MAIN_CURVE"~ "Pasienter med pre-operativ kurve over 70 grader",
              "Liggetid" ~ "Pasienter med 7 dager eller lengre liggetid",
              "Komplikasjoner_3mnd" ~ "Pasienter som har rapportert komplikasjoner (unntatt smerte) etter 3-6 måneder",
              "CURRENT_SURGERY" ~ "Andel pasienter som reopereres (reoperasjonsrate)"),

            ylab = dplyr::case_match(
              input$kval_var,
              "SRS22_spm21_3mnd" ~ "'Svært godt fornøyd' og 'ganske fornøyd' med behandlingen (3-6 mnd)",
              "PRE_MAIN_CURVE"~ "Pre-operativ kurve over 70 grader",
              "Liggetid" ~ "Liggetid 7 dager eller lengre",
              "Komplikasjoner_3mnd" ~ "Rapportert komplikasjoner 3-6 mnd (unntatt smerte)",
              "CURRENT_SURGERY" ~ "Reoperasjonsrate"),


            ymin = dplyr::case_match(
              input$kval_var,
              "SRS22_spm21_3mnd" ~ 70,
              "PRE_MAIN_CURVE"~ 0,
              "Liggetid" ~ 0,
              "Komplikasjoner_3mnd" ~ 0,
              "CURRENT_SURGERY" ~ 0),

            ymax = dplyr::case_match(
              input$kval_var,
              "SRS22_spm21_3mnd" ~ 100,
              "PRE_MAIN_CURVE"~ 10,
              "Liggetid" ~ 10,
              "Komplikasjoner_3mnd" ~ 10,
              "CURRENT_SURGERY" ~ 5)


          )
      })


      # Store reactive choices in data set for caption in ggplot
      my_data_reactive <- reactive({
        #date <- format(c(date1_reactive(), date2_reactive(), "%d/%m/%y"))
        my_data <- data.frame(c(input$kval_var,
                                if(input$kjønn_var == "nei"){"begge"}
                                else{input$kjønn_var},
                                format(date1_reactive(), "%d%m%y"),
                                format(date2_reactive(), "%d%m%y"),
                                age1_reactive(), age2_reactive(),
                                input$type_op))
      })


      ###### COUNT KVALTITETSINDIKATORER #############################################

      kval_df_reactive <- reactive({
        x <- deformitet::count_kvalind(data,
                                       input$kjønn_var,
                                       input$type_op,
                                       input$kval_var,
                                       userRole(),
                                       userUnitId(),
                                       map_data)
        })

      ###### PLOT ####################################################################

      kval_plot <- reactive({
        deformitet::kval_plot(kval_df_reactive(),
                              gg_data_reactive(),
                              my_data_reactive(),
                              input$kjønn_var)
      })

      output$kval_plot <- renderPlot({
        kval_plot()
      })

      ##### TABLE ####################################################################

      output$kval_table <- DT::renderDT({datatable(kval_df_reactive(),
                                                   class = 'white-space:nowrap compact',
                                                   colnames = c("Sykehus",
                                                                "Kjønn",
                                                                "Antall",
                                                                "Antall - kvalitetsindikator",
                                                                "Andel - kvalitetsindikator"))
        })

      ##### KVALITETSINDIKATORER over tid ############################################


      ##### NEDLASTING ###############################################################
      output$download_fig <-  downloadHandler(
        filename = function(){
          paste("Figur_", input$kval_var,"_", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file){
          pdf(file, onefile = TRUE, width = 15, height = 9)
          plot(kval_plot())
          dev.off()
        }
      )

      output$download_tbl <- downloadHandler(
        filename = function(){
          paste("Tabell_", input$kval_var, "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file){
          write.csv(kval_df_reactive(), file)
        }
      )

      #### RENDER TEXT ##############################################################
      output$text_header <- renderText({
        data <- explanation_kvalind(input$kjønn_var, input$kval_var)
        data$header
      })

      output$text_body <- renderText({
        data <- explanation_kvalind(input$kjønn_var, input$kval_var)
        data$text
      })
    })
}



