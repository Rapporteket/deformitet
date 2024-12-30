####### MODULE FOR KVALITETSINDIKATORER ########################################

#' @export

module_kvalitetsindikator_UI <- function(id){
  ns <- NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
      selectInput( # First select
        inputId = ns("kval_var"),
        label = "Velg Kvalitetsindikator:",
        choices = c("SRS22 'Samme behandling på nytt?' 3-6 mnd" = "SRS22_spm22_3mnd",
                    #"SRS22 'Samme behandling på nytt?' 12 mnd" = "SRS22_spm22_12mnd",
                    #"SRS22 'Samme behandling på nytt?' 5 år" = "SRS22_spm22_60mnd",
                    "Pre-operativ kurve" = "PRE_MAIN_CURVE",
                    #"Prosent korreksjon kurve" = "Diff_prosent_kurve",
                    "Liggetid" = "Liggetid",
                    "Komplikasjoner, 3-6 mnd" = "Komplikasjoner_3mnd"
        ),
        selected = "SRS22_spm22_3mnd"),


      shiny::radioButtons( # second select
        inputId = ns("kjønn_var"),
        label = "Dele på kjønn?",
        choices = c("kvinne",
                    "mann",
                    "begge",
                    "se alle fordelinger" = "nei"),
        selected = "nei"),

      shiny::radioButtons( # sixth select
        inputId = ns("type_op"),
        label = "Type operasjon",
        choices = c("Primæroperasjon", "Reoperasjon", "Begge"),
        selected = "Primæroperasjon"
      )),


      shiny::mainPanel(
        bslib::navset_card_underline(
          title = "Visualisering",
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
            p(
              shiny::textOutput(
                outputId = ns("text_body"))))
        )
      )
    )
  )
}


#' @export

module_kvalitetsindikator_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){

      # #### Read in data:
      # regdata <- deformitet::les_og_flate_ut()
      #
      # #### Clean and tidy data:
      #
      # regdata <- deformitet::pre_pros(regdata)

      regdata <- readRDS("../dev/fake_data_deformitet.rds")

      regdata <- regdata %>%
        dplyr::mutate(Sykehus =
                        dplyr::recode(Sykehus,
                                      "Bergen" = "Haukeland",
                                      "Riksen" = "Rikshospitalet"))


      date1_reactive <- reactive({
        date1 <- min(regdata$SURGERY_DATE)
      })

      date2_reactive <- reactive({
        date2 <- max(regdata$SURGERY_DATE)
      })

      age1_reactive <- reactive({
        age1 <- min(regdata$Alder_num)
      })

      age2_reactive <-reactive({
        age2 = max(regdata$Alder_num)
      })



      #     Use prepVar()-function to return a list
      prep_kval_reactive <- reactive({
        deformitet::prepVar(regdata,
                input$kval_var,
                input$kjønn_var,
                date1_reactive(), date2_reactive(),
                age1_reactive(), age2_reactive(),
                input$type_op)
      })


      # Make data frame out of list 2 - gg-data
      gg_data_reactive <- reactive({
        gg_data <- data.frame(prep_kval_reactive()[2])
      })


      # Store reactive choices in data set for caption in ggplot
      my_data_reactive <- reactive({
        #date <- format(c(date1_reactive(), date2_reactive(), "%d/%m/%y"))
        my_data <- data.frame(c(input$kval_var, if(input$kjønn_var == "nei"){"begge"}else{input$kjønn_var},
                                format(date1_reactive(), "%d%m%y"), format(date2_reactive(), "%d%m%y"),
                                age1_reactive(), age2_reactive(),
                                input$type_op))
      })


      ###### COUNT KVALTITETSINDIKATORER #############################################

      kval_df_reactive <- reactive({
        x <- deformitet::kval_count(regdata, input$kval_var, input$kjønn_var, input$type_op)
      })


      ###### PLOT ####################################################################

      kval_plot <- reactive({
        deformitet::kval_plot(kval_df_reactive(), gg_data_reactive(), my_data_reactive(), input$kjønn_var)
      })

      output$kval_plot <- renderPlot({
        #ggplot2::ggsave("test.test.pdf", kval_plot(), width = 2.5, length = 1.5, unit = "mm")
        kval_plot()
      })

      ##### TABLE ####################################################################

      output$kval_table <- DT::renderDT({datatable(kval_df_reactive(),
                                                   class = 'white-space:nowrap compact',
                                                   colnames = c("Sykehus",
                                                                "Kjønn",
                                                                "Antall nasjonalt",
                                                                "Antall per sykehus",
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

      output$text_header <- renderText({
        dplyr::case_match(input$kval_var,
                          "SRS22_spm22_3mnd" ~ "SRS22 'Samme behandling på nytt?' 3-6 mnd:",
                          "PRE_MAIN_CURVE"~ "Pre-operativ kurve:",
                          "Liggetid" ~ "Liggetid:",
                          "Komplikasjoner_3mnd" ~ "Komplikasjoner 3-6 mnd:")
      })

      output$text_body <- renderText({
        dplyr::case_match(input$kval_var,
                          "SRS22_spm22_3mnd" ~ "Antall pasienter som svarte 'definitivt ja' og 'sannsynligvis ja' / antall opererte *100",
                          "PRE_MAIN_CURVE"~ "Antall pasienter med preoperativ kurve > 70 grader / antall opererte *100",
                          "Liggetid" ~ "Antall pasienter med post-operativ liggetid 7 dager eller mer / antall opererte *100",
                          "Komplikasjoner_3mnd" ~ "Antall pasienter som rapporterer komplikasjoner ved 3-6 mnds oppfølging / antall opererte *100")
      })
    })
}



