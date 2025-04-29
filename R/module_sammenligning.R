#'@title Ui sammenligningsmodul
#'
#'@export

module_sammenligning_UI <- function (id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        selectInput( # First select
          inputId = ns("comp1"),
          label = "Velg variabel 1 (før operasjon) eller ved 3mnd oppfølging:",
          choices = c("SRS22 totalskår" = "SRS22_MAIN_SCORE",
                      "SRS22 totalskår 3mnd" = "SRS22_FULL_SCORE",
                      "SRS22 mental helse" = "SRS22_MENTALHEALTH_SCORE",
                      "SRS22 mental helse 3mnd" = "SRS22_MENTALHEALTH_SCORE_patient3mths",
                      "SRS22 smerte" = "SRS22_PAIN_SCORE",
                      "SRS22 smerte 3mnd" = "SRS22_PAIN_SCORE_patient3mths",
                      "Pre-operativ kurve" = "PRE_MAIN_CURVE",
                      "Helsetilstand" = "Helsetilstand",
                      "Helsetilstand 3mnd" = "Helsetilstand_3mnd"),
          selected = "SRS22_MAIN_SCORE"),

        conditionalPanel(
          condition =
            "input.comp1 == 'SRS22_MAIN_SCORE' ||
            input.comp1 == 'SRS22_FULL_SCORE' ||
            input.comp1 == 'SRS22_MENTALHEALTH_SCORE' ||
            input.comp1 == 'SRS22_MENTALHEALTH_SCORE_patient3mths' ||
            input.comp1 == 'SRS22_PAIN_SCORE' ||
            input.comp1 == 'SRS22_PAIN_SCORE_patient3mths'",
          ns = ns,

          selectInput(ns("comp2"),
                      label = "Velg variabel 2 (etter operasjon - 3-60 mnd oppfølging):",
                      choices = c("SRS22 totalskår 3mnd" =
                                    "SRS22_FULL_SCORE",
                                  "SRS22 totalskår 12mnd" =
                                    "SRS22_FULL_SCORE_patient12mths",
                                  # "SRS22 totalskår 60mnd" =
                                  #  "SRS22_FULL_SCORE_patient60mths",
                                  "SRS22 mental helse 3mnd" =
                                    "SRS22_MENTALHEALTH_SCORE_patient3mths",
                                  "SRS22 mental helse 12mnd" =
                                    "SRS22_MENTALHEALTH_SCORE_patient12mths",
                                  #"SRS22 mental helse 60mnd" =
                                  # "SRS22_MENTALHEALTH_SCORE_patient60mths",
                                  "SRS22 smerte 3mnd" =
                                    "SRS22_PAIN_SCORE_patient3mths",
                                  "SRS22 smerte 12mnd" =
                                    "SRS22_PAIN_SCORE_patient12mths"
                                  #"SRS22 smerte 60mnd" =
                                  #  "SRS22_PAIN_SCORE_patient60mths"
                                  ),
                      selected = "SRS22_FULL_SCORE"
                      )
      ),

      conditionalPanel(
        condition = "input.comp1 == 'PRE_MAIN_CURVE'",
        ns = ns,
        selectInput(ns("comp3"),
                    label = "Velg variabel 2 (etter operasjon - 3-60 mnd oppfølging):",
                    choices = c("Post-operativ kurve" = "POST_MAIN_CURVE"))

      ),

      conditionalPanel(
        condition =
        "input.comp1 == 'Helsetilstand' ||
        input.comp1 == 'Helsetilstand_3mnd'",
        ns = ns,
        selectInput(ns("comp4"),
                    label = "Velg variabel 2 (etter operasjon - 3-60 mnd oppfølging):",
                    choices = c("Helsetilstand 3mnd" = "Helsetilstand_3mnd",
                                "Helsetilstand 12mnd" = "Helsetilstand_12mnd")
                               # "Helsetilstand 60 mnd" = "Helsetilsand_60mnd")
                    )
      ),

      shiny::radioButtons( # third select
        inputId = ns("gender_var"),
        label = "Dele på kjønn?",
        choices = c("kvinne" = "kvinne",
                    "mann" = "mann",
                    "begge" = "begge"),
        selected = "begge"
        ),

      #shinyWidgets::chooseSliderSkin("Flat", color = "#112446"),
      sliderInput( # fifth select
        inputId = ns("alder_var"),
        label = "Aldersintervall:",
        min = 0,
        max = 100,
        value = c(10, 20),
        dragRange = TRUE
        ),

      radioButtons( # sixth select
        inputId = ns("type_op"),
        label = "Type operasjon",
        choices = c("Primæroperasjon", "Reoperasjon", "Begge"),
        selected = "Primæroperasjon"
      ),

      shinyjs::hidden(uiOutput(outputId = ns('reshid'))),

      dateRangeInput( # fourth select
        inputId = ns("date"),
        label = "Tidsintervall:",
        start = "2023-01-02",
        end = "2024-09-02",
        min = "2023-01-01",
        max = "2025-09-02",
        format = "dd-mm-yyyy",
        separator = " - "
      )),

      shiny::mainPanel(
        bslib::navset_card_underline(
          bslib::nav_panel("Figur",
                           shiny::plotOutput(outputId = ns("sam_plot")),
                           shiny::downloadButton(ns("download_sam_plot"), "Last ned figur"))
        )
      )
    )
  )
}

#'@title Server sammenligningsmodul
#'
#'@export

module_sammenligning_server <- function (id, userRole, userUnitId, data) {
  moduleServer(
    id,
    function(input, output, session){

      output$reshid <- renderUI({
        ns <- session$ns
        if (userRole() == 'SC') {
          shiny::selectInput(
            inputId = ns("reshId_var"),
            label = "Enhet",
            choices = c("Haukeland" = 111961, "Rikshospitalet" = 103240, "St.Olav" = 102467),
            selected = "Haukeland"
          )
        }
      })

      ##### MAKE BASIC UTVALG ##################################################

      data_sam_reactive <- reactive({

        if (userRole() == "SC") {
          x <- deformitet::utvalg_basic(data,
                                        input$reshId_var,
                                        input$gender_var,
                                        input$type_op,
                                        input$date[1],
                                        input$date[2],
                                        input$alder_var[1],
                                        input$alder_var[2])

        } else {
          x <- deformitet::utvalg_basic(data,
                                        userUnitId(),
                                        input$gender_var,
                                        input$type_op,
                                        input$date[1],
                                        input$date[2],
                                        input$alder_var[1],
                                        input$alder_var[2])

      }
    })

      #### CHECK FOR SMALL SAMPLE SIZE IN CHOSEN VARIABLES #####################
#
      data_sam_reactive2 <- reactive({

        if(input$comp1 == "PRE_MAIN_CURVE") {
          deformitet::check_small_sample(data_sam_reactive(), input$comp1, input$comp3)
        }
        if (input$comp1 == "Helsetilstand" ||
            input$comp1 == "Helsetilstand_3mnd") {
          deformitet::check_small_sample(data_sam_reactive(), input$comp1, input$comp4)
        } else {
          deformitet::check_small_sample(data_sam_reactive(), input$comp1, input$comp2)
        }
      })

      # this function returns a list that must be unpacked

      data_sam1 <- reactive({
        data1 <- data.frame(data_sam_reactive2()[1])})

      data_sam2 <- reactive({
        data1 <- data.frame(data_sam_reactive2()[2])})


      #### MAKE CONDITIONAL LABELS #############################################

      data_sam_labels <- reactive({
        deformitet::make_labels(data_sam1(), data_sam2(), input$comp1, input$comp2)
      })

      # #### MAKE PLOT ###########################################################

      sam_plot <- reactive({

        if (input$comp1 == "PRE_MAIN_CURVE") {

          deformitet::comparison_plot_continuous(data_sam1(),
                                                 data_sam2(),
                                                 data_sam_labels(),
                                                 input$comp1,
                                                 input$comp3)
        } else {
          if (input$comp1 == "Helsetilstand" ||
              input$comp1 == "Helsetilstand_3mnd") {

            deformitet::comparison_plot_discrete(data_sam1(),
                                                 data_sam2(),
                                                 data_sam_labels(),
                                                 input$comp1,
                                                 input$comp4)
        } else {
          deformitet::comparison_plot_continuous(data_sam1(),
                                                 data_sam2(),
                                                 data_sam_labels(),
                                                 input$comp1,
                                                 input$comp2)
        }
        }
        })

      output$sam_plot <- renderPlot({
        sam_plot()
      })

      # nolint start

      ### MAKE TABLE ###########################################################

      # sam_table <- reactive({
      #
      #   if (input$comp1 == "PRE_MAIN_CURVE") {
      #
      #     deformitet::tabell_sam(regdata,
      #                            input$comp1,
      #                            input$comp3)
      #   } else {
      #     if (input$comp1 == "Helsetilstand" ||
      #         input$comp1 == "Helsetilstand_3mnd") {
      #
      #       deformitet::tabell_sam_discrete(regdata,
      #                                       input$comp1,
      #                                       input$comp4)
      #     } else {
      #       deformitet::tabell_sam(regdata,
      #                             input$comp1,
      #                             input$comp2)
      #     }
      #   }
      # })
      #
      # output$sam_table <- DT::renderDT({datatable(sam_table(),
      #                                             extensions = 'Buttons',
      #                                             options = list(
      #                                               dom = 'Bfrtip',
      #                                               buttons = c('copy', 'csv', 'excel','pdf')),
      #                                             class = 'white-space:nowrap compact')
      # })

      # nolint end

      output$download_sam_plot <-  downloadHandler(
        filename = function(){
          paste("Figur_sammenligning", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file){
          pdf(file, onefile = TRUE, width = 15, height = 9)
          plot(sam_plot())
          dev.off()
        }
      )


    }
  )
}


