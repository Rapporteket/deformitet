#'@title Ui sammenligningsmodul
#'
#'@export

module_sammenligning_UI <- function (id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        selectInput( # First select
          inputId = ns("comp"),
          label = "Velg variabel",
          choices = c("SRS22 totalskår",
                      "Funksjon",
                      "Selvbilde",
                      "Mental helse",
                      "Smerte",
                      "Helsetilstand",
                      "Tilfredshet"),
          selected = "SRS22 totalskår"),

        shiny::selectInput( # Second select
          inputId = ns("plot_valg"),
          label = "Velg plot-type",
          choices = c("Tetthetsplot",
                      "Boksplot"),
          selected = "Boksplot"
          ),

        conditionalPanel(
          condition = "input.plot_valg == 'Tetthetsplot'",
          shiny::selectInput( #fifth select
            inputId = ns("valg_sammenligning"),
            label = "Velg sammenligning",
            choices = c("Før operasjon - 3 mnd",
                        "Før operasjon - 12 mnd",
                        "Før operasjon - 5 år",
                        "3 mnd - 12 mnd",
                        "3 mnd - 5 år",
                        "12 mnd - 5 år")),
            selected = "Før operasjon - 3 mnd",
            ns = NS(id)
          ),

      shiny::radioButtons( # second select
        inputId = ns("gender_var"),
        label = "Dele på kjønn?",
        choices = c("kvinne" = "kvinne",
                    "mann" = "mann",
                    "begge" = "begge"),
        selected = "begge"
        ),

      #shinyWidgets::chooseSliderSkin("Flat", color = "#112446"),
      sliderInput( # third select
        inputId = ns("alder_var"),
        label = "Aldersintervall:",
        min = 0,
        max = 100,
        value = c(10, 20),
        dragRange = TRUE
        ),

      radioButtons( # fourth select
        inputId = ns("type_op"),
        label = "Type operasjon",
        choices = c("Primæroperasjon", "Reoperasjon", "Begge"),
        selected = "Primæroperasjon"
      ),


      shinyjs::hidden(uiOutput(outputId = ns('reshid'))),

      dateRangeInput( # sixth select
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
          bslib::nav_panel(
            "Figur",
            shiny::plotOutput(outputId = ns("comp_plot")),
            shiny::downloadButton(ns("download_comp_plot"), "Last ned figur"),
          )
        )
      )
    )
  )
}

#'@title Server sammenligningsmodul
#'
#'@export

module_sammenligning_server <- function (id, data, userRole, userUnitId) {
  moduleServer(
    id,
    function(input, output, session){

      reshid <- reactiveValues(reshId_var = 111961)


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

      observe({
        req(input$reshId_var)
        reshid$reshId_var <- input$reshId_var
      })


      ##### MAKE BASIC UTVALG ##################################################

      data_sam_reactive <- reactive({

        if (userRole() == "SC") {
          x <- deformitet::utvalg_basic(data,
                                reshid$reshId_var,
                                input$gender_var,
                                input$type_op,
                                input$date[1],
                                input$date[2],
                                input$alder_var[1],
                                input$alder_var[2]
                                )
          } else {
            x <- deformitet::utvalg_basic(data,
                                        userUnitId(),
                                        input$gender_var,
                                        input$type_op,
                                        input$date[1],
                                        input$date[2],
                                        input$alder_var[1],
                                        input$alder_var[2]
                                        )

      }
    })

      my_data_reactive <- reactive({
        x <- format(input$date, "%d/%m/%y")
        my_data <- data.frame(c(input$comp, input$gender_var, x[1], x[2], input$alder_var[1], input$alder_var[2], input$type_op))
      })

      comparability_data_reactive <- reactive({
        data <- make_comparability_table(data_sam_reactive(), input$comp)
      })

      new_labels_reactive <- reactive({
        labels <- new_labels(comparability_data_reactive())
      })

      clean_comp_data_reactive <- reactive({
        data_clean <- clean_comparability_table(new_labels_reactive(), input$comp)
      })

      gg_data_boxplot_reactive <- reactive({
        gg_data_boxplot <- gg_data_comparability(input$comp)
      })

      comp_variables_reactive <- reactive({
        find_comp_variables(clean_comp_data_reactive(), input$valg_sammenligning)
      })

      comp_plot_reactive <- reactive({

        if(input$plot_valg == "Boksplot") {
          deformitet::ggplot_comparability(clean_comp_data_reactive(), gg_data_boxplot_reactive(), my_data_reactive())

        } else {
          deformitet::density_plot_comparability(comp_variables_reactive(), gg_data_boxplot_reactive(), my_data_reactive())
          }
        })

      output$comp_plot <- renderPlot({
        comp_plot_reactive()
        })




      #### CHECK FOR SMALL SAMPLE SIZE IN CHOSEN VARIABLES #####################

     # Make new dataframe - I need one column with value and one with time of oppfølging

      # #### MAKE PLOT ###########################################################

      # sam_plot <- reactive({
      #
      #   if (input$comp1 == "PRE_MAIN_CURVE") {
      #
      #     deformitet::comparison_plot_continuous(data_sam1(),
      #                                            data_sam2(),
      #                                            data_sam_labels(),
      #                                            input$comp1,
      #                                            input$comp3)
      #   } else {
      #     if (input$comp1 == "Helsetilstand" ||
      #         input$comp1 == "Helsetilstand_3mnd") {
      #
      #       deformitet::comparison_plot_discrete(data_sam1(),
      #                                            data_sam2(),
      #                                            data_sam_labels(),
      #                                            input$comp1,
      #                                            input$comp4)
      #   } else {
      #     deformitet::comparison_plot_continuous(data_sam1(),
      #                                            data_sam2(),
      #                                            data_sam_labels(),
      #                                            input$comp1,
      #                                            input$comp2)
      #   }
      #   }
      #   })
      #
      # output$sam_plot <- renderPlot({
      #   sam_plot()
      # })

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

      # output$download_sam_plot <-  downloadHandler(
      #   filename = function(){
      #     paste("Figur_sammenligning", Sys.Date(), ".pdf", sep = "")
      #   },
      #   content = function(file){
      #     pdf(file, onefile = TRUE, width = 15, height = 9)
      #     plot(sam_plot())
      #     dev.off()
      #   }
      # )

      output$download_comp_plot <-  downloadHandler(
        filename = function(){
          paste("boxplot_sammenligning", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file){
          pdf(file, onefile = TRUE, width = 15, height = 9)
          plot(comp_plot_reactive())
          dev.off()
        }
      )


    }
  )
}


