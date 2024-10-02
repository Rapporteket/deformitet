library(shiny)
library(ggplot2)
library(dplyr)
library(rlang)
library(tidyr)
library(bslib)



# Få inn data
regdata <- deformitet::les_og_flate_ut()

# Preprosessering


# PrepVar
# Input needed:
  # res= prep(regdata, Kurve_pre, "mann", "2023-01-09", "2023-01-20", 10, 20)
# Output : list with small df (based on choices) and gg-data

# MakeTable
# Input needed:
  # data set which is data frame 1 one from the list made by PrepVar
  # reshId
# Output : table which calculates rates and percentages

# MaaePlot_gg
# Input needed:
  # data made by MakeTable
  # + also gg-data
  # + also data of choices made by user


# The app itself

ui <- fluidPage(

  sidebarLayout(

    # Inputs: Select variables to plot
    sidebarPanel(

      # Select variable for x-axis
      selectInput(
        inputId = "x_var",
        label = "Variabel:",
        choices = c("Helsetilstand" = "Helsetilstand",
                    "Helsetilstand 3-6 mnd" = "Helsetilstand_3mnd",
                    #"Helsetilstand 12 mnd" = "Helsetilstand_12mnd",
                    #"Helsetilstand 5 år" = "Helsetilstand_60mnd",
                    "SRS22 'Samme behandling på nytt?' 3-6 mnd" = "SRS22_spm22_3mnd",
                    #"SRS22 'Samme behandling på nytt?' 12 mnd" = "SRS22_spm22_12mnd",
                    #"SRS22 'Samme behandling på nytt?' 5 år" = "SRS22_spm22_60mnd",
                    "SRS22 'Fornøyd med resultatet?' 3-6 mnd" =  "SRS22_spm21_3mnd",
                    #"SRS22 'Fornøyd med resultatet?' 12 mdn" = "SRS22_spm21_12mnd",
                    #"SRS22 'Fornøyd med resultatet?' 5 år" = "SRS22_spm21_60mnd",
                    "BMI-kategori" = "BMI_kategori",
                    "Alder" = "Alder",
                    "Pre-operativ kurve" = "Kurve_pre",
                    "Post-operativ kurve" = "Kurve_post",
                    "Prosent korreksjon kurve" = "Diff_kurve_prosent",
                    "Liggetid" = "Liggetid",
                    "Knvitid" = "Knivtid",
                    "Blodtap pr. 100 ml" = "Blodtap_100",
                    "Blodtap pr. 200 ml" = "Blodtap_200",
                    "SRS22 totalscore" = "SRS22_total",
                    "SRS22 funksjon ved innleggelse" = "SRS22_funksjon",
                    "SRS22 funksjon, 3-6 mnd" = "SRS22_funksjon_3mnd",
                    #"SRS22 funksjon, 12 mnd" = "SRS22_funksjon_12mnd",
                    #"SRS22 funksjon, 5 år" = "SRS22_funksjon_60mnd",
                    "SRS22 smerte ved innleggelse" = "SRS22_smerte",
                    "SRS22 smerte, 3-6 mnd" = "SRS22_smerte_3mnd",
                    #"SRS22 smerte, 12 mnd" = "SRS22_smerte_12mnd",
                    #"SRS22 smerte, 5 år" = "SRS22_smerte_60mnd",
                    "SRS22 selvbilde ved innleggelse" = "SRS22_selvbilde",
                    "SRS22 selvbilde, 3-6 mnd" = "SRS22_selvbilde_3mnd",
                    #"SRS22 selvbilde, 12 mnd" = "SRS22_selvbilde_12mnd",
                    #"SRS22 selvbilde, 5 år" = "SRS22_selvbilde_60mnd",
                    "SRS22 mental helse ved innleggelse" = "SRS22_mhelse",
                    "SRS22 mental helse, 3-6 mnd" = "SRS22_mhelse_3mnd",
                    #"SRS22 mental helse, 12 mdn" = "SRS22_mhelse_12mnd",
                    #"SRS22 mental helse, 5 år" = "SRS22_mhelse_60mnd",
                    "SRS22 tilfredshet, 3-6 mnd" = "SRS22_fornoyd_3mnd",
                    #"SRS22 tilfredshet, 12 mnd" = "SRS22_fornoyd_12mnd",
                    #"SRS22 tilfredshet, 5 år" = "SRS22_fornoyd_60mnd",
                    "Komplikasjoner, 3-6 mnd" = "Komplikasjoner_3mnd"
                    ),
        selected = "BMI_CATEGORY"),


      selectInput(
        inputId = "kjønn_var",
        label = "Utvalg basert på kjønn",
        choices = c("begge", "mann", "kvinne"),
        selected = "begge"),


      dateRangeInput(
        inputId = "date",
        label = "Tidsintervall:",
        start = "2023-01-02",
        end = "2024-09-02",
        min = "2023-01-01",
        max = "2025-09-02",
        format = "mm/dd/yy",
        separator = " - "),


      sliderInput(
        inputId = "alder_var",
        label = "Aldersintervall:",
        min = 0,
        max = 100,
        value = c(10, 20),
        dragRange = TRUE)


      ),


    # Output: Show plot
    mainPanel(
      navset_card_underline(
        title = "Visualiseringer",
        nav_panel("Figur", plotOutput(outputId = "plot")),
        nav_panel("Tabell", tableOutput(outputId = "table"))
      ))
  #     plotOutput(outputId = "barplot"))
    )
)
# Define server ----------------------------------------------------------------

server <- function(input, output, session) {

  #
  regdata <- pre_pros(regdata)

  # PrepVar

  # Problemet ligger her! Den får ikke til å sjekke disse tingene osv.
  # Jeg tror det er det noe feil i logikken (Y)
  prep_var_reactive <- reactive({
    prep(
      regdata = regdata,
      var = input$x_var,
      var_kjønn = input$kjønn_var,
      time1 = input$date[1],
      time2 = input$date[2],
      alder1 = input$alder_var[1],
      alder2 = input$alder_var[2])
  })

  df_reactive <- reactive({
    df <- data.frame(prep_var_reactive()[1])
  })

  gg_data_reactive <- reactive({
    gg_data <- data.frame(prep_var_reactive()[2])
  })


  output$table <- renderTable()
#
#   # Make table
#   tabell_reactive <- reactive({
#     df <- df_reactive()
#     make_table(
#       data = df,
#       reshID = "Bergen"
#     )
#   })
#
#   output$plot<- renderPlot({
#     data <- tabell_reactive()
#     gg_data <- gg_data_reactive()
#     MakePlot_gg(data)
#   })
  # ,
  # output$table <- renderTable({
  #   deformitet::make_table(regdata, input$x_var, input$x_var, input$x_var, input$x_var)
  # })
}
#
# navn = gg_makeHist2(regdata, BMI_CATEGORY)
# navn + xlab("BMI")+
#   ggtitle("Fordeling av")+
#   theme(plot.title = element_text(size = 14, face ="bold", hjust = 0.5 , vjust = 1.5))

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
