#'@title Module registreringer
#'@export


module_registreringer_UI <- function (id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::fluidPage(
      shiny::sidebarLayout(
        shiny::sidebarPanel(width = 3,
        shiny::selectInput(size = 10, selectize=FALSE,
          inputId = ns("registreringer"),
          label = "Velg tidsperiode",
          choices = c("Siste år",
                      "Siste måned",
                      "Siste 6 måneder",
                      "Egendefinert tidsperiode"),
          selected = "Siste år"
        ),

        shiny::conditionalPanel(
          condition = "input.registreringer == 'Egendefinert tidsperiode'",
          dateRangeInput(width = '100%',
            inputId = ns("date"),
            label = "Egendefinert tidsintervall:",
            start = "2024-01-01",
            end = Sys.Date(), # "2025-12-01",
            min = "2023-01-01",
            max = Sys.Date(), #  "2025-12-01",
            format = "dd-mm-yyyy",
            separator = " - "),
          ns = NS(id)
      )
    ),
    shiny::mainPanel(
      bslib::navset_card_underline(
        bslib::nav_panel("Antall registreringer",
                         DT::DTOutput(outputId = ns("reg_table")),
                         shiny::downloadButton(ns("download_reg_table"), "Last ned tabell"),
                         bslib::navset_card_underline(
                           title = h4("Antall registreringer pr. operasjonsdato"),
                           bslib::card_body("Denne tabellen gir en oversikt over antall registrerte operasjoner pr måned (i en gitt tidsperiode)."))),
        bslib::nav_panel("Antall registreringer pr. skjema",
                         textOutput(outputId = ns("skjema_text")),
                         DT::DTOutput(outputId = ns("reg_skjema_table")),
                         shiny::downloadButton(ns("download_reg_skjema_table"), "Last ned tabell", class = "butt2"),
                         bslib::navset_card_underline(
                           title = h4("Antall registreringer pr. skjema"),
                           bslib::card_body("Denne tabellen gir en oversikt over antall registrerte skjema (i en gitt tidsperiode).")))
  )
)
)
)
)
}


#'@title Server sammenligningsmodul
#'@export

module_registreringer_server <- function (id, userRole, userUnitId, data) {
  moduleServer(
    id,
    function(input, output, session){

######## SET CONDITIONAL DATES BASED ON UI CHOICES #############################
      date1 <- reactive({
      if (input$registreringer == "Egendefinert tidsperiode") {
        date1 <- input$date[1]
      }
      else {
        if (input$registreringer == "Siste år") {
          date1 <- format(ymd(Sys.Date() - years(1)), "%d-%m-%Y")
        }
        else {
          if (input$registreringer == "Siste måned") {
            date1 <- format(ymd(Sys.Date() - months(1)), "%d-%m-%Y")
          }
          else {
            date1 <- format(ymd(Sys.Date() - months(6)), "%d-%m-%Y")
          }
        }
      }
      })


      date2 <- reactive({
        if (input$registreringer == "Egendefinert tidsperiode") {
          date2 <- input$date[2]
        }
        else {
          date2 <- format(Sys.Date(), "%d-%m-%Y")
        }
      })

########### MAKE DATA TABLES ###################################################

      reg_reactive <- reactive({
        table_reg <- tbl_reg(date1(), date2(), data)
      })

      reg_skjema_reactive <- reactive({
        table_skjema <- tbl_skjema_reg(date1(), date2(), data)
      })

############ DISPLAY ###########################################################

      output$reg_table <- DT::renderDT({
        ns <- session$ns
        datatable(reg_reactive())
      })

      output$reg_skjema_table <- DT::renderDT({
        ns <- session$ns
        datatable(reg_skjema_reactive())
      })

      output$skjema_text <- renderText({
        ns <- session$ns
        paste0("Viser data for tidsrommet: ", date1(), " til ", date2())
      })

      output$download_reg_table <- downloadHandler(
        filename = function(){
          paste("AntallOperasjoner_", input$registreringer, "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file){
          write.csv(reg_reactive(), file)
        }
      )

      output$download_reg_skjema_table <- downloadHandler(
        filename = function(){
          paste("RegistreringerPrSkjema_", input$registreringer, "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file){
          write.csv(reg_skjema_reactive(), file)
        }
      )
    }
  )
}

