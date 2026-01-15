#'@title Module registreringer
#'@export


module_registreringer_ui <- function (id) {
  ns <- shiny::NS(id)
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
                              condition = paste0("input['", ns("registreringer"), "'] == 'Egendefinert tidsperiode'"),
                              shiny::dateRangeInput(width = '100%',
                                                    inputId = ns("date"),
                                                    label = "Egendefinert tidsintervall:",
                                                    start = "2024-01-01",
                                                    end = Sys.Date(), # "2025-12-01",
                                                    min = "2023-01-01",
                                                    max = Sys.Date(), #  "2025-12-01",
                                                    format = "dd-mm-yyyy",
                                                    separator = " - ")
                            )
        ),
        shiny::mainPanel(
          bslib::navset_card_underline(
            bslib::nav_panel("Antall registreringer",
                             DT::DTOutput(outputId = ns("reg_table")),
                             shiny::downloadButton(ns("download_reg_table"), "Last ned tabell"),
                             bslib::navset_card_underline(
                               title = shiny::h4("Antall registreringer pr. operasjonsdato"),
                               bslib::card_body("Denne tabellen gir en oversikt over antall registrerte operasjoner pr måned (i en gitt tidsperiode)."))),
            bslib::nav_panel("Antall registreringer pr. skjema",
                             shiny::textOutput(outputId = ns("skjema_text")),
                             DT::DTOutput(outputId = ns("reg_skjema_table")),
                             shiny::downloadButton(ns("download_reg_skjema_table"), "Last ned tabell", class = "butt2"),
                             bslib::navset_card_underline(
                               title = shiny::h4("Antall registreringer pr. skjema"),
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
  shiny::moduleServer(
    id,
    function(input, output, session){

      ######## SET CONDITIONAL DATES BASED ON UI CHOICES #############################
      date1 <- shiny::reactive({
        if (input$registreringer == "Egendefinert tidsperiode") {
          date1 <- input$date[1]
        }
        else {
          if (input$registreringer == "Siste år") {
            date1 <- format(lubridate::ymd(Sys.Date() - lubridate::years(1)), "%d-%m-%Y")
          }
          else {
            if (input$registreringer == "Siste måned") {
              date1 <- format(lubridate::ymd(Sys.Date() - lubridate::months(1)), "%d-%m-%Y")
            }
            else {
              date1 <- format(lubridate::ymd(Sys.Date() - lubridate::months(6)), "%d-%m-%Y")
            }
          }
        }
      })


      date2 <- shiny::reactive({
        if (input$registreringer == "Egendefinert tidsperiode") {
          date2 <- input$date[2]
        }
        else {
          date2 <- format(Sys.Date(), "%d-%m-%Y")
        }
      })

      ########### MAKE DATA TABLES ###################################################

      reg_reactive <- shiny::reactive({
        table_reg <- tbl_reg(date1(), date2(), data)
      })

      reg_skjema_reactive <- shiny::reactive({
        table_skjema <- tbl_skjema_reg(date1(), date2(), data)
      })

      ############ DISPLAY ###########################################################

      output$reg_table <- DT::renderDT({
        ns <- session$ns
        DT::datatable(reg_reactive())
      })

      output$reg_skjema_table <- DT::renderDT({
        ns <- session$ns
        DT::datatable(reg_skjema_reactive())
      })

      output$skjema_text <- shiny::renderText({
        ns <- session$ns
        paste0("Viser data for tidsrommet: ", date1(), " til ", date2())
      })

      output$download_reg_table <- shiny::downloadHandler(
        filename = function(){
          paste("AntallOperasjoner_", input$registreringer, "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file){
          write.csv(reg_reactive(), file)
        }
      )

      output$download_reg_skjema_table <- shiny::downloadHandler(
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

