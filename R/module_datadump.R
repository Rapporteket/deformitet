
### Module for creating data based on selection criteria
#'@title module datadump ui
#'@export
module_datadump_UI <- function(id){
  ns <- NS(id)

  shiny::tagList(
    shiny::fluidPage(
      shiny::sidebarPanel(

        selectInput(
          inputId = ns("choice_datadump"),
          label = "Ønsket datautvalg:",
          choices = c("Datasett basert på utvalg",
                      "Datasett basert på skjematype og utvalg"),
          selected = "Datasett basert på utvalg"),

        conditionalPanel(
          condition = paste0("input['", ns("choice_datadump"), "'] == 'Datasett basert på skjematype og utvalg'"),
          selectInput(
            inputId = ns("skjema_type"),
            label = "Data fra: ",
            choices = c("Pasientskjema",
                        "Kirurgskjema")
          )),

        selectInput( # second select - var2
          inputId = ns("kjonn_var"),
          label = "Utvalg basert på kjønn",
          choices = c("begge", "mann", "kvinne"),
          selected = "begge"),

        sliderInput( # third select - var3
          inputId = ns("alder_var"),
          label = "Aldersintervall:",
          min = 0,
          max = 100,
          value = c(0,100),
          dragRange = TRUE),

        #startDato <- paste0(as.numeric(format(Sys.Date()-100, "%Y")), '-01-01')
        dateRangeInput( # first select - var1
          inputId = ns("date"),
          label = "Tidsintervall:",
          start = paste0(as.numeric(format(Sys.Date()-100, "%Y")), '-01-01'),  # "2023-01-01",
          end = Sys.Date(),
          min = paste0(as.numeric(format(Sys.Date()-100, "%Y")), '-01-01'), # "2023-01-01",
          max = "2026-01-01",
          format = "dd-mm-yyyy",
          separator = " - "),

        shiny::downloadButton(ns("download_data"), "Last ned data")
        ),


      mainPanel(
        bslib::navset_card_underline(
          bslib::nav_panel("Info",
            bslib::card(
              bslib::card_header(
                h2("Her kan et utvalg av data fra registeret lastes ned")
                ),
              bslib::card_body(
              h4("Dersom bruker ikke er i registerledelsen vil brukeren her kun få tilgang til
              data som allerede tilhører brukerens enhet (allerede journalført data,
                 dvs. ikke PROM)"),
              h3("Datasett basert på utvalg"),
              p("Dersom 'Datasett basert på utvalg' velges vil flere valg bli
              tilgjengelig slik at brukeren kan velge et begrenset datasett ut fra -
              aldersintervall, tidsintervall, pasientens kjønn. Instillingene som er satt
              som default inkluderer hele datasettet. Dersom ingen av instillingene endres
              vil brukeren laste ned all data som er tilgjengelig for denne brukeren."),
              h3("Datasett basert på skjematype"),
              p("Dersom 'Datasett basert på skjematype' velges vil det være mulig å laste ned et datasett fra hvert eller
                flere av skjemaene som registeret sender inn. Det vil også her være mulig å begrense utvalget.",
                tags$br(),
                tags$br(),
                "- Pasientskjema: informasjon registrert om pasientene pre-operativt. Superbrukere (SC) har også tilgang til oppfølgingsskjema (PROM - 3-6, 12 og 60 mnd)",
                tags$br(),
                "- Kirurgskjema: informasjon registert av kirurg ved operasjon og ved oppfølging")
              )
            )
          ),
          bslib::nav_panel("Forhåndsvisning",
            bslib::card(
              bslib::card_header(
                h3("Her er en forhåndsvisning av tabellen som lastes ned"),
                DT::DTOutput(outputId = ns("datadump")),
                )))
        )
      )
    )
  )
}


#'@export
module_datadump_server <- function(id, data, userRole, userUnitId){
  moduleServer(
    id,
    function(input, output, session){

      # filtrering

        Datadump_reactive <- reactive({
          data <- deformitet::filtrer_datadump(data,
                                             input$date[1],
                                             input$date[2],
                                             input$kjonn_var,
                                             input$alder_var[1],
                                             input$alder_var[2],
                                             userRole(),
                                             userUnitId())
        })


      colnames_surgeonform <- colnames(deformitet::defHentData("surgeonform"))
      colnames_surgeonfollowup <- colnames(deformitet::defHentData("surgeonfollowup"))

      colnames <- c(colnames_surgeonform, colnames_surgeonfollowup)

 #------- Datadump med selvvalgte navn----------

      #NB: Enklest å lage hele datadumpen her uten å hente "tilsmussede data"...
      #Mappe på navn skjema for skjema

      # raw_regdata <- deformitet::alleRegData()
      # RegData <- deformitet::pre_pros(raw_regdata)




      # ----#Sjekk hvilke variabler som har originalnavn og kan endres
      #Gå gjennom navneendringer i kode og gjør disse i samsvar med selvvalgte navn


#      -------------------------------------------------------------------

      # orgname = RegData$ShNavn[match(unique(RegData$ReshId), RegData$ReshId)])

      select_datadump_reactive <- reactive ({
        if (input$skjema_type == "Pasientskjema"){
          data <- Datadump_reactive() %>%
            dplyr::select(-any_of(colnames))
          } else {
            data <- Datadump_reactive() %>%
              dplyr::select(any_of(colnames))
            }
        })

      #sjekk:
     #  data <- RegData %>%
     #    dplyr::select(-any_of(colnames))
     # intersect(colnames, names(RegData))

      output$datadump <- DT::renderDT({
        if (input$choice_datadump == "Datasett basert på skjematype og utvalg") {
          table <- DT::datatable(select_datadump_reactive(),
                                 class = 'white-space:nowrap compact')
        } else {
            table <- DT::datatable(Datadump_reactive(),
                                   class = 'white-space:nowrap compact')
            }
      })


      output$download_data <- downloadHandler(
        filename = function() {
          paste('deformitet-', Sys.Date(), '.csv', sep = '')
        },
        content = function(file) {
          if (input$choice_datadump == "Datasett basert på skjematype og utvalg") {
            write.csv2(select_datadump_reactive(), file)
          } else {
            write.csv(Datadump_reactive(), file)
          }
        }
      )
    }
  )
}

