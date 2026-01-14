
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
          choices = c(
            "Datasett med selvvalgte navn",
            "Datasett basert på utvalg",
            "Datasett basert på skjematype og utvalg"),
          selected = "Datasett med selvvalgte navn"),

        conditionalPanel(
          condition = paste0("input['", ns("choice_datadump"), "'] == 'Datasett basert på skjematype og utvalg'"),
          selectInput(
            inputId = ns("skjema_type"),
            label = "Data fra: ",
            choices = c("Pasientskjema",
                        "Kirurgskjema")
          )),

       #startDato <- paste0(as.numeric(format(Sys.Date()-100, "%Y")), '-01-01')
        dateRangeInput(
          inputId = ns("date"),
          label = "Tidsintervall:",
          start = paste0(as.numeric(format(Sys.Date()-100, "%Y")), '-01-01'),
          end = Sys.Date(),
          min = "2023-01-01",
          max = Sys.Date(),
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
              data som allerede tilhører brukerens enhet (allerede journalført data, dvs. ikke PROM)"),
                               h3("Datasett basert på utvalg"),
                               p("Dersom 'Datasett basert på utvalg' velges, vil flere valg bli
              tilgjengelig slik at brukeren kan velge et begrenset datasett ut fra tidsintervall."),
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

    datadumpEgneNavn <- reactive({
        if (input$choice_datadump == "Datasett med selvvalgte navn") {
          data <- deformitet::alleRegData(egneVarNavn=1) |>
            dplyr::filter(dplyr::between(.data$InnDato, as.Date(input$date[1]), as.Date(input$date[2])))
          }
      })


      # filtrering

      Datadump_reactive <- reactive({
        head(names(data))
        data <- deformitet::filtrer_datadump(data,
                                             input$date[1],
                                             input$date[2],
                                             userRole(),
                                             userUnitId())
      })

      #      -------------------------------------------------------------------

      colnames_surgeonform <- colnames(deformitet::defHentData("surgeonform"))
      colnames_surgeonfollowup <- colnames(deformitet::defHentData("surgeonfollowup"))
      colnames <- c(colnames_surgeonform, colnames_surgeonfollowup)


      # orgname = RegData$ShNavn[match(unique(RegData$ReshId), RegData$ReshId)])

      select_datadump_reactive <- reactive ({
        if (input$skjema_type == "Pasientskjema"){
          data <- Datadump_reactive() |>
            dplyr::select(-any_of(colnames))
        } else {
          data <- Datadump_reactive() |>
            dplyr::select(any_of(colnames))
        }
      })

         output$datadump <- DT::renderDT({
             table <- DT::datatable(
               switch(input$choice_datadump,
                      "Datasett basert på skjematype og utvalg" = select_datadump_reactive(),
                      "Datasett basert på utvalg" = Datadump_reactive(),
                      "Datasett med selvvalgte navn" = datadumpEgneNavn()),
               class = 'white-space:nowrap compact')
           })


      output$download_data <- downloadHandler(
        filename = function() {
          paste0('deformitet-', Sys.Date(), '.csv')
        },
        content = function(file) {
          switch(input$choice_datadump,
                        "Datasett basert på skjematype og utvalg" = write.csv2(select_datadump_reactive(), file),
                        "Datasett med selvvalgte navn" = write.csv2(datadumpEgneNavn(), file),
                        "Datasett basert på utvalg" = write.csv2(Datadump_reactive()(), file))
        }
      ) #download

    }
  )
}
