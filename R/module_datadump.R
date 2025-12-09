
### Module for creating data based on selection criteria
#'@title module datadump ui
#'@export
module_datadump_UI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidPage(
      shiny::sidebarPanel(

        shiny::selectInput(
          inputId = ns("choice_datadump"),
          label = "Ønsket datautvalg:",
          choices = c("Datasett basert på utvalg",
            "Datasett basert på skjematype og utvalg"),
          selected = "Datasett basert på utvalg"),

        shiny::conditionalPanel(
          condition = paste0("input['", ns("choice_datadump"), "'] == 'Datasett basert på skjematype og utvalg'"),
          selectInput(
            inputId = ns("skjema_type"),
            label = "Data fra: ",
            choices = c("Pasientskjema",
              "Kirurgskjema")
          )),

        shiny::selectInput( # second select - var2
          inputId = ns("kjønn_var"),
          label = "Utvalg basert på kjønn",
          choices = c("begge", "mann", "kvinne"),
          selected = "begge"),

        shiny::sliderInput( # third select - var3
          inputId = ns("alder_var"),
          label = "Aldersintervall:",
          min = 0,
          max = 100,
          value = c(0,100),
          dragRange = TRUE),

        shiny::dateRangeInput( # first select - var1
          inputId = ns("date"),
          label = "Tidsintervall:",
          start = "2023-01-01",
          end = "2026-01-01",
          min = "2023-01-01",
          max = "2026-01-01",
          format = "dd-mm-yyyy",
          separator = " - "),

        shiny::downloadButton(ns("download_data"), "Last ned data")
      ),


      shiny::mainPanel(
        bslib::navset_card_underline(
          bslib::nav_panel("Info",
            bslib::card(
              bslib::card_header(
                h2("Her kan et utvalg av data fra registeret lastes ned")
              ),
              bslib::card_body(
                shiny::h4("Dersom bruker ikke er i registerledelsen vil brukeren her kun få tilgang til
              data som allerede tilhører brukerens enhet (allerede journalført data,
                 dvs. ikke PROM)"),
                shiny::h3("Datasett basert på utvalg"),
                shiny::p("Dersom 'Datasett basert på utvalg' velges vil flere valg bli
              tilgjengelig slik at brukeren kan velge et begrenset datasett ut fra -
              aldersintervall, tidsintervall, pasientens kjønn. Instillingene som er satt
              som default inkluderer hele datasettet. Dersom ingen av instillingene endres
              vil brukeren laste ned all data som er tilgjengelig for denne brukeren."),
                shiny::h3("Datasett basert på skjematype"),
                shiny::p("Dersom 'Datasett basert på skjematype' velges vil det være mulig å laste ned et datasett fra hvert eller
                flere av skjemaene som registeret sender inn. Det vil også her være mulig å begrense utvalget.",
                  shiny::tags$br(),
                  shiny::tags$br(),
                  "- Pasientskjema: informasjon registrert om pasientene pre-operativt. Superbrukere (SC) har også tilgang til oppfølgingsskjema (PROM - 3-6, 12 og 60 mnd)",
                  shiny::tags$br(),
                  "- Kirurgskjema: informasjon registert av kirurg ved operasjon og ved oppfølging")
              )
            )
          ),
          bslib::nav_panel("Forhåndsvisning",
            bslib::card(
              bslib::card_header(
                shiny::h3("Her er en forhåndsvisning av tabellen som lastes ned"),
                DT::DTOutput(outputId = ns("datadump")),
              )))
        )
      )
    )
  )
}


#'@export
module_datadump_server <- function(id, data, userRole, userUnitId){
  shiny::moduleServer(
    id,
    function(input, output, session){

      # do the cleaning

      clean_datadump_reactive <- shiny::reactive({
        data <- deformitet::clean_datadump(data,
          input$date[1],
          input$date[2],
          input$kjønn_var,
          input$alder_var[1],
          input$alder_var[2],
          userRole(),
          userUnitId())
      })


      colnames_surgeonform <- colnames(deformitet::deformitetHentTabell("surgeonform"))
      colnames_surgeonfollowup <- colnames(deformitet::deformitetHentTabell("surgeonfollowup"))

      colnames <- c(colnames_surgeonform, colnames_surgeonfollowup)

      select_datadump_reactive <- shiny::reactive ({
        if (input$skjema_type == "Pasientskjema"){
          data <- clean_datadump_reactive() %>%
            dplyr::select(-dplyr::any_of(colnames))
        } else {
          data <- clean_datadump_reactive() %>%
            dplyr::select(dplyr::any_of(colnames))
        }
      })

      output$datadump <- DT::renderDT({
        if (input$choice_datadump == "Datasett basert på skjematype og utvalg") {
          DT::datatable(select_datadump_reactive(),
            class = 'white-space:nowrap compact')
        } else {
          DT::datatable(clean_datadump_reactive(),
            class = 'white-space:nowrap compact')
        }
      })


      output$download_data <- shiny::downloadHandler(
        filename = function() {
          paste('data-', Sys.Date(), '.csv', sep = '')
        },
        content = function(file) {
          if (input$choice_datadump == "Datasett basert på skjematype og utvalg") {
            write.csv(select_datadump_reactive(), file)
          } else {
            write.csv(clean_datadump_reactive(), file)
          }
        }
      )
    }
  )
}

