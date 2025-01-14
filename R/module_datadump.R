
### Module for creating data based on selection criteria
#'@title module datadump ui
#'@export
module_datadump_UI <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      sidebarPanel(

        selectInput(
          inputId = ns("choice_datadump"),
          label = "Ønsket datautvalg:",
          choices = c("Datasett basert på utvalg",
                      "Datasett basert på skjematype og utvalg"),
          selected = "Datasett basert på utvalg"),

        dateRangeInput( # first select - var1
          inputId = ns("date"),
          label = "Tidsintervall:",
          start = "2023-01-02",
          end = "2024-09-02",
          min = "2023-01-02",
          max = "2025-01-02",
          format = "mm/dd/yy",
          separator = " - "),

        selectInput( # second select - var2
          inputId = ns("kjønn_var"),
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


        conditionalPanel(
          condition = paste0("input['", ns("choice_datadump"), "'] == 'Datasett basert på skjematype og utvalg'"),
          selectInput(
            inputId = ns("skjema_type"),
            label = "Data fra: ",
            choices = c("Pasientskjema",
                        "Kirurgskjema",
                        "Kirurgskjema oppfølging")
          )),

        downloadButton(
          outputId= ns("d1"),
          label = "Download",
          class = "btn-lg btn-success")
      ),

      mainPanel(
        class = "p-3 mb-2 bg-light text-dark",
        bslib::card(
          bslib::card_header(
            h1("Her kan et utvalg av data fra registeret lastes ned")
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
              "- Pasient ved operasjon: informasjon om pasientene ved operasjon",
              tags$br(),
              "- Kirurg ved operasjon: informasjon registert av kirurg ved operasjon",
              tags$br(),
              "- Oppfølging kirurgi: informasjon registeret av kirurg ved oppfølging - 3mnd, 12mnd og 60mnd")

            )
        )
      )
    )
  )
}


#'@export
module_datadump_server <- function(id){
  moduleServer(
    id,
    function(input, output, session){

      reshID == "103240"

      # do the cleaning

      if(userRole == "SC"){
        clean_datadump_reactive <- reactive({
          data <- deformitet::clean_datadump(regdata, input$date[1], input$date[2], input$kjønn_var, input$alder_var[1], input$alder_var[2])
        })
      }

      else{
        clean_datadump_reactive <- reactive({
          data <- deformitet::clean_datadump(regdata, input$date[1], input$date[2], input$kjønn_var, input$alder_var[1], input$alder_var[2])
          data <- data %>%
            select(-contains(c("mths", "mnd"))) %>%
            filter(CENTREID == reshID)
        })
      }

      if(choice_datadump == "Datasett basert på skjematype og utvalg"){
        if(skjema_type == "Pasientskjema"){
          select_datadump_reactive <- reactive({
            data <- clean_datadump_reactive() %>%
              select()### SOMETHING
          })
          }
          else{
            if(skjema_type == "Kirurgiskjema"){
              select_datadump_reactive <- reactive({
                data <- clean_datadump_reactive() %>%
                  select()### SOMETHING
            })
            }
            else{
              select_datadump_reactive <- reactive({
              data <- clean_datadump_reactive() %>%
                select()
              })
            }
        }
      }
    })
}




