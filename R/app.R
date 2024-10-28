
source("/home/rstudio/deformitet/R/global.R")
source("/home/rstudio/deformitet/R/modules/module_datadump.R")

#'@export
hallo <- function(){
  print("hallo")
}

deformitet:::hallo()

# The app itself

# Rapporteket graphics
shiny::addResourcePath("rap", system.file("www", package = "rapbase"))


ui <-

  shiny::tagList( # Needed for "about the user" tags
    shiny::navbarPage(# type of page

      ###### Graphics ----------------------------------------------------------
      title = shiny::div(shiny::a(shiny::includeHTML(system.file('www/logo.svg', package = 'rapbase'))), # add the logo
                  "Rapporteket for deformitet"),
      windowTitle = "Rapporteket for deformitet",
      theme = "rap/bootstrap.css", # theme of app
      id = "tabs",


      ###### New tab -----------------------------------------------------------
      shiny::tabPanel( # First tab
        title = "Startside",
        shiny::mainPanel(
          width = 12,
          shiny::htmlOutput("veiledning", inline = TRUE), # load in the htmloutput wanted. This file is found in folder "inst"
          rapbase:::appNavbarUserWidget( # get info about the user. See server for the input
            user = shiny::uiOutput("appUserName"),
            organization = shiny::uiOutput("appOrgName"),
            addUserInfo = TRUE
          )
        )
      ),


      ###### New tab -----------------------------------------------------------
      shiny::tabPanel( # Second tab
        title = "Fordelingsfigur og -tabell",
        shiny::sidebarLayout(

          # Inputs: select variables to plot
          shiny::sidebarPanel(
            width = 3,


            # Select variable for x-axis
            selectInput( # First select
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
                        "Prosent korreksjon kurve" = "Diff_prosent_kurve",
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
                        "Komplikasjoner, 3-6 mnd" = "Komplikasjoner_3mnd",
                        "Komplikasjonstyper" = "Komplikasjonstype"
                        ),
              selected = "BMI_kategori"),


            selectInput( # second select
              inputId = "kjønn_var",
              label = "Utvalg basert på kjønn",
              choices = c("begge", "mann", "kvinne"),
              selected = "begge"),


            dateRangeInput( # third select
              inputId = "date",
              label = "Tidsintervall:",
              start = "2023-01-02",
              end = "2024-09-02",
              min = "2023-01-01",
              max = "2025-09-02",
              format = "mm/dd/yy",
              separator = " - "),


            sliderInput( # fourth select
              inputId = "alder_var",
              label = "Aldersintervall:",
              min = 0,
              max = 100,
              value = c(10, 20),
              dragRange = TRUE),

        selectInput( # fifth select
          inputId = "reshId_var",
          label = "Enhet",
          choices = c("Bergen", "Riksen", "St.Olav"),
          selected = "Bergen"
        )),


    # Output: Show plot
    mainPanel(
      navset_card_underline(
        title = "Visualiseringer",
        nav_panel("Figur", plotOutput(outputId = "plot")),
        nav_panel("Tabell", DTOutput(outputId = "table"))
      ))
    )
  #     plotOutput(outputId = "barplot"))

            ),

  ##### New tab ----------------------------------------------------------------
shiny::tabPanel( # third tab
  title = "Datautvalg",
  shiny::fluidPage(
    module_datadump_UI(
      id = "module_1")
  ))
))

# # Define server ----------------------------------------------------------------
#
server <- function(input, output, session) {

  #### Read in data:
  regdata <- les_og_flate_ut()

  #### Clean and tidy data:
  regdata <- deformitet::pre_pros(regdata)

  # Prepare data based on UI choices
  prepVar_reactive <- reactive({
    deformitet::prepVar(regdata, input$x_var, input$kjønn_var, input$date[1], input$date[2], input$alder_var[1], input$alder_var[2])
    })

  # Unpack part of list - data
  data_reactive <- reactive({
    data <- data.frame(prepVar_reactive()[1])
  })

  # Unpack part of list - gg-data
  gg_data_reactive <- reactive({
    gg_data <- data.frame(prepVar_reactive()[2])
  })

  # Make table of komplikasjonstyper
  kompl_reactive <- reactive({
    deformitet::kompl_data(regdata, input$reshId_var)
  })

# Make table
table_reactive <- reactive({
  deformitet::makeTable(data_reactive(), input$reshId_var)
})

my_data_reactive <- reactive({
  x <- format(input$date, "%d/%m/%y")
  my_data <- data.frame(c(input$x_var, input$kjønn_var, x[1], x[2], input$alder_var[1], input$alder_var[2]))
})

output$table <- DT::renderDataTable({
  if(input$x_var == "Komplikasjonstype"){
    kompl_reactive()
  }
  else{datatable(table_reactive(),
    colnames = c("Sykehus", input$x_var, "antall per var", "antall per sykehus", "andel", "prosent"))}
})



# Print figure

output$plot <- renderPlot({
  if(input$x_var != "Komplikasjonstype"){
  gg_data <- data.frame(gg_data_reactive())
  deformitet::makePlot_gg(table_reactive(), gg_data, my_data_reactive())
  }
  else{
    gg_kompl <- data.frame(c("title" = "Operasjoner pr komplikasjonstype", "xlab" = "Komplikasjonstype"))
    deformitet::makePlot_gg(kompl_reactive(), gg_kompl, my_data_reactive())}
})



  output$appUserName <- shiny::renderText(
    paste(rapbase::getUserFullName(session),
          rapbase::getUserRole(session), sep = ", ")
  )

  output$appOrgName <- shiny::renderText(rapbase::getUserReshId(session))
  userInfo <-
    rapbase::howWeDealWithPersonalData(session, callerPkg = "deformitet")
  shiny::observeEvent(input$userInfo, {
    shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
                           type = "", imageUrl = "rap/logo.svg",
                           closeOnEsc = True, closeOnClickOutside = TRUE,
                           html = TRUE, confirmButtonText = rapbase::opOptOutOk())
  })

  # Veiledning
  output$veiledning <- shiny::renderUI({
    rapbase::renderRmd(
      system.file("veiledning.Rmd", package = "deformitet"),
      outputType = "html_fragment"
    )
  })

  userRole <- rapbase::getUserRole(session)

  if(userRole == "accessLevel"){
    shiny::hideTab(inputId = "tabs", target = "Datautvalg", session = getDefaultReactiveDomain())
  }

  output$d1 <- shiny::downloadHandler(
    filename = function() {
      paste('datadump_utvalg', Sys.Date(), '.csv', sep = "")
    },
    content = function(con){
      write.csv(regdata, con)
    }
  )
}

# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
