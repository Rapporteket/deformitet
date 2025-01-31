#' Server logic for the deformitet app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

### + eksport i server

app_server <- function(input, output, session) {

  library(dplyr)
  library(deformitet)
  library(tidyr)
  library(ggplot2)
  library(DT)
  library(shiny)
  library(rapbase)
  library(bslib)
  library(shinyWidgets)
  library(NHSRplotthedots)

######## USER INFO--------------------------------------------------------------

  userRole = "SC"
  #### MÅ VISES IGJEN NÅR JEG IKKE DRIVER MED FALSKE DATA ####
##Render small header with user info

  output$appUserName <- shiny::renderText(
    paste(rapbase::getUserFullName(session),
          rapbase::getUserRole(session), sep = ", ")
  )

  output$appOrgName <- shiny::renderText(rapbase::getUserReshId(session))

  # Make pop-up with "Dette vet Rapporteket om deg:"

  userInfo <- rapbase::howWeDealWithPersonalData(session,
                                                 callerPkg = "deformitet")
  shiny::observeEvent(input$userInfo, {
    shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
                           type = "", imageUrl = "rap/logo.svg",
                           closeOnEsc = True, closeOnClickOutside = TRUE,
                           html = TRUE, confirmButtonText = rapbase::opOptOutOk())
  })

  ################################################################################
  ##### TAB: Startside ###########################################################

  # Veiledning
  output$veiledning <- shiny::renderUI({
    rapbase::renderRmd(
      system.file("veiledning.Rmd", package = "deformitet"),
      outputType = "html_fragment"
    )
  })


  ################################################################################
  ##### TAB: Fordelingsfigur og -tabell ##########################################

  ######### DATA TIDYING----------------------------------------------------------
  #### Read in data:
  #regdata <- deformitet::les_og_flate_ut()
  #
  # #### Clean and tidy data:
  #
  #regdata <- deformitet::pre_pros(regdata)

  ######## FAKE DATA ###########

  regdata <- readRDS("../dev/fake_data_deformitet.rds")

  ## General cleaning
  regdata <- regdata %>%
    dplyr::mutate(Sykehus =
                    dplyr::recode(Sykehus,
                                  "Bergen" = "Haukeland",
                                  "Riksen" = "Rikshospitalet"))

  regdata$BMI_kategori <- ordered(regdata$BMI_kategori,
                                  levels =c("Alvorlig undervekt\n < 16",
                                            "Undervekt\n (16-17)",
                                            "Mild undervekt\n (17-18,5)",
                                            "Normal\n (18,5-25)",
                                            "Overvekt\n (25-30)",
                                            "Moderat fedme\n, klasse I (30-35)",
                                            "Fedme, klasse II \n (35-40)",
                                            "Fedme, klasse III \n (40-50)"))

  # Prepare data based on UI choices

  prepVar_reactive <- reactive({
    deformitet::prepVar(
      regdata,
      input$x_var,
      input$kjønn_var,
      input$date[1],
      input$date[2],
      input$alder_var[1],
      input$alder_var[2],
      input$type_op
      )
  })

  # Make data frame where UI choices are stored

  ### ALSO DOCUMENT ALL ACCESS EACH USER ROLE HAS

  my_data_reactive <- reactive({
    x <- format(input$date, "%d/%m/%y")
    my_data <- data.frame(c(input$x_var, input$kjønn_var, x[1], x[2], input$alder_var[1], input$alder_var[2], input$type_op))
  })
  # prepVar() returns a list

  # Unpack part 1 of list: data

  data_reactive <- reactive({
    data <- data.frame(prepVar_reactive()[1])
  })

  # Unpack part 2 of list: gg-data

  gg_data_reactive <- reactive({
    gg_data <- data.frame(prepVar_reactive()[2])
  })


  ######## AGGREGATE DATA-------------------------------------------------------

  #Aggregate data in table format

  table_reactive <- reactive({
    deformitet::makeTable(data_reactive(), input$reshId_var, input$type_view)
  })

  # Make table of komplikasjonstyper
  ### Komplikasjonstyper is aggregated separately from the rest of the variables

  kompl_reactive <- reactive({
    test <- deformitet::kompl_data(regdata, input$reshId_var)
  })


  ########## DISPLAY DATA-------------------------------------------------------

  ## TABLE

  output$table <- DT::renderDT({
    if(input$x_var == "Komplikasjonstype"){ # if "komplikasjonstype is chosen, use kompl_reactive
      kompl_reactive()
    }
    else{datatable(table_reactive())
      }
  })


  ## FIGURE

  output$plot <- renderPlot({
    if(input$x_var != "Komplikasjonstype"){
      gg_data <- data.frame(gg_data_reactive())
      deformitet::makePlot_gg(table_reactive(),
                              gg_data,
                              my_data_reactive(),
                              input$type_view)
    }
    else{
      gg_kompl <- data.frame(c("title" = "Operasjoner pr komplikasjonstype",
                               "xlab" = "Komplikasjonstype"))
      deformitet::makePlot_gg(kompl_reactive(),
                              gg_kompl,
                              my_data_reactive(),
                              input$type_view)}
  })

################################################################################
##### TAB: Kvalitetsindikatorer ################################################


  deformitet::module_kvalitetsindikator_server("kval1")

  ################################################################################
  ##### TAB: Sammenligning #####################################################


  deformitet::module_sammenligning_server("sam1")

  ################################################################################
  ##### TAB: SPC #################################################################

  deformitet::module_spc_server("spc")

  ################################################################################
  ##### TAB: Nestlasting av datadump #############################################

  #userRole <- rapbase::getUserRole(session) # define userRole

  if(userRole != "SC"){ # hide tab is userRole is not SC
    shiny::hideTab(
      inputId = "tabs", # saying its the tabs part of the page that should be hidden
      target = "Datautvalg" # saying its the tab with "Datautvalg"
    )
    shiny::hideTab(
      inputId = "tabs",
      target = "Eksport"
    )
  }

  output$d1 <- shiny::downloadHandler( # output = downloadHandler
    filename = function() {
      paste('datadump_utvalg', Sys.Date(), '.csv', sep = "") # make file name
    },
    content = function(con){
      write.csv(regdata, con) # content is the non aggregated, fully processed data)
    }
  )

################################################################################
###### TAB: Exporting data #####################################################

  # Brukerkontroller

  rapbase::exportUCServer("deformitetExport", "deformitet")

  # Veiledning

  rapbase::exportGuideServer("deformitetExportGuide", "deformitet")

}

