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


  ######### DATA TIDYING----------------------------------------------------------
  ### Read in data:
  regdata <- deformitet::les_og_flate_ut()

  #### Clean and tidy data:

  regdata <- deformitet::pre_pros(regdata)

  ######## USER INFO--------------------------------------------------------------

  # Make a df that can be used for mapping between resh-ids and hospital names
  # Must be organized as df with two columns: UnitId and orgname
  # in order for navbarWidgetServer2 to work properly

  map_db_resh <- regdata %>%
    select(Sykehus, CENTREID) %>% # select required columns
    unique() %>% # keep only unique variables
    mutate(UnitId = CENTREID, # make new column with new name
           orgname = Sykehus) %>% # make new column with new name
    select(-c(Sykehus, CENTREID)) # take out old columns


  user <- rapbase::navbarWidgetServer2("deformitetNavbarWidget", # denne skal bli navbarWidgetServer når alt er fikset i rapbase
                                       "deformitet",
                                       caller = "deformitet",
                                       map_orgname = shiny::req(map_db_resh))


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

  # Prepare data based on UI choices


  output$reshid <- renderUI({
    if (user$role() == 'SC') { # fifth select
      shiny::selectInput(
        inputId = "reshId_var",
        label = "Enhet",
        choices = c("Haukeland" = 111961, "Rikshospitalet" = 103240, "St.Olav" = 102467),
        selected = "Haukeland"
      )
    }
  })


  output$view_type <- renderUI({
    if(user$role() == 'SC') {
      shiny::radioButtons( # seventh select
        inputId = "type_view",
        label = "Vis rapport for:",
        choices = c("Hele landet" = "hele landet",
                    "Hele landet, uten sammenligning" = "hele landet, uten sammenligning",
                    "Hver enhet" = "hver enhet",
                    "Egen enhet" = "egen enhet"
        ))
    } else {
      shiny::radioButtons( # seventh select
        inputId = "type_view",
        label = "Vis rapport for:",
        choices = c("Hele landet" = "hele landet",
                    "Hele landet, uten sammenligning" = "hele landet, uten sammenligning",
                    "Egen enhet" = "egen enhet"
        ))
    }
  })


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
    if (user$role() == 'SC') {
      reshid = input$reshId_var
    } else {
      reshid = user$org()
    }
    deformitet::makeTable(data_reactive(), reshid, input$type_view)
  })

  # Make table of komplikasjonstyper
  ### Komplikasjonstyper is aggregated separately from the rest of the variables

  kompl_reactive <- reactive({
    if (user$role() == 'SC') {
      reshid = input$reshId_var
    } else {
      reshid = user$org()
    }
    test <- deformitet::kompl_data(regdata, reshid)
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


  deformitet::module_kvalitetsindikator_server("kval1",
                                               db_data = map_db_resh,
                                               userRole = user$role,
                                               userUnitId = user$org)

  ################################################################################
  ##### TAB: Sammenligning #####################################################


  deformitet::module_sammenligning_server("sam1",
                                          userRole = user$role,
                                          userUnitId = user$org)

  ################################################################################
  ##### TAB: SPC #################################################################

  # Add ready-made module here if requested by registry


  ################################################################################
  ##### TAB: Nestlasting av datadump #############################################


  deformitet::module_datadump_server("module_1",
                                     userRole = user$role,
                                     userUnitId = user$org)

################################################################################
###### TAB: Exporting data #####################################################

  shiny::observeEvent(
    shiny::req(user$role()), {
      if (user$role() != "SC") {
        shiny::hideTab("tabs", target = "Eksport")
      }
  })

  # Brukerkontroller

  rapbase::exportUCServer("deformitetExport", "deformitet")

  # Veiledning

  rapbase::exportGuideServer("deformitetExportGuide", "deformitet")


}

