#' Server logic for the deformitet app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

app_server <- function(input, output, session) {
  rapbase::appLogger(session = session, msg = 'Starter Deformitet')
  reshID <- rapbase::getUserReshId(session)
  userRole <- rapbase::getUserRole(session)

  # Last inn data
  regData <- getFakeRegData()

  # Brukerinformasjon i menylinja (navbar)
  output$appUserName <-
    shiny::renderText(
      paste(rapbase::getUserFullName(session),
            rapbase::getUserRole(session), sep = ", "))
  output$appOrgName <- shiny::renderText(rapbase::getUserReshId(session))
  userInfo <-
    rapbase::howWeDealWithPersonalData(session, callerPkg = "deformitet")
  shiny::observeEvent(input$userInfo, {
    shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
               type = "", imageUrl = "rap/logo.svg",
               closeOnEsc = TRUE, closeOnClickOutside = TRUE,
               html = TRUE, confirmButtonText = rapbase::noOptOutOk())
  })

  # Veiledning
  output$veiledning <- shiny::renderUI({
    rapbase::renderRmd(
      system.file("veiledning.Rmd", package = "deformitet"),
      outputType = "html_fragment"
    )
  })


  # Figur og tabell
  # Figur
   output$distPlot <- shiny::renderPlot({
    makeHist(df = regData, var = input$var, bins = input$bins)
   })

  # Tabell
  output$distTable <- shiny::renderTable({
    makeHist(df = regData, var = input$var, bins = input$bins,
             makeTable = TRUE)
  })


  # Samlerapport
  ## vis
  output$samlerapport <- shiny::renderUI({
    rapbase::renderRmd(
      system.file("samlerapport.Rmd", package = "deformitet"),
      outputType = "html_fragment",
      params = list(type = "html",
                    var = input$varS,
                    bins = input$binsS)
    )
  })

  ## last ned
  output$downloadSamlerapport <- shiny::downloadHandler(
    filename = function() {
      basename(tempfile(pattern = "deformitetSamlerapport",
                        fileext = paste0(".", input$formatS)))
    },
    content = function(file) {
      srcFile <-
        normalizePath(system.file("samlerapport.Rmd", package = "deformitet"))
      fn <- rapbase::renderRmd(srcFile, outputType = input$formatS,
                               params = list(type = input$formatS,
                                             var = input$varS,
                                             bins = input$binsS))
      file.rename(fn, file)
    }
  )

  ## nye abonnement
  ## Objects currently shared among subscription and dispathcment
  orgs <- list(Sykehus1 = 1234,
               Sykehus2 = 4321)
  reports <- list(
    Samlerapport1 = list(
      synopsis = "Automatisk samlerapport1",
      fun = "samlerapport1Fun",
      paramNames = c("p1", "p2"),
      paramValues = c("Alder", 1)
    ),
    Samlerapport2 = list(
      synopsis = "Automatisk samlerapport2",
      fun = "samlerapport2Fun",
      paramNames = c("p1", "p2"),
      paramValues = c("BMI", 1)
    )
  )

  ## Subscription
  rapbase::autoReportServer(
    id = "testSubscription", registryName = "deformitet",
    type = "subscription", reports = reports, orgs = orgs, freq = "quarter"
  )

  ## Dispatchment
  org <- rapbase::autoReportOrgServer("deformitetDispatch", orgs)

  paramNames <- shiny::reactive(c("reshID"))
  paramValues <- shiny::reactive(c(org$value()))

  rapbase::autoReportServer(
    id = "deformitetDispatch", registryName = "deformitet",
    type = "dispatchment", org = org$value, paramNames = paramNames,
    paramValues = paramValues, reports = reports, orgs = orgs,
    eligible = (userRole == "SC"), freq = "quarter"
  )

  # Eksport
  rapbase::exportUCServer("deformitetExport", "deformitet")
  ## veileding
  rapbase::exportGuideServer("deformitetExportGuide", "deformitet")



}
