#' Server logic for the rapRegTemplate app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

app_server <- function(input, output, session) {

  # Last inn data
  regData <- getFakeRegData()

  # Brukerinformasjon i menylinja (navbar)
  output$appUserName <-
    shiny::renderText(
      paste(rapbase::getUserFullName(session),
            rapbase::getUserRole(session), sep = ", "))
  output$appOrgName <- shiny::renderText(rapbase::getUserReshId(session))
  userInfo <-
    rapbase::howWeDealWithPersonalData(session, callerPkg = "rapRegTemplate")
  shiny::observeEvent(input$userInfo, {
    shinyalert::shinyalert("Dette vet Rapporteket om deg:", userInfo,
               type = "", imageUrl = "rap/logo.svg",
               closeOnEsc = TRUE, closeOnClickOutside = TRUE,
               html = TRUE, confirmButtonText = rapbase::noOptOutOk())
  })

  # Veiledning
  output$veiledning <- shiny::renderUI({
    rapbase::renderRmd(
      system.file("veiledning.Rmd", package = "rapRegTemplate"),
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
      system.file("samlerapport.Rmd", package = "rapRegTemplate"),
      outputType = "html_fragment",
      params = list(type = "html",
                    var = input$varS,
                    bins = input$binsS)
    )
  })

  ## last ned
  output$downloadSamlerapport <- shiny::downloadHandler(
    filename = function() {
      basename(tempfile(pattern = "rapRegTemplateSamlerapport",
                        fileext = paste0(".", input$formatS)))
    },
    content = function(file) {
      srcFile <-
        normalizePath(system.file("samlerapport.Rmd", package = "rapRegTemplate"))
      fn <- rapbase::renderRmd(srcFile, outputType = input$formatS,
                               params = list(type = input$formatS,
                                             var = input$varS,
                                             bins = input$binsS))
      file.rename(fn, file)
    }
  )


  # Abonnement
  ## rekative verdier for aa holde rede paa endringer som skjer mens
  ## applikasjonen kjorer
  subscription <- shiny::reactiveValues(
    tab = rapbase::makeAutoReportTab(session, type = "subscription"))

  ## lag tabell over gjeldende status for abonnement
  output$activeSubscriptions <- DT::renderDataTable(
    subscription$tab, server = FALSE, escape = FALSE, selection = "none",
    options = list(dom = "tp", ordning = FALSE,
                   columnDefs = list(list(visible = FALSE, targets = 6))),
    rownames = FALSE
  )

  ## lag side som viser status for abonnement, ogsaa naar det ikke finnes noen
  output$subscriptionContent <- shiny::renderUI({
    userFullName <- rapbase::getUserFullName(session)
    if (length(subscription$tab) == 0) {
      shiny::p(paste("Ingen aktive abonnement for", userFullName))
    } else {
      shiny::tagList(
        shiny::p(paste0("Aktive abonnement som sendes per epost til ",
                        userFullName, ":")),
        DT::dataTableOutput("activeSubscriptions")
      )
    }
  })

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
    id = "testSubscription", registryName = "rapRegTemplate",
    type = "subscription", reports = reports, orgs = orgs, freq = "quarter"
  )

  # Utsending
  ## reaktive verdier for aa holde rede paa endringer som skjer mens
  ## applikasjonen kjorer
  dispatchment <- shiny::reactiveValues(
    tab = rapbase::makeAutoReportTab(session = session, type = "dispatchment"),
    report = "Automatisk samlerapport1",
    freq = "M\u00E5nedlig-month",
    email = vector()
  )

  ## observer og foreta endringer mens applikasjonen kjorer
  shiny::observeEvent(input$addEmail, {
    dispatchment$email <- c(dispatchment$email, input$email)
  })
  shiny::observeEvent(input$delEmail, {
    dispatchment$email <-
      dispatchment$email[!dispatchment$email == input$email]
  })
  shiny::observeEvent(input$dispatch, {
    package <- "rapRegTemplate"
    type <- "dispatchment"
    owner <- rapbase::getUserName(session)
    ownerName <- rapbase::getUserFullName(session)
    interval <- strsplit(input$dispatchmentFreq, "-")[[1]][2]
    intervalName <- strsplit(input$dispatchmentFreq, "-")[[1]][1]
    runDayOfYear <- rapbase::makeRunDayOfYearSequence(
      interval = interval)

    email <- dispatchment$email
    organization <- rapbase::getUserReshId(session)

    if (input$dispatchmentRep == "Automatisk samlerapport1") {
      synopsis <- "Automatisk samlerapport1"
      fun <- "samlerapport1Fun"
      paramNames <- c("p1", "p2")
      paramValues <- c("Alder", 1)

    }
    if (input$dispatchmentRep == "Automatisk samlerapport2") {
      synopsis <- "Automatisk samlerapport2"
      fun <- "samlerapport2Fun"
      paramNames <- c("p1", "p2")
      paramValues <- c("BMI", 2)
    }
    rapbase::createAutoReport(synopsis = synopsis, package = package,
                              type = type, fun = fun, paramNames = paramNames,
                              paramValues = paramValues, owner = owner,
                              ownerName = ownerName,
                              email = email, organization = organization,
                              runDayOfYear = runDayOfYear,
                              interval = interval, intervalName = intervalName)
    dispatchment$tab <-
      rapbase::makeAutoReportTab(session, type = "dispatchment")
    dispatchment$email <- vector()
  })

  ## ui: velg rapport
  output$report <- shiny::renderUI({
    shiny::selectInput(
      "dispatchmentRep", "Rapport:",
      c("Automatisk samlerapport1", "Automatisk samlerapport2"),
      selected = dispatchment$report)
  })

  ## ui: velg frekvens
  output$freq <- shiny::renderUI({
    shiny::selectInput(
      "dispatchmentFreq", "Frekvens:",
      list("\u00C5rlig" = "\u00C5rlig-year",
            Kvartalsvis = "Kvartalsvis-quarter",
            Maanedlig = "M\u00E5nedlig-month",
            Ukentlig = "Ukentlig-week",
            Daglig = "Daglig-DSTday"),
      selected = dispatchment$freq)
  })

  ## ui: legg til gyldig- og slett epost
  output$editEmail <- shiny::renderUI({
    if (!grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$",
               input$email)) {
      shiny::tags$p("Angi mottaker over")
    } else {
      if (input$email %in% dispatchment$email) {
        shiny::actionButton("delEmail", "Slett epostmottaker",
                            icon = shiny::icon("trash"))
      } else {
        shiny::actionButton("addEmail", "Legg til epostmottaker",
                            icon = shiny::icon("pencil"))
      }
    }
  })

  ## ui: vis valgte mottakere
  output$recipients <- shiny::renderText(paste(dispatchment$email,
                                               sep = "<br>"))

  ## ui: lag ny utsending
  output$makeDispatchment <- shiny::renderUI({
    if (length(dispatchment$email) == 0) {
      NULL
    } else {
      shiny::actionButton("dispatch", "Lag utsending",
                          icon = shiny::icon("save"))
    }
  })

  ## lag tabell over gjeldende status for utsending
  output$activeDispatchments <- DT::renderDataTable(
        dispatchment$tab, server = FALSE, escape = FALSE, selection = "none",
        options = list(dom = "tp", ordering = FALSE), rownames = FALSE
  )


  ## ui: lag side som viser status for utsending, ogsaa naar det ikke finnes noen
  output$dispatchmentContent <- shiny::renderUI({
    if (length(dispatchment$tab) == 0) {
      shiny::p("Det finnes ingen utendinger")
    } else {
      shiny::tagList(
        shiny::p("Aktive utsendinger:"),
        DT::dataTableOutput("activeDispatchments")
      )
    }
  })

  # Rediger eksisterende auto rapport (alle typer)
  shiny::observeEvent(input$edit_button, {
    repId <- strsplit(input$edit_button, "_")[[1]][2]
    rep <- rapbase::readAutoReportData()[[repId]]
    if (rep$type == "subscription") {

    }
    if (rep$type == "dispatchment") {
      dispatchment$freq <- paste0(rep$intervalName, "-", rep$interval)
      dispatchment$email <- rep$email
      rapbase::deleteAutoReport(repId)
      dispatchment$tab <-
        rapbase::makeAutoReportTab(session, type = "dispatchment")
      dispatchment$report <- rep$synopsis
    }
    if (rep$type == "bulletin") {

    }
  })

  # Slett eksisterende auto rapport (alle typer)
  shiny::observeEvent(input$del_button, {
    repId <- strsplit(input$del_button, "_")[[1]][2]
    rapbase::deleteAutoReport(repId)
    subscription$tab <-
      rapbase::makeAutoReportTab(session, type = "subscription")
    dispatchment$tab <-
      rapbase::makeAutoReportTab(session, type = "dispatchment")
  })

}
