#' Client (ui) for the rapRegTemplate app
#'
#' @return An shiny app ui object
#' @export

app_ui <- function() {

  shiny::addResourcePath("rap", system.file("www", package = "rapbase"))
  regTitle <- "rapRegTemplate"

  shiny::tagList(
    shiny::navbarPage(
      title = shiny::div(shiny::a(shiny::includeHTML(system.file("www/logo.svg",
                                            package = "rapbase"))),
                  regTitle),
      windowTitle = regTitle,
      theme = "rap/bootstrap.css",
      id = "tabs",

      shiny::tabPanel("Veiledning",
                      shiny::mainPanel(width = 12,
                                       shiny::htmlOutput("veiledning", inline = TRUE),
                                       rapbase::appNavbarUserWidget(
                                         user = shiny::uiOutput("appUserName"),
                                         organization = shiny::uiOutput("appOrgName"),
                                         addUserInfo = TRUE)
                      )
      ),
      shiny::tabPanel("Figur og tabell"
                       ,
                       shiny::sidebarLayout(
                         shiny::sidebarPanel(width = 3,
                           shiny::selectInput(inputId = "var",
                                       label = "Variabel:",
                                       c("mpg", "disp", "hp", "drat", "wt", "qsec")),
                           shiny::sliderInput(inputId = "bins",
                                       label = "Antall grupper:",
                                       min = 1,
                                       max = 10,
                                       value = 5)
                         ),
                         shiny::mainPanel(
                           shiny::tabsetPanel(
                             shiny::tabPanel("Figur", shiny::plotOutput("distPlot")),
                             shiny::tabPanel("Tabell", shiny::tableOutput("distTable"))
                           )
                         )
                       )
      ),
      shiny::tabPanel("Samlerapport"
                       ,
                       shiny::tabPanel("Fordeling av mpg",
                         shiny::sidebarLayout(
                           shiny::sidebarPanel(
                             width = 3,
                             shiny::selectInput(
                               inputId = "varS",
                               label = "Variabel:",
                               c("mpg", "disp", "hp", "drat", "wt", "qsec")),
                             shiny::sliderInput(
                               inputId = "binsS",
                               label = "Antall grupper:",
                               min = 1,
                               max = 10,
                               value = 5),
                             shiny::selectInput(
                               inputId = "formatS",
                               label = "Velg format for nedlasting:",
                               choices = list(PDF = "pdf", HTML = "html")
                             ),
                             shiny::downloadButton(
                               outputId = "downloadSamlerapport",
                               label = "Last ned!")
                           ),
                           shiny::mainPanel(
                             shiny::uiOutput("samlerapport")
                           )
                         )
                       )
      ),
      shiny::tabPanel("Abonnement"
                       ,
                       shiny::sidebarLayout(
                         shiny::sidebarPanel(width = 3,
                           shiny::selectInput("subscriptionRep", "Rapport:",
                                       c("Samlerapport1", "Samlerapport2")),
                           shiny::selectInput("subscriptionFreq", "Frekvens:",
                                       list("\u212brlig" = "\u212brlig-year",
                                            Kvartalsvis = "Kvartalsvis-quarter",
                                            "M\u00e5nedlig" = "M\u00e5nedlig-month",
                                            Ukentlig = "Ukentlig-week",
                                            Daglig = "Daglig-DSTday"),
                                       selected = "M\u00e5nedlig-month"),
                           shiny::actionButton("subscribe", "Bestill",
                                        icon = shiny::icon("paper-plane"))
                         ),
                         shiny::mainPanel(
                           shiny::uiOutput("subscriptionContent")
                         )
                       )
      ),
      shiny::tabPanel(
        shiny::span("Abonnement v2",
                    title="Bestill tilsending av rapporter p\u00e5 e-post"),
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::autoReportInput("testSubscription")
          ),
          shiny::mainPanel(
            rapbase::autoReportUI("testSubscription")
          )
        )
      ),
      shiny::tabPanel("Utsending"
                       ,
                       shiny::sidebarLayout(
                         shiny::sidebarPanel(width = 3,
                           shiny::uiOutput("report"),
                           shiny::uiOutput("freq"),
                           shiny::textInput("email", "Epostmottakere:"),
                           shiny::uiOutput("editEmail"),
                           shiny::htmlOutput("recipients"),
                           shiny::tags$hr(),
                           shiny::uiOutput("makeDispatchment")
                         ),
                         shiny::mainPanel(
                           shiny::uiOutput("dispatchmentContent")
                         )
                       )
      )

    ) # navbarPage
  ) # tagList
}
