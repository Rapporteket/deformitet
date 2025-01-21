#'@title Ui sammenligningsmodul
#'
#'@export

module_sammenligning_UI <- function (id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        selectInput( # First select
          inputId = ns("comp1"),
          label = "Velg variabel 1 (før operasjon) eller ved 3mnd oppfølging:",
          choices = c("SRS22 totalskår" = "SRS22_MAIN_SCORE",
                      "SRS22 totalskår 3mnd" = "SRS22_FULL_SCORE",
                      "SRS22 mental helse" = "SRS22_MENTALHEALTH_SCORE",
                      "SRS22 mental helse 3mnd" = "SRS22_MENTALHEALTH_SCORE_patient3mths",
                      "SRS22 smerte" = "SRS22_PAIN_SCORE",
                      "SRS22 smerte 3mnd" = "SRS22_PAIN_SCORE_patient3mths",
                      "Pre-operativ Kurve" = "PRE_MAIN_CURVE",
                      "Helsetilstand" = "Helsetilstand",
                      "Helsetilstand 3mnd" = "Helsetilstand_3mnd"
          ),
          selected = "PRE_MAIN_CURVE"),

        conditionalPanel( ### THIS IS NOT WORKING YET
          condition =
            "input.comp1 == 'SRS22_MAIN_SCORE' ||
            input.comp1 == 'SRS22_FULL_SCORE' ||
            input.comp1 == 'SRS22_MENTALHEALTH_SCORE' ||
            input.comp1 == 'SRS22_MENTALHEALTH_SCORE_patient3mths' ||
            input.comp1 == 'SRS22_PAIN_SCORE' ||
            input.comp1 == 'SRS22_PAIN_SCORE_patient3mths'",

          selectInput(
            inputId = ns("comp2_a"),
            label = "Velg variabel 2 (etter operasjon - 3-60 mnd oppfølging):",
            choices = c("SRS22 totalskår 3mnd" = "SRS22_FULL_SCORE",
                        "SRS22 totalskår 12mnd" = "SRS22_FULL_SCORE_patient12mths",
                        "SRS22 totalskår 60mnd" = "SRS22_FULL_SCORE_patient60mths",
                        "SRS22 mental helse 3mnd" = "SRS22_MENTALHEALTH_SCORE_patient3mths",
                        "SRS22 mental helse 12mnd" = "SRS22_MENTALHEALTH_SCORE_patient12mths",
                        "SRS22 mental helse 60mnd" = "SRS22_MENTALHEALTH_SCORE_patient60mths",
                        "SRS22 smerte 3mnd" = "SRS22_PAIN_SCORE_patient3mths",
                        "SRS22 smerte 12mnd" = "SRS22_PAIN_SCORE_patient12mths",
                        "SRS22 smerte 60mnd" = "SRS22_PAIN_SCORE_patient60mths"
                        ),
            selected = "SRS22_FULL_SCORE"),
      )
      ),
      shiny::mainPanel(
      )
    )
  )
}


#'@title Server sammenligningsmodul
#'
#'@export

comparison_module_server <- function () {


}
