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
          )),

        conditionalPanel(
          condition =
            "input.comp1 == 'SRS22_MAIN_SCORE' ||
            input.comp1 == 'SRS22_FULL_SCORE' ||
            input.comp1 == 'SRS22_MENTALHEALTH_SCORE' ||
            input.comp1 == 'SRS22_MENTALHEALTH_SCORE_patient3mths' ||
            input.comp1 == 'SRS22_PAIN_SCORE' ||
            input.comp1 == 'SRS22_PAIN_SCORE_patient3mths'",
          ns = ns,

          selectInput(ns("comp2_a"),
                      label = "Velg variabel 2 (etter operasjon - 3-60 mnd oppfølging):",
                      choices = c("SRS22 totalskår 3mnd" =
                                    "SRS22_FULL_SCORE",
                                  "SRS22 totalskår 12mnd" =
                                    "SRS22_FULL_SCORE_patient12mths",
                                  "SRS22 totalskår 60mnd" =
                                    "SRS22_FULL_SCORE_patient60mths",
                                  "SRS22 mental helse 3mnd" =
                                    "SRS22_MENTALHEALTH_SCORE_patient3mths",
                                  "SRS22 mental helse 12mnd" =
                                    "SRS22_MENTALHEALTH_SCORE_patient12mths",
                                  "SRS22 mental helse 60mnd" =
                                    "SRS22_MENTALHEALTH_SCORE_patient60mths",
                                  "SRS22 smerte 3mnd" =
                                    "SRS22_PAIN_SCORE_patient3mths",
                                  "SRS22 smerte 12mnd" =
                                    "SRS22_PAIN_SCORE_patient12mths",
                                  "SRS22 smerte 60mnd" =
                                    "SRS22_PAIN_SCORE_patient60mths"
                                  )
                      )
      ),

      conditionalPanel(
        condition = "input.comp1 == 'PRE_MAIN_CURVE'",
        ns = ns,
        selectInput(ns("comp2_b"),
                    label = "Velg variabel 2 (etter operasjon - 3-60 mnd oppfølging):",
                    choices = c("Post-operativ kurve" = "POST_MAIN_CURVE"),
                    )

      ),

      conditionalPanel(
        condition =
        "input.comp1 == 'Helsetilstand' ||
        input.comp1 == 'Helsetilstand_3mnd'",
        ns = ns,
        selectInput(ns("comp2_c"),
                    label = "Velg variabel 2 (etter operasjon - 3-60 mnd oppfølging):",
                    choices = c("Helsetilstand 3mnd" = "Helsetilstand_3mnd",
                                "Helsetilstand 12mnd" = "Helsetilstand_12mnd",
                                "Helsetilstand 60 mnd" = "Helsetilsand_60mnd")
                    )
      ),

      shiny::radioButtons( # third select
        inputId = ns("kjønn_var"),
        label = "Dele på kjønn?",
        choices = c("kvinne" = "kvinne",
                    "mann" = "mann",
                    "begge" = "begge",
                    "se alle fordelinger" = "nei"),
        selected = "begge"
        ),

      dateRangeInput( # fourth select
        inputId = ns("date"),
        label = "Tidsintervall:",
        start = "2023-01-02",
        end = "2024-09-02",
        min = "2023-01-01",
        max = "2025-09-02",
        format = "mm/dd/yy",
        separator = " - "
        ),

      shinyWidgets::chooseSliderSkin("Flat", color = "#112446"),
      sliderInput( # fifth select
        inputId = ns("alder_var"),
        label = "Aldersintervall:",
        min = 0,
        max = 100,
        value = c(10, 20),
        dragRange = TRUE
        ),

      radioButtons( # sixth select
        inputId = "type_op",
        label = "Type operasjon",
        choices = c("Primæroperasjon", "Reoperasjon", "Begge"),
        selected = "Primæroperasjon"
      ),


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
