#' @title Function for getting explanations for kvalitetsindikatorer
#'
#' @export


explanation_kvalind <- function(kjønn_choice, var){

  data <- data.frame(header = "", text = "")


  data <- data %>%
    dplyr::mutate(text =
                    dplyr::case_when(
                      {{kjønn_choice}} == "begge" ~
                        dplyr::case_match({{var}},
                                          "SRS22_spm22_3mnd" ~
                                            paste0("Figuren viser fordelingen av andel pasienter som svarte 'definitivt ja' eller 'sannsynligvis ja' på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall pasienter som svarte 'definitivt ja' eller 'sannsynligvis ja' / antall opererte *100", "<br/>"),
                                          "SRS22_spm21_3mnd" ~
                                            paste0("Figuren viser fordelingen av andel pasienter som svarte 'svært godt fornøyd' eller 'ganske fornøyd' på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall pasienter som svarte 'svært godt fornøyd' eller 'ganske fornøyd' / antall opererte *100", "<br/>"),
                                          "PRE_MAIN_CURVE"~
                                            paste0("Figuren viser fordelingen av andel pasienter som hadde pre-operativ kurve over 70 grader på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall pasienter med preoperativ kurve > 70 grader / antall opererte *100", "<br/>"),
                                          "Liggetid" ~
                                            paste0("Figuren viser fordelingen av andel pasienter med post-operativ liggetid på 7 eller flere dager på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall pasienter med post-operativ liggetid 7 dager eller mer / antall opererte *100", "<br/>"),
                                          "Komplikasjoner_3mnd" ~
                                            paste0("Figuren viser fordelingen av andel pasienter som rapporterte komplikasjoner (med unntak av smerte) ved 3-6 oppfølging på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall pasienter som rapporterer komplikasjoner (med unntak av smerte) ved 3-6 mnds oppfølging / antall opererte *100", "<br/>"),
                                          "CURRENT_SURGERY" ~
                                            paste0("Figuren viser reoperasjonsrate på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall reoperasjoner / antall operasjoner *100", "<br/>")),

                      {{kjønn_choice}} == "mann"~
                        dplyr::case_match({{var}},
                                          "SRS22_spm22_3mnd" ~
                                            paste0("Figuren viser fordelingen av andel mannlige pasienter som svarte 'definitivt ja' eller 'sannsynligvis ja' på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall mannlige pasienter som svarte 'definitivt ja' eller 'sannsynligvis ja' / antall opererte menn *100", "<br/>"),
                                          "SRS22_spm21_3mnd" ~
                                            paste0("Figuren viser fordelingen av andel mannlige pasienter som svarte 'svært godt fornøyd' eller 'ganske fornøyd' på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall mannlige pasienter som svarte 'svært godt fornøyd' eller 'ganske fornøyd' / antall opererte menn *100", "<br/>"),
                                          "PRE_MAIN_CURVE"~
                                            paste0("Figuren viser fordelingen av andel mannlige pasienter som hadde pre-operativ kurve over 70 grader på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall mannlige pasienter med preoperativ kurve > 70 grader / antall opererte menn *100", "<br/>"),
                                          "Liggetid" ~
                                            paste0("Figuren viser fordelingen av andel mannlige pasienter med post-operativ liggetid på 7 eller flere dager på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall mannlige pasienter med post-operativ liggetid 7 dager eller mer / antall opererte menn *100", "<br/>"),
                                          "Komplikasjoner_3mnd" ~
                                            paste0("Figuren viser fordelingen av andel mannlige pasienter som rapporterte komplikasjoner (med unntak av smerte) ved 3-6 oppfølging på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall mannlige pasienter som rapporterer komplikasjoner (med unntak av smerte) ved 3-6 mnds oppfølging / antall opererte menn *100", "<br/>"),
                                          "CURRENT_SURGERY" ~
                                            paste0("Figuren viser reoperasjonsrate for menn på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall reopererte menn / antall opererte menn *100", "<br/>")),

                      {{kjønn_choice}} == "kvinne" ~
                        dplyr::case_match({{var}},
                                          "SRS22_spm22_3mnd" ~
                                            paste0("Figuren viser fordelingen av andel kvinnelige pasienter som svarte 'definitivt ja' eller 'sannsynligvis ja' på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall kvinnelige pasienter som svarte 'definitivt ja' eller 'sannsynligvis ja' / antall opererte kvinner * 100", "<br/>"),
                                          "SRS22_spm21_3mnd" ~
                                            paste0("Figuren viser fordelingen av andel kvinnelige pasienter som svarte 'svært godt fornøyd' eller 'ganske fornøyd' på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall kvinnelige pasienter som svarte 'svært godt fornøyd' eller 'ganske fornøyd' / antall opererte kvinner * 100", "<br/>"),
                                          "PRE_MAIN_CURVE"~
                                            paste0("Figuren viser fordelingen av andel kvinnelige pasienter som hadde pre-operativ kurve over 70 grader på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall kvinnelige pasienter med preoperativ kurve > 70 grader / antall opererte kvinner *100", "<br/>"),
                                          "Liggetid" ~
                                            paste0("Figuren viser fordelingen av andel kvinnelige pasienter med post-operativ liggetid på 7 eller flere dager på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall kvinnelige pasienter med post-operativ liggetid 7 dager eller mer / antall opererte kvinner *100", "<br/>"),
                                          "Komplikasjoner_3mnd" ~
                                            paste0("Figuren viser fordelingen av andel kvinnelige pasienter som rapporterte komplikasjoner (med unntak av smerte) ved 3-6 oppfølging på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall kvinnelige pasienter som rapporterer komplikasjoner (med unntak av smerte) ved 3-6 mnds oppfølging / antall opererte kvinner *100", "<br/>"),
                                          "CURRENT_SURGERY" ~
                                            paste0("Figuren viser reoperasjonsrate for kvinner på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall reopererte kvinner / antall opererte kvinner *100", "<br/>")),

                      {{kjønn_choice}} == "nei" ~
                        dplyr::case_match({{var}},
                                          "SRS22_spm22_3mnd" ~
                                          paste0("Figuren viser fordelinger av andel pasienter (av begge kjønn),
                                          andel kvinnelige pasienter og andel mannlige pasienter som svarte
                                          'definitivt ja' eller 'sannsynligvis ja' på de ulike sykehusene.
                                                 For hvert sykehus er hver andel regnet ut slik:", "<br/>",
                                                 "antall pasienter som svarte 'definitivt ja' og 'sannsynligvis ja' / antall opererte * 100", "<br/>",
                                                 "antall kvinnelige pasienter som svarte 'definitivt ja' og 'sannsynligvis ja' / antall opererte kvinner * 100", "<br/>",
                                                 "antall mannlige pasienter som svarte 'definitivt ja' og 'sannsynligvis ja' / antall opererte menn * 100", "<br/>"),

                                          "SRS22_spm21_3mnd" ~
                                          paste0("Figuren viser fordelinger av andel pasienter (av begge kjønn),
                                          andel kvinnelige pasienter og andel mannlige pasienter som svarte
                                          'svært godt fornøyd' eller 'ganske fornøyd' på de ulike sykehusene.
                                                 For hvert sykehus er hver andel regnet ut slik:", "<br/>",
                                                 "antall pasienter som svarte 'svært godt fornøyd' og 'ganske fornøyd' / antall opererte * 100", "<br/>",
                                                 "antall kvinnelige pasienter som svarte 'svært godt fornøyd' og 'ganske fornøyd' / antall opererte kvinner * 100", "<br/>",
                                                 "antall mannlige pasienter som svarte 'svært godt fornøyd' og 'ganske fornøyd' / antall opererte menn * 100", "<br/>"),

                                          "PRE_MAIN_CURVE"~
                                          paste0("Figuren viser fordelinger av andel pasienter (av begge kjønn),
                                          andel kvinnelige pasienter og andel mannlige pasienter som hadde
                                          pre-operativ kurve over 70 grader på de ulike sykehusene.
                                                 For hvert sykehus er hver andel regnet ut slik:", "<br/>",
                                                 "antall pasienter med preoperativ kurve > 70 grader / antall opererte *100", "<br/>",
                                                 "antall kvinnelige pasienter med preoperativ kurve > 70 grader / antall opererte kvinner *100", "<br/>",
                                                 "antall mannlige pasienter med preoperativ kurve > 70 grader / antall opererte menn *100", "<br/>"),

                                          "Liggetid" ~
                                          paste0("Figuren viser fordelinger av andel pasienter (av begge kjønn),
                                          andel kvinnelige pasienter og andel mannlige pasienter med
                                          post-operativ liggetid på 7 eller flere dager på de ulike sykehusene.
                                                 For hvert sykehus er hver andel regnet ut slik:", "<br/>",
                                                 "antall pasienter med post-operativ liggetid 7 dager eller mer / antall opererte *100", "<br/>",
                                                 "antall kvinnelige pasienter med post-operativ liggetid 7 dager eller mer / antall opererte kvinner *100", "<br/>",
                                                 "antall mannlige pasienter med post-operativ liggetid 7 dager eller mer / antall opererte menn *100", "<br/>"),

                                          "Komplikasjoner_3mnd" ~
                                          paste0("Figuren viser fordelinger av andel pasienter (av begge kjønn),
                                          andel kvinnelige pasienter og andel mannlige pasienter som
                                          rapporterte komplikasjoner (med unntak av smerte) ved 3-6 oppfølging på de ulike sykehusene
                                          Smerte som komplikasjon er tatt ut fra denne utregningen ettersom dette er
                                          en komplikasjon som i høy grad er forventet etter operasjonen.
                                                 For hvert sykehus er hver andel regnet ut slik:", "<br/>",
                                                 "antall pasienter som rapporterer komplikasjoner (unntatt smerte) ved 3-6 mnds oppfølging / antall opererte *100", "<br/>",
                                                 "antall kvinnelige pasienter som rapporterer komplikasjoner (unntatt smerte) ved 3-6 mnds oppfølging / antall opererte kvinner *100", "<br/>",
                                                 "antall mannlige pasienter som rapporterer komplikasjoner (unntatt smerte) ved 3-6 mnds oppfølging / antall opererte menn *100", "<br/>"),

                                          "CURRENT_SURGERY" ~
                                          paste0("Figuren viser reoperasjonsrate for begge kjønn,
                                          kvinnelige pasienter og mannlige pasienter.
                                                 For hvert sykehus er hver rate regnet ut slik:", "<br/>",
                                                 "antall reoperasjoner / antall operasjoner *100", "<br/>",
                                                 "antall reopererte kvinner / antall opererte kvinner *100", "<br/>",
                                                 "antall reopererte menn / antall opererte menn *100", "<br/>")
                                             )
                                          ),
                  header =
                    dplyr::case_match({{var}},
                                      "SRS22_spm22_3mnd" ~ "SRS22 'Samme behandling på nytt?' 3-6 mnd:",
                                      "SRS22_spm21_3mnd" ~ "SRS22 'Fornøyd med behandlingen?' 3-6 mnd:",
                                      "PRE_MAIN_CURVE"~ "Pre-operativ kurve:",
                                      "Liggetid" ~ "Liggetid:",
                                      "Komplikasjoner_3mnd" ~ "Komplikasjoner 3-6 mnd:",
                                      "CURRENT_SURGERY" ~ "Reoperasjonsrate:")

    )


  return(data)

}

# Testing that it works
##g <- explanation_kvalind("begge", "SRS22_spm22_3mnd")
