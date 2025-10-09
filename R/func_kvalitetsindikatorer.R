## Funksjon for å telle forløp pr kvalitetsindikator
#' @title Kvalitetsindikatortelling
#' @export

count_kvalind <- function (data, kjoenn, var, userRole, userUnitId, map_data) {

  # Legge til kolonne med telling av komplikasjoner der smerte er tatt ut
  data <- ny_komplikasjon3mnd_usmerte(data)


  # Telle forløp

  my_tiny_data <- data %>%
    dplyr::group_by(Sykehus, Kjønn) %>%
    dplyr::add_tally(name = "n") %>% # antall pasienter per sykehus per kjønn
    dplyr::ungroup() %>%
    dplyr::filter(dplyr::case_when({{var}} == "PRE_MAIN_CURVE" ~
                                     PRE_MAIN_CURVE > 70,
                                   {{var}} == "Komplikasjoner_3mnd" ~
                                     komplikasjoner_uSmerte_3mnd == "ja",
                                   {{var}} == "Liggetid" ~
                                     Liggetid == "> 7"| Liggetid == "7",
                                   {{var}} == "SRS22_spm21_3mnd" ~
                                     SRS22_spm21_3mnd == "Ganske fornøyd" |
                                     SRS22_spm21_3mnd == "Svært godt fornøyd",
                                   {{var}} == "CURRENT_SURGERY" ~
                                     CURRENT_SURGERY == 2,
                                   TRUE ~
                                     CURRENT_SURGERY == 1 | CURRENT_SURGERY == 2)) %>%
    dplyr::group_by(Sykehus, Kjønn) %>%
    dplyr::add_count(name = "antall_kval_syk_kjønn") %>%
    dplyr::ungroup() %>%
    dplyr::select(Sykehus, Kjønn, n, antall_kval_syk_kjønn) %>%
    dplyr::distinct()

  my_tiny_data_nasj <- data %>%
    dplyr::group_by(Kjønn) %>%
    dplyr::add_tally(name = "n") %>%
    dplyr::mutate(Sykehus = "Nasjonalt") %>%
    dplyr::relocate(Sykehus, .before = "Kjønn") %>%
    dplyr::ungroup() %>%
    dplyr::filter(dplyr::case_when({{var}} == "PRE_MAIN_CURVE" ~
                                     PRE_MAIN_CURVE > 70,
                                   {{var}} == "Komplikasjoner_3mnd" ~
                                     komplikasjoner_uSmerte_3mnd == "ja",
                                   {{var}} == "Liggetid" ~
                                     Liggetid == "> 7"| Liggetid == "7",
                                   {{var}} == "SRS22_spm21_3mnd" ~
                                     SRS22_spm21_3mnd == "Ganske fornøyd" |
                                     SRS22_spm21_3mnd == "Svært godt fornøyd",
                                   {{var}} == "CURRENT_SURGERY" ~
                                     CURRENT_SURGERY == 2,
                                   TRUE ~
                                     CURRENT_SURGERY == 1 | CURRENT_SURGERY == 2)) %>%
    dplyr::group_by(Kjønn) %>%
    dplyr::add_count(name = "antall_kval_syk_kjønn") %>%
    dplyr::ungroup() %>%
    dplyr::select(Sykehus, Kjønn, n, antall_kval_syk_kjønn) %>%
    dplyr::distinct()


  my_tiny_data_total <- rbind(my_tiny_data_nasj, my_tiny_data)


  my_begge <- my_tiny_data_total %>%
    dplyr::group_by(Sykehus) %>%
    dplyr::mutate(n = sum(n),
                  antall_kval_syk_kjønn = sum(antall_kval_syk_kjønn)) %>%
    dplyr::select(-c(Kjønn)) %>%
    dplyr::mutate(Kjønn = "begge") %>%
    dplyr::relocate(Kjønn, .before = "n") %>%
    dplyr::distinct()

  data_total <- full_join(my_tiny_data_total, my_begge)


  ######################### Calculate andeler ####################################

  data_total <- data_total %>%
    dplyr::mutate(
      andel_per_syk_kjønn =
        round(antall_kval_syk_kjønn/n*100, 2))


  ##### BRUKERVALG: #####

  if(kjoenn == "begge"){
    data_total <- data_total %>%
      dplyr::filter(Kjønn == "begge")
  }
  else{
    if(kjoenn == "kvinne"){
      data_total <- data_total %>%
        dplyr::filter(Kjønn == "kvinne")
    }
    else{
      if(kjoenn == "mann"){
        data_total <- data_total %>%
          dplyr::filter(Kjønn == "mann")
      }
    }
  }

  # Filter basert på brukertilhørighet:

  map_data <- map_data %>%
    dplyr::rename(CENTREID = UnitId,
                  Sykehus = orgname) %>%
    dplyr::add_row(CENTREID = "0",
                   Sykehus = "Nasjonalt")


  data_total <- left_join(data_total, map_data)

  if (userRole != "SC") {
    data_total <- data_total %>%
      dplyr::filter(CENTREID == {{userUnitId}} | CENTREID == "0")
  }

  data_total <- data_total %>%
    select(-c(CENTREID))


  return (data_total)
}


######### FUNCTION THAT COUNTS NUMBER OF FORLØPs WITH COMPLICATIONS OTHER THAN
######### PAIN ###############################################################

#'@title Ny komplikasjonskolonne
#'@export

ny_komplikasjon3mnd_usmerte <- function (data) {

  data <- data %>%
    dplyr::mutate(komplikasjoner_uSmerte_3mnd =
                    dplyr::if_else(COMPLICATIONS_BLEEDING == 1 |
                                     COMPLICATIONS_HEAD == 1 |
                                     COMPLICATIONS_DVT == 1 |
                                     COMPLICATIONS_UTI == 1 |
                                     COMPLICATIONS_PNEUMONIA == 1 |
                                     COMPLICATIONS_PE == 1 |
                                     COMPLICATIONS_INFECTION_WOUND == 1 |
                                     COMPLICATIONS_INFECTION_DEEP == 1 |
                                     COMPLICATIONS_INFECTION_REOP == 1 |
                                     COMPLICATIONS_NUMBNESS == 1 |
                                     COMPLICATIONS_OTHER == 1, "ja", "nei")) %>%
    dplyr::mutate(komplikasjoner_uSmerte_3mnd =
                    if_else(is.na(komplikasjoner_uSmerte_3mnd), "nei",
                            if_else(komplikasjoner_uSmerte_3mnd == "nei", "nei", "ja")))


  return (data)
}


# nolint start
## test for å sjekke at det fungerer:
## r <- count_kvalind(regdata, "ee", "Primæroperasjon", "PRE_MAIN_CURVE", "SC", 111961, map_db_resh)


#' @export

kval_plot <- function(data, gg_data, data_var, choice_kjønn){

  data <- data %>%
    dplyr::mutate(Sykehus = forcats::fct_relevel(Sykehus, "Nasjonalt", after = Inf))

  kval_plot <-
    ggplot2::ggplot(data = data, aes(x = andel_per_syk_kjønn,
                                     y = Sykehus,
                                     fill = Sykehus))+

    annotate('rect', xmin = gg_data$ymin, xmax = gg_data$ymax,
             ymin=-Inf, ymax=Inf, alpha=0.2, fill="palegreen4") +

    ggplot2::geom_col(alpha = .7)+

    #ggplot2::geom_hline(xintercept = gg_data$yintercept, linetype = "dashed", color = "#87189D")+

    # ggplot2::geom_rect(aes(ymin = 0, ymax = 5, xmin = x_start, xmax = x_end), alpha = .5)+

    ggplot2::scale_x_continuous(breaks = c(0, 5, 10,20,30,40,50,60,70,80,90,100))+

    #ggplot2::geom_col(data = kval, aes(x = Sykehus, y = andel_per_syk), color = "#003087" )+

    #### TITLES ################################################################

  ggplot2::xlab(paste("Andel pasienter (%)", gg_data$ylab))+

    ggplot2::labs(title = gg_data$title,
                  caption = paste0("**Valgte variabler:**", "\n", data_var[1,],
                                   ", ", "\n", "Kjønn: ", data_var[2,], "\n",
                                   "Dato: ", data_var[3,], "-", data_var[4,], "\n",
                                   "Alder: ", data_var[5,], "-", data_var[6,]))+

    ggplot2::geom_label(aes(x = 0, label = paste(antall_kval_syk_kjønn, "av", n)),
                        fill = "#BFCED6", color = "#003087", fontface = "italic",
                        position = position_dodge(.9), hjust = -.01, size = 3,
                        alpha = .8)+

    ggplot2::facet_wrap(~Kjønn) +


    ##### THEME AND COLOURS ####################################################

  ggplot2::theme_bw(base_size = 16)+ # light theme

    ggplot2::theme(plot.caption = element_text(color = "#87189D", # add caption
                                               face = "italic"),
                   legend.position = case_when({{choice_kjønn}} == "begge" ~ "none",
                                               TRUE ~ "right"),
                   axis.text.x = element_text(size = 10),
                   axis.text.y = element_text(size = 14),

                   axis.title.x = element_text(size = 12),

                   plot.title = element_text(size = 16))+

    ggplot2::scale_fill_manual(values = c("#6CACE4", "#6FA287", "#BFCED6", "#003087"),
                               guide = guide_legend(reverse = TRUE))



  return(kval_plot)
}

# nolint start
## Test for å se at det kjører
##y <- kval_plot(r, gg_data, data_var, "nei")
##y
# nolint end

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
                                            paste0("Figuren viser fordelingen av andel pasienter som svarte 'definitivt ja' eller 'sannsynligvis ja' på spørsmål om de ville ønsket samme operasjon på nytt ved 3-6 måneders oppfølging. For hvert sykehus er det regnet ut slik:", "<br/>", "antall pasienter som svarte 'definitivt ja' eller 'sannsynligvis ja' / antall opererte *100", "<br/>"),
                                          "SRS22_spm21_3mnd" ~
                                            paste0("Figuren viser fordelingen av andel pasienter som svarte 'svært godt fornøyd' eller 'ganske fornøyd' på spørsmål om de er fornøyde med resultatet av operasjonen ved 3-6 måneders oppfølging. For hvert sykehus er det regnet ut slik:", "<br/>", "antall pasienter som svarte 'svært godt fornøyd' eller 'ganske fornøyd' / antall opererte *100", "<br/>"),
                                          "PRE_MAIN_CURVE"~
                                            paste0("Figuren viser fordelingen av andel pasienter som hadde pre-operativ kurve over 70 grader. For hvert sykehus er det regnet ut slik:", "<br/>", "antall pasienter med preoperativ kurve > 70 grader / antall opererte *100", "<br/>"),
                                          "Liggetid" ~
                                            paste0("Figuren viser fordelingen av andel pasienter med post-operativ liggetid på 7 eller flere dager. For hvert sykehus er det regnet ut slik:", "<br/>", "antall pasienter med post-operativ liggetid 7 dager eller mer / antall opererte *100", "<br/>"),
                                          "Komplikasjoner_3mnd" ~
                                            paste0("Figuren viser fordelingen av andel pasienter som rapporterte komplikasjoner (med unntak av smerte) ved 3-6 måneders oppfølging. For hvert sykehus er det regnet ut slik:", "<br/>", "antall pasienter som rapporterer komplikasjoner (med unntak av smerte) ved 3-6 mnds oppfølging / antall opererte *100", "<br/>"),
                                          "CURRENT_SURGERY" ~
                                            paste0("Figuren viser reoperasjonsrate. For hvert sykehus er det regnet ut slik:", "<br/>", "antall reoperasjoner / antall operasjoner *100", "<br/>")),

                      {{kjønn_choice}} == "mann"~
                        dplyr::case_match({{var}},
                                          "SRS22_spm22_3mnd" ~
                                            paste0("Figuren viser fordelingen av andel mannlige pasienter som svarte 'definitivt ja' eller 'sannsynligvis ja' på spørsmål om de ville ønsket samme operasjon på nytt ved 3-6 måneders oppfølging. For hvert sykehus er det regnet ut slik:", "<br/>", "antall mannlige pasienter som svarte 'definitivt ja' eller 'sannsynligvis ja' / antall opererte menn *100", "<br/>"),
                                          "SRS22_spm21_3mnd" ~
                                            paste0("Figuren viser fordelingen av andel mannlige pasienter som svarte 'svært godt fornøyd' eller 'ganske fornøyd' på spørsmål om de er fornøyde med resultatet av operasjonen ved 3-6 måneders oppfølging. For hvert sykehus er det regnet ut slik:", "<br/>", "antall mannlige pasienter som svarte 'svært godt fornøyd' eller 'ganske fornøyd' / antall opererte menn *100", "<br/>"),
                                          "PRE_MAIN_CURVE"~
                                            paste0("Figuren viser fordelingen av andel mannlige pasienter som hadde pre-operativ kurve over 70 grader på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall mannlige pasienter med preoperativ kurve > 70 grader / antall opererte menn *100", "<br/>"),
                                          "Liggetid" ~
                                            paste0("Figuren viser fordelingen av andel mannlige pasienter med post-operativ liggetid på 7 eller flere dager på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall mannlige pasienter med post-operativ liggetid 7 dager eller mer / antall opererte menn *100", "<br/>"),
                                          "Komplikasjoner_3mnd" ~
                                            paste0("Figuren viser fordelingen av andel mannlige pasienter som rapporterte komplikasjoner (med unntak av smerte) ved 3-6 måneders oppfølging. For hvert sykehus er det regnet ut slik:", "<br/>", "antall mannlige pasienter som rapporterer komplikasjoner (med unntak av smerte) ved 3-6 mnds oppfølging / antall opererte menn *100", "<br/>"),
                                          "CURRENT_SURGERY" ~
                                            paste0("Figuren viser reoperasjonsrate for menn på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall reopererte menn / antall opererte menn *100", "<br/>")),

                      {{kjønn_choice}} == "kvinne" ~
                        dplyr::case_match({{var}},
                                          "SRS22_spm22_3mnd" ~
                                            paste0("Figuren viser fordelingen av andel kvinnelige pasienter som svarte 'definitivt ja' eller 'sannsynligvis ja' på spørsmål om de ville ønsket samme operasjon på nytt ved 3-6 måneders oppfølging. For hvert sykehus er det regnet ut slik:", "<br/>", "antall kvinnelige pasienter som svarte 'definitivt ja' eller 'sannsynligvis ja' / antall opererte kvinner * 100", "<br/>"),
                                          "SRS22_spm21_3mnd" ~
                                            paste0("Figuren viser fordelingen av andel kvinnelige pasienter som svarte 'svært godt fornøyd' eller 'ganske fornøyd' på spørsmål om de er fornøyde med resultatet av operasjonen ved 3-6 måneders oppfølging. For hvert sykehus er det regnet ut slik:", "<br/>", "antall kvinnelige pasienter som svarte 'svært godt fornøyd' eller 'ganske fornøyd' / antall opererte kvinner * 100", "<br/>"),
                                          "PRE_MAIN_CURVE"~
                                            paste0("Figuren viser fordelingen av andel kvinnelige pasienter som hadde pre-operativ kurve over 70 grader på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall kvinnelige pasienter med preoperativ kurve > 70 grader / antall opererte kvinner *100", "<br/>"),
                                          "Liggetid" ~
                                            paste0("Figuren viser fordelingen av andel kvinnelige pasienter med post-operativ liggetid på 7 eller flere dager på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall kvinnelige pasienter med post-operativ liggetid 7 dager eller mer / antall opererte kvinner *100", "<br/>"),
                                          "Komplikasjoner_3mnd" ~
                                            paste0("Figuren viser fordelingen av andel kvinnelige pasienter som rapporterte komplikasjoner (med unntak av smerte) ved 3-6 måneders oppfølging. For hvert sykehus er det regnet ut slik:", "<br/>", "antall kvinnelige pasienter som rapporterer komplikasjoner (med unntak av smerte) ved 3-6 mnds oppfølging / antall opererte kvinner *100", "<br/>"),
                                          "CURRENT_SURGERY" ~
                                            paste0("Figuren viser reoperasjonsrate for kvinner på de ulike sykehusene. For hvert sykehus er det regnet ut slik:", "<br/>", "antall reopererte kvinner / antall opererte kvinner *100", "<br/>")),

                      {{kjønn_choice}} == "nei" ~
                        dplyr::case_match({{var}},
                                          "SRS22_spm22_3mnd" ~
                                            paste0("Figuren viser fordelinger av andel pasienter (av begge kjønn),
                                          andel kvinnelige pasienter og andel mannlige pasienter som svarte
                                          'definitivt ja' eller 'sannsynligvis ja' på spørsmål om de ville ønsket samme operasjon på nytt ved 3-6 måneders oppfølging.
                                                 For hvert sykehus er hver andel regnet ut slik:", "<br/>",
                                                   "antall pasienter som svarte 'definitivt ja' og 'sannsynligvis ja' / antall opererte * 100", "<br/>",
                                                   "antall kvinnelige pasienter som svarte 'definitivt ja' og 'sannsynligvis ja' / antall opererte kvinner * 100", "<br/>",
                                                   "antall mannlige pasienter som svarte 'definitivt ja' og 'sannsynligvis ja' / antall opererte menn * 100", "<br/>"),

                                          "SRS22_spm21_3mnd" ~
                                            paste0("Figuren viser fordelinger av andel pasienter (av begge kjønn),
                                          andel kvinnelige pasienter og andel mannlige pasienter som svarte
                                          'svært godt fornøyd' eller 'ganske fornøyd' på spørsmål om de er fornøyde med resultatet av operasjonen ved 3-6 måneders oppfølging.
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
                                          andel kvinnelige pasienter og andel mannlige pasienter som rapporterte komplikasjoner (med unntak av smerte) ved 3-6 måneders oppfølging.
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



