#'
#' @title Simple function
#'
#' @export
#'

prepVar <- function(data, var, var_kjønn, time1, time2, alder1, alder2, type_op){

  # Filter by gender

  data <- data %>%
    dplyr::filter(Kjønn == dplyr::case_when({{var_kjønn}} == "kvinne" ~ "kvinne",
                                            {{var_kjønn}} == "mann" ~ "mann",
                                            {{var_kjønn}} != "kvinne" | {{var_kjønn}} != "mann" ~ Kjønn))

  # Filter by operation type

  data <- data %>%
    dplyr::filter(dplyr::case_when({{type_op}} == "Primæroperasjon" ~ CURRENT_SURGERY == 1,
                                   {{type_op}} == "Reoperasjon" ~ CURRENT_SURGERY == 2,
                                   {{type_op}} == "Begge" ~ CURRENT_SURGERY %in% c(1, 2)))

  # Add filter on surgery date--------------------------------------------------

  data <- data %>%
    dplyr::filter(dplyr::between(SURGERY_DATE,
                                 as.Date({{time1}}),
                                 as.Date({{time2}})))

  # Add filter on age-----------------------------------------------------------

  # Using column "Alder_num" in which alder is given as an integer

  data <- data %>%
    dplyr::filter(dplyr::between(Alder_num,
                                 {{alder1}},
                                 {{alder2}}))


  # data <- data %>%
  #   tidyr::drop_na({{var}})

  gg_data <- data.frame(title = "")


  # Add good titles on each variable--------------------------------------------

  gg_data <- gg_data %>%
    dplyr::mutate(title = dplyr::case_when({{var}} == "BMI_kategori" ~ "Andel operasjoner fordelt på BMI-kategorier",

                                           # ALDER:
                                           {{var}} == "Alder" ~ "Andel operasjoner fordelt på aldersgrupper",

                                           # KURVE:
                                           {{var}} == "PRE_MAIN_CURVE" ~ "Andel operasjoner fordelt på pre-operativ kurve",
                                           {{var}} == "Kurve_pre" ~ "Andel operasjoner fordelt på pre-operativ kurve",
                                           {{var}} == "Kurve_post" ~ "Andel operasjoner fordelt på post-operativ kurve",
                                           {{var}} == "Diff_prosent_kurve" ~ "Andel operasjoner fordelt på prosentvis korreksjon i kurve",

                                           # LIGGETID
                                           {{var}} == "Liggetid" ~ "Andel operasjoner fordelt på liggetid etter operasjon",

                                           # KNIVTID
                                           {{var}} == "Knivtid" ~ "Andel operasjoner fordelt på knivtid",

                                           # BLODTAP:
                                           {{var}} == "Blodtap_100" ~ "Andel operasjoner fordelt på blodtap",
                                           {{var}} == "Blodtap_200" ~ "Andel operasjoner fordelt på blodtap",

                                           # SRS22:total
                                           {{var}} == "SRS22_total" ~ "Andel operasjoner fordelt på total SRS22 skår (1-5) ved innleggelse",

                                           # SRS22: funksjon
                                           {{var}} == "SRS22_funksjon" ~ "Andel operasjoner fordelt på SRS22-funksjonsskår (1-5) ved innleggelse",
                                           {{var}} == "SRS22_funksjon_3mnd" ~ "Andel operasjoner fordelt på SRS22-funksjonsskår (1-5) ved 3-6 måneders oppfølging",
                                           {{var}} == "SRS22_funksjon_12mnd" ~ "Andel operasjoner fordelt på SRS22-funksjonsskår (1-5) ved 12 måneders oppfølging",
                                           {{var}} == "SRS22_funksjon_60mnd" ~ "Andel operasjoner fordelt på SRS22-funksjonsskår (1-5) ved 5 års oppfølging",

                                           # SRS22: smerte
                                           {{var}} == "SRS22_smerte" ~ "Andel operasjoner fordelt på SRS22-smertesskår (1-5) ved innleggelse",
                                           {{var}} == "SRS22_smerte_3mnd" ~ "Andel operasjoner fordelt på SRS22-smertesskår (1-5) ved 3-6 måneders oppfølging",
                                           {{var}} == "SRS22_smerte_12mnd" ~ "Andel operasjoner fordelt på SRS22-smertesskår (1-5) ved 12 måneders oppfølging",
                                           {{var}} == "SRS22_smerte_60mnd" ~ "Andel operasjoner fordelt på SRS22-smertesskår (1-5) ved 5 års oppfølging",

                                           # SRS22: selvbilde
                                           {{var}} == "SRS22_selvbilde" ~ "Andel operasjoner fordelt på SRS22-selvbildesskår (1-5) ved innleggelse",
                                           {{var}} == "SRS22_selvbilde_3mnd" ~ "Andel operasjoner fordelt på SRS22-selvbildesskår (1-5) ved 3-6 måneders oppfølging",
                                           {{var}} == "SRS22_selvbilde_12mnd" ~ "Andel operasjoner fordelt på SRS22-selvbildesskår (1-5) ved 12 måneders oppfølging",
                                           {{var}} == "SRS22_selvbilde_60mnd" ~ "Andel operasjoner fordelt på SRS22-selvbildesskår (1-5) ved 5 års oppfølging",

                                           # SRS22: mental helse
                                           {{var}} == "SRS22_mhelse" ~ "Andel operasjoner fordelt på SRS22-mental-helse-skår (1-5) ved innleggelse",
                                           {{var}} == "SRS22_mhelse_3mnd" ~ "Andel operasjoner fordelt på SRS22-mental-helse-skår (1-5) ved 3-6 måneders oppfølging",
                                           {{var}} == "SRS22_mhelse_12mnd" ~ "Andel operasjoner fordelt på SRS22-mental-helse-skår (1-5) ved 12 måneders oppfølging",
                                           {{var}} == "SRS22_mhelse_60mnd" ~ "Andel operasjoner fordelt på SRS22-mental-helse-skår (1-5) ved 5 års oppfølging",

                                           # SRS22: fornøyd
                                           {{var}} == "SRS22_fornoyd_3mnd" ~ "Andel operasjoner fordelt på SRS22-fornøydhetsskår (1-5) ved 3-6 måneders oppfølging",
                                           {{var}} == "SRS22_fornoyd_12mnd" ~ "Andel operasjoner fordelt på SRS22-fornøydhetsskår (1-5) ved 12 måneders oppfølging",
                                           {{var}} == "SRS22_fornoyd_60mnd" ~ "Andel operasjoner fordelt på SRS22-fornøydhetsskår (1-5) ved 5 års oppfølging",

                                           # SRS22: spm 21 - hvor fornøyd?
                                           {{var}} == "SRS22_spm21_3mnd" ~ "Andel operasjoner fordelt på: 'Er du fornøyd med resultatet av behandlingen?' ved 3-6 måneders oppfølging",
                                           {{var}} == "SRS22_spm21_12mnd" ~ "Andel operasjoner fordelt på: 'Er du fornøyd med resultatet av behandlingen?' ved 12 måneders oppfølging",
                                           {{var}} == "SRS22_spm21_60mnd" ~ "Andel operasjoner fordelt på: 'Er du fornøyd med resultatet av behandlingen?' ved 5 års oppfølging",

                                           # SRS22: spm 22 - på nytt?
                                           {{var}} == "SRS22_spm22_3mnd" ~ "Andel operasjoner fordelt på: 'Ville du ønsket samme behandling på nytt?' ved 3-6 måneders oppfølging",
                                           {{var}} == "SRS22_spm22_12mnd" ~ "Andel operasjoner fordelt på: 'Ville du ønsket samme behandling på nytt?' ved 12 måneders oppfølging",
                                           {{var}} == "SRS22_spm22_60mnd" ~ "Andel operasjoner fordelt på: 'Ville du ønsket samme behandling på nytt?' ved 5 års oppfølging",

                                           # EQ5D

                                           # HELSETILSTAND
                                           {{var}} == "Helsetilstand" ~ "Andel operasjoner fordelt på helsetilstandsskår (0-100) ved innleggelse",
                                           {{var}} == "Helsetilstand_3mnd" ~ "Andel operasjoner fordelt på helsetilstandsskår (1-5) ved 3-6 måneders oppfølging",
                                           {{var}} == "Helsetilstand_12mnd" ~ "Andel operasjoner fordelt på helsetilstandsskår (1-5) ved 12 måneders oppfølging",
                                           {{var}} == "Helsetilstand_60mnd" ~ "Andel operasjoner fordelt på helsetilstandsskår (1-5) ved 5 års oppfølging",

                                           # KOMPLIKASJONER
                                           {{var}} == "Komplikasjoner_3mnd" ~ "Andel komplikasjoner pr. operasjon ved 3-6 måneders oppfølging",

                                           {{var}} == "Andel operasjoner" ~ "Andel operasjoner"
    ),

    # Add title for label in plot (specifically xlab in ggplot)---------------------

    xlab = dplyr::case_when({{var}} == "BMI_kategori" ~ "BMI-kategorier",

                            # ALDER:
                            {{var}} == "Alder" ~ "Aldersgrupper",

                            # KURVE:
                            {{var}} == "PRE_MAIN_CURVE" ~ "Pre-operativ kurve",
                            {{var}} == "Kurve_pre" ~ "Pre-operativ kurve",
                            {{var}} == "Kurve_post" ~ "Post-operativ kurve",
                            {{var}} == "Diff_prosent_kurve" ~ "Post-operativ prosent korreksjon",

                            # LIGGETID
                            {{var}} == "Liggetid" ~ "Liggetid etter operasjon, oppgitt i dager",

                            # KNIVTID
                            {{var}} == "Knivtid" ~ "Knivtid, oppgitt i minutter",

                            # BLODTAP:
                            {{var}} == "Blodtap_100" ~ "Blodtap pr 100ml",
                            {{var}} == "Blodtap_200" ~ "Blodtap pr 200ml",

                            # SRS22:total
                            {{var}} == "SRS22_total" ~ "Total SRS22 skår (1-5) ved innleggelse",

                            # SRS22: funksjon
                            {{var}} == "SRS22_funksjon" ~ "SRS22-funksjonsskår (1-5), innleggelse",
                            {{var}} == "SRS22_funksjon_3mnd" ~ "SRS22-funksjonsskår (1-5), 3-6 måneders oppfølging",
                            {{var}} == "SRS22_funksjon_12mnd" ~ "SRS22-funksjonsskår (1-5), 12 måneders oppfølging",
                            {{var}} == "SRS22_funksjon_60mnd" ~ "SRS22-funksjonsskår (1-5), 5 års oppfølging",

                            # SRS22: smerte
                            {{var}} == "SRS22_smerte" ~ "SRS22-smertesskår (1-5), innleggelse",
                            {{var}} == "SRS22_smerte_3mnd" ~ "SRS22-smertesskår (1-5), 3-6 måneders oppfølging",
                            {{var}} == "SRS22_smerte_12mnd" ~ "SRS22-smertesskår (1-5), 12 måneders oppfølging",
                            {{var}} == "SRS22_smerte_60mnd" ~ "SRS22-smertesskår (1-5), 5 års oppfølging",

                            # SRS22: selvbilde
                            {{var}} == "SRS22_selvbilde" ~ "SRS22-selvbildesskår (1-5), innleggelse",
                            {{var}} == "SRS22_selvbilde_3mnd" ~ "SRS22-selvbildesskår (1-5), 3-6 måneders oppfølging",
                            {{var}} == "SRS22_selvbilde_12mnd" ~ "SRS22-selvbildesskår (1-5), 12 måneders oppfølging",
                            {{var}} == "SRS22_selvbilde_60mnd" ~ "SRS22-selvbildesskår (1-5), 5 års oppfølging",

                            # SRS22: mental helse
                            {{var}} == "SRS22_mhelse" ~ "SRS22-mental-helse-skår (1-5), innleggelse",
                            {{var}} == "SRS22_mhelse_3mnd" ~ "SRS22-mental-helse-skår (1-5), 3-6 måneders oppfølging",
                            {{var}} == "SRS22_mhelse_12mnd" ~ "SRS22-mental-helse-skår (1-5), 12 måneders oppfølging",
                            {{var}} == "SRS22_mhelse_60mnd" ~ "SRS22-mental-helse-skår (1-5), 5 års oppfølging",

                            # SRS22: fornøyd
                            {{var}} == "SRS22_fornoyd_3mnd" ~ "SRS22-fornøydhetsskår (1-5), 3-6 måneders oppfølging",
                            {{var}} == "SRS22_fornoyd_12mnd" ~ "SRS22-fornøydhetsskår (1-5), 12 måneders oppfølging",
                            {{var}} == "SRS22_fornoyd_60mnd" ~ "SRS22-fornøydhetsskår (1-5), 5 års oppfølging",

                            # SRS22: spm 21 - hvor fornøyd?
                            {{var}} == "SRS22_spm21_3mnd" ~ "'Er du fornøyd med resultatet av behandlingen?', 3-6 måneders oppfølging",
                            {{var}} == "SRS22_spm21_12mnd" ~ "'Er du fornøyd med resultatet av behandlingen?', 12 måneders oppfølging",
                            {{var}} == "SRS22_spm21_60mnd" ~ "'Er du fornøyd med resultatet av behandlingen?', 5 år oppfølging",

                            # SRS22: spm 22 - på nytt?
                            {{var}} == "SRS22_spm22_3mnd" ~ "'Ville du ønsket samme behandling på nytt?', 3-6 måneders oppfølging",
                            {{var}} == "SRS22_spm22_12mnd" ~ "'Ville du ønsket samme behandling på nytt?', 12 måneders oppfølging",
                            {{var}} == "SRS22_spm22_60mnd" ~ "'Ville du ønsket samme behandling på nytt?', 5 års oppfølging",

                            # EQ5D

                            # HELSETILSTAND
                            {{var}} == "Helsetilstand" ~ "Helsetilstandsskår (0-100), innleggelse",
                            {{var}} == "Helsetilstand_3mnd" ~ "Helsetilstandsskår (1-5), 3-6 måneders oppfølging",
                            {{var}} == "Helsetilstand_12mnd" ~ "Helsetilstandsskår (1-5), 12 måneders oppfølging",
                            {{var}} == "Helsetilstand_60mnd" ~ "Helsetilstandsskår (1-5), 5 års oppfølging",

                            # KOMPLIKASJONER
                            {{var}} == "Komplikasjoner_3mnd" ~ "Selvrapportert komplikasjon, 3-6 måneders oppfølging",

                            {{var}} == "Andel operasjoner" ~ "Andel operasjoner"

    ))


  #### Select and return the column of interest-----------------------------------


my_data <- data %>%
    dplyr::select(c("Sykehus",
                    "CENTREID",
                    {{var}},
                    "Kjønn",
                    "CURRENT_SURGERY"))


  return(list(my_data, gg_data)) # returns a list (the list is unpacked in UI)

}

# Test of the function
## x <- prepVar(regdata, "SRS22_spm22_3mnd", "mm", "2023-01-02", "2024-10-02", 1, 20, "Primæroperasjon")
# Inspect returned data frame (object 1 in list):
## f <- data.frame(x[1])
## gg_data <- data.frame(x[2])
