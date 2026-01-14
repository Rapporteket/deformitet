
#' @title prepvar-funksjon
#'
#' @export
#'

prepVar <- function(data, var, var_kjonn,
                    time1, time2,
                    alder1, alder2,
                    type_op,
                    visning = "uten_tid"){


  data <- prep_var_na(data, var)


  # Filter by gender

  data <- data |>
    dplyr::filter(.data$Kjonn == dplyr::case_when({{var_kjonn}} == "kvinne" ~ "kvinne",
                                            {{var_kjonn}} == "mann" ~ "mann",
                                            {{var_kjonn}} != "kvinne" | {{var_kjonn}} != "mann" ~ Kjonn))

  # Filter by operation type

  data <- data |>
    dplyr::filter(dplyr::case_when({{type_op}} == "Primæroperasjon" ~ CURRENT_SURGERY == 1,
                                   {{type_op}} == "Reoperasjon" ~ CURRENT_SURGERY == 2,
                                   {{type_op}} == "Begge" ~ CURRENT_SURGERY %in% c(1, 2)))

  # Add filter on surgery date--------------------------------------------------

  data <- data |>
    dplyr::filter(dplyr::between(.data$SURGERY_DATE,
                                 as.Date({{time1}}),
                                 as.Date({{time2}})))

  # Add filter on age-----------------------------------------------------------

  # Using column "Alder_num" in which alder is given as an integer

  data <- data |>
    dplyr::filter(dplyr::between(.data$Alder_num,
                                 {{alder1}},
                                 {{alder2}}))


  # data <- data |>
  #   tidyr::drop_na({{var}})

  gg_data <- data.frame(title = "")


  # Add good titles on each variable--------------------------------------------

  gg_data <- gg_data |>
    dplyr::mutate(title = dplyr::case_when({{var}} %in% c("BMI_kategori", "BMI") ~
                                             "Andel operasjoner fordelt på BMI-kategorier",

                                           # ALDER:
                                           {{var}} %in% c("Alder", "Alder_num") ~ "Andel operasjoner fordelt på aldersgrupper",

                                           # KURVE:
                                           {{var}} %in% c("Kurve_pre", "PRE_MAIN_CURVE") ~
                                             "Andel operasjoner fordelt på pre-operativ kurve",
                                           {{var}} %in% c("Kurve_post", "POST_MAIN_CURVE") ~
                                             "Andel operasjoner fordelt på post-operativ kurve",
                                           {{var}} %in% c("Diff_prosent_kurve", "Diff_prosent_kurve_raw") ~
                                             "Andel operasjoner fordelt på prosentvis korreksjon i kurve",


                                           # LIGGETID
                                           {{var}} %in% c("Liggetid", "BED_DAYS_POSTOPERATIVE") ~
                                             "Andel operasjoner fordelt på liggetid etter operasjon",

                                           # KNIVTID
                                           {{var}} %in% c("Knivtid", "kniv_tid") ~ "Andel operasjoner fordelt på knivtid",

                                           # BLODTAP:
                                           {{var}} %in% c("Blodtap_100", "Blodtap_200", "PER_BLOOD_LOSS_VALUE") ~
                                                            "Andel operasjoner fordelt på blodtap",

                                           # SRS22:total
                                           {{var}} %in% c("SRS22_total", "SRS22_MAIN_SCORE") ~
                                             "Andel operasjoner fordelt på total SRS22 skår (1-5) preoperativt",
                                           {{var}} %in% c("SRS22_total_3mnd", "SRS22_FULL_SCORE") ~
                                             "Andel operasjoner fordelt på total SRS22 skår (1-5) ved 3-6 måneders oppfølging",
                                           {{var}} %in% c("SRS22_total_12mnd", "SRS22_FULL_SCORE_patient12mths") ~
                                             "Andel operasjoner fordelt på total SRS22 skår (1-5) ved 12 måneders oppfølging",
                                           {{var}} %in% c("SRS22_total_60mnd", "SRS22_FULL_SCORE_patient60mths") ~
                                             "Andel operasjoner fordelt på total SRS22 skår (1-5) ved 5 års oppfølging",

                                           # SRS22: funksjon
                                           {{var}} %in% c("SRS22_funksjon","SRS22_FUNCTION_SCORE") ~
                                             "Andel operasjoner fordelt på SRS22-funksjonsskår (1-5) preoperativt",
                                           {{var}} %in% c("SRS22_funksjon_3mnd", "SRS22_FUNCTION_SCORE_patient3mths") ~
                                             "Andel operasjoner fordelt på SRS22-funksjonsskår (1-5) ved 3-6 måneders oppfølging",
                                           {{var}} %in% c("SRS22_funksjon_12mnd", "SRS22_FUNCTION_SCORE_patient12mths") ~
                                             "Andel operasjoner fordelt på SRS22-funksjonsskår (1-5) ved 12 måneders oppfølging",
                                           {{var}} %in% c("SRS22_funksjon_60mnd", "SRS22_FUNCTION_SCORE_patient60mths") ~
                                             "Andel operasjoner fordelt på SRS22-funksjonsskår (1-5) ved 5 års oppfølging",

                                           # SRS22: smerte
                                           {{var}} %in% c("SRS22_smerte",
                                                          "SRS22_PAIN_SCORE") ~
                                             "Andel operasjoner fordelt på SRS22-smertesskår (1-5) preoperativt",
                                           {{var}} %in% c("SRS22_smerte_3mnd",
                                                          "SRS22_PAIN_SCORE_patient3mths") ~
                                             "Andel operasjoner fordelt på SRS22-smertesskår (1-5) ved 3-6 måneders oppfølging",
                                           {{var}} %in% c("SRS22_smerte_12mnd",
                                                          "SRS22_PAIN_SCORE_patient12mths") ~
                                             "Andel operasjoner fordelt på SRS22-smertesskår (1-5) ved 12 måneders oppfølging",
                                           {{var}} %in% c("SRS22_smerte_60mnd",
                                                          "SRS22_PAIN_SCORE_patient60mths") ~
                                             "Andel operasjoner fordelt på SRS22-smertesskår (1-5) ved 5 års oppfølging",


                                           # SRS22: selvbilde
                                           {{var}} %in% c("SRS22_selvbilde",
                                                          "SRS22_SELFIMAGE_SCORE") ~
                                             "Andel operasjoner fordelt på SRS22-selvbildesskår (1-5) preoperativt",
                                           {{var}} %in% c("SRS22_selvbilde_3mnd",
                                                          "SRS22_SELFIMAGE_SCORE_patient3mths") ~
                                             "Andel operasjoner fordelt på SRS22-selvbildesskår (1-5) ved 3-6 måneders oppfølging",
                                           {{var}} %in% c("SRS22_selvbilde_12mnd",
                                                          "SRS22_SELFIMAGE_SCORE_patient12mths") ~
                                             "Andel operasjoner fordelt på SRS22-selvbildesskår (1-5) ved 12 måneders oppfølging",
                                           {{var}} %in% c("SRS22_selvbilde_60mnd",
                                                          "SRS22_SELFIMAGE_SCORE_patient60mths") ~
                                             "Andel operasjoner fordelt på SRS22-selvbildesskår (1-5) ved 5 års oppfølging",

                                           # SRS22: mental helse
                                           {{var}} %in% c("SRS22_mhelse",
                                                          "SRS22_MENTALHEALTH_SCORE") ~
                                             "Andel operasjoner fordelt på SRS22-mental-helse-skår (1-5) preoperativt",
                                           {{var}} %in% c("SRS22_mhelse_3mnd",
                                                          "SRS22_MENTALHEALTH_SCORE_patient3mths") ~
                                             "Andel operasjoner fordelt på SRS22-mental-helse-skår (1-5) ved 3-6 måneders oppfølging",
                                           {{var}} %in% c("SRS22_mhelse_12mnd",
                                                          "SRS22_MENTALHEALTH_SCORE_patient12mths") ~
                                             "Andel operasjoner fordelt på SRS22-mental-helse-skår (1-5) ved 12 måneders oppfølging",
                                           {{var}} %in% c("SRS22_mhelse_60mnd",
                                                          "SRS22_MENTALHEALTH_SCORE_patient60mths") ~
                                             "Andel operasjoner fordelt på SRS22-mental-helse-skår (1-5) ved 5 års oppfølging",

                                           # SRS22: fornøyd
                                           {{var}} %in% c("SRS22_fornoyd_3mnd",
                                                          "SRS22_SATISFACTION_SCORE") ~
                                             "Andel operasjoner fordelt på SRS22-fornøydhetsskår (1-5) ved 3-6 måneders oppfølging",
                                           {{var}} %in% c("SRS22_fornoyd_12mnd",
                                                          "SRS22_SATISFACTION_SCORE_patient12mths") ~
                                             "Andel operasjoner fordelt på SRS22-fornøydhetsskår (1-5) ved 12 måneders oppfølging",
                                           {{var}} %in% c("SRS22_fornoyd_60mnd",
                                                          "SRS22_SATISFACTION_SCORE_patient60mths") ~
                                             "Andel operasjoner fordelt på SRS22-fornøydhetsskår (1-5) ved 5 års oppfølging",

                                           # SRS22: spm 21 - hvor fornøyd?
                                           {{var}} %in% c("SRS22_spm21_3mnd",
                                                          "SRS22_21") ~ "Andel operasjoner fordelt på: 'Er du fornøyd med resultatet av behandlingen?' ved 3-6 måneders oppfølging",
                                           {{var}} %in% c("SRS22_spm21_12mnd",
                                                          "SRS22_21_patient12mths") ~ "Andel operasjoner fordelt på: 'Er du fornøyd med resultatet av behandlingen?' ved 12 måneders oppfølging",
                                           {{var}} %in% c("SRS22_spm21_60mnd",
                                                          "SRS22_21_patient60mths") ~ "Andel operasjoner fordelt på: 'Er du fornøyd med resultatet av behandlingen?' ved 5 års oppfølging",

                                           # SRS22: spm 22 - på nytt?
                                           {{var}} %in% c("SRS22_spm22_3mnd",
                                                          "SRS22_22") ~
                                             "Andel operasjoner fordelt på: 'Ville du ønsket samme behandling på nytt?' ved 3-6 måneders oppfølging",
                                           {{var}} %in% c("SRS22_spm22_12mnd",
                                                          "SRS22_22_patient12mths") ~
                                             "Andel operasjoner fordelt på: 'Ville du ønsket samme behandling på nytt?' ved 12 måneders oppfølging",
                                           {{var}} %in% c("SRS22_spm22_60mnd",
                                                          "SRS22_22_patient60mths") ~
                                             "Andel operasjoner fordelt på: 'Ville du ønsket samme behandling på nytt?' ved 5 års oppfølging",

                                           # EQ5D

                                           # HELSETILSTAND
                                           {{var}} %in% c("Helsetilstand",
                                                          "HELSETILSTAND_SCALE") ~
                                             "Andel operasjoner fordelt på helsetilstandsskår (0-100) preoperativt",
                                           {{var}} %in% c("Helsetilstand_3mnd",
                                                           "HEALTH_CONDITION_SCALE") ~
                                             "Andel operasjoner fordelt på helsetilstandsskår (1-5) ved 3-6 måneders oppfølging",
                                           {{var}} %in% c("Helsetilstand_12mnd",
                                                          "HEALTH_CONDITION_SCALE_patient12mths") ~
                                             "Andel operasjoner fordelt på helsetilstandsskår (1-5) ved 12 måneders oppfølging",
                                           {{var}} %in% c("Helsetilstand_60mnd",
                                                          "HEALTH_CONDITION_SCALE_patient_60_mths") ~
                                             "Andel operasjoner fordelt på helsetilstandsskår (1-5) ved 5 års oppfølging",


                                           # KOMPLIKASJONER
                                           {{var}} == "Komplikasjoner_3mnd" ~ "Andel komplikasjoner pr. operasjon(3-6 mnd)",
                                           {{var}} == "Komplikasjoner_12mnd" ~ "Andel komplikasjoner pr. operasjon (12 mnd)",
                                           {{var}} == "Komplikasjoner_60mnd" ~ "Andel komplikasjoner pr. operasjon (5 år)",


                                           {{var}} == "Andel operasjoner" ~ "Andel operasjoner"
    ),

    # Add title for label in plot (specifically xlab in ggplot)---------------------

    xlab = dplyr::case_when({{var}} == "BMI_kategori" ~ "BMI-kategorier",
                            {{var}} == "BMI" ~ "BMI",

                            # ALDER:
                            {{var}} == "Alder" ~ "Aldersgrupper",
                            {{var}} == "Alder_num" ~ "Alder",

                            # KURVE:
                            {{var}} %in% c("Kurve_pre", "PRE_MAIN_CURVE") ~
                              "Pre-operativ kurve",
                            {{var}} %in% c("Kurve_post", "POST_MAIN_CURVE") ~
                              "Post-operativ kurve",
                            {{var}} %in% c("Diff_prosent_kurve", "Diff_prosent_kurve_raw") ~
                              "Post-operativ prosent korreksjon",

                            # LIGGETID
                            {{var}} %in% c("Liggetid", "BED_DAYS_POSTOPERATIVE") ~
                              "Liggetid etter operasjon, oppgitt i dager",

                            # KNIVTID
                            {{var}} %in% c("Knivtid", "kniv_tid") ~ "Knivtid, oppgitt i minutter",

                            # BLODTAP:
                            {{var}} == "Blodtap_100" ~ "Blodtap pr 100ml",
                            {{var}} == "Blodtap_200" ~ "Blodtap pr 200ml",
                            {{var}} == "PER_BLOOD_LOSS_VALUE" ~ "Blodtap i ml",

                            # SRS22:total
                            {{var}} %in% c("SRS22_total", "SRS22_MAIN_SCORE") ~
                              "Total SRS22 skår (1-5) preoperativt",
                            {{var}} %in% c("SRS22_total_3mnd", "SRS22_FULL_SCORE") ~
                              "Total SRS22 skår (1-5) ved 3-6 måneders oppfølging",
                            {{var}} %in% c("SRS22_total_12mnd", "SRS22_FULL_SCORE_patient12mths") ~
                              "Total SRS22 skår (1-5) ved 12 måneders oppfølging",
                            {{var}} %in% c("SRS22_total_60mnd", "SRS22_FULL_SCORE_patient60mths") ~
                              "Total SRS22 skår (1-5) ved 5 års oppfølging",

                            # SRS22: funksjon
                            {{var}} %in% c("SRS22_funksjon","SRS22_FUNCTION_SCORE") ~
                              "SRS22-funksjonsskår (1-5) preoperativt",
                            {{var}} %in% c("SRS22_funksjon_3mnd", "SRS22_FUNCTION_SCORE_patient3mths") ~
                              "SRS22-funksjonsskår (1-5), 3-6 måneders oppfølging",
                            {{var}} %in% c("SRS22_funksjon_12mnd", "SRS22_FUNCTION_SCORE_patient12mths") ~
                              "SRS22-funksjonsskår (1-5), 12 måneders oppfølging",
                            {{var}} %in% c("SRS22_funksjon_60mnd", "SRS22_FUNCTION_SCORE_patient60mths") ~
                              "SRS22-funksjonsskår (1-5), 5 års oppfølging",

                            # SRS22: smerte
                            {{var}} %in% c("SRS22_smerte", "SRS22_PAIN_SCORE") ~
                              "SRS22-smertesskår (1-5) preoperativt",
                            {{var}} %in% c("SRS22_smerte_3mnd", "SRS22_PAIN_SCORE_patient3mths") ~
                              "SRS22-smertesskår (1-5), 3-6 måneders oppfølging",
                            {{var}} %in% c("SRS22_smerte_12mnd", "SRS22_PAIN_SCORE_patient12mths") ~
                              "SRS22-smertesskår (1-5), 12 måneders oppfølging",
                            {{var}} %in% c("SRS22_smerte_60mnd", "SRS22_PAIN_SCORE_patient60mths") ~
                              "SRS22-smertesskår (1-5), 5 års oppfølging",

                            # SRS22: selvbilde
                            {{var}} %in% c("SRS22_selvbilde",
                                           "SRS22_SELFIMAGE_SCORE") ~
                              "SRS22-selvbildesskår (1-5) preoperativt",
                            {{var}} %in% c("SRS22_selvbilde_3mnd",
                                           "SRS22_SELFIMAGE_SCORE_patient3mths") ~
                              "SRS22-selvbildesskår (1-5), 3-6 måneders oppfølging",
                            {{var}} %in% c("SRS22_selvbilde_12mnd",
                                           "SRS22_SELFIMAGE_SCORE_patient12mths") ~
                              "SRS22-selvbildesskår (1-5), 12 måneders oppfølging",
                            {{var}} %in% c("SRS22_selvbilde_60mnd",
                                           "SRS22_SELFIMAGE_SCORE_patient60mths") ~
                              "SRS22-selvbildesskår (1-5), 5 års oppfølging",

                            # SRS22: mental helse
                            {{var}} %in% c("SRS22_mhelse",
                                           "SRS22_MENTALHEALTH_SCORE") ~
                              "SRS22-mental-helse-skår (1-5) preoperativt",
                            {{var}} %in% c("SRS22_mhelse_3mnd",
                                           "SRS22_MENTALHEALTH_SCORE_patient3mths") ~
                              "SRS22-mental-helse-skår (1-5), 3-6 måneders oppfølging",
                            {{var}} %in% c("SRS22_mhelse_12mnd",
                                           "SRS22_MENTALHEALTH_SCORE_patient12mths") ~
                              "SRS22-mental-helse-skår (1-5), 12 måneders oppfølging",
                            {{var}} %in% c("SRS22_mhelse_60mnd",
                                           "SRS22_MENTALHEALTH_SCORE_patient60mths") ~
                              "SRS22-mental-helse-skår (1-5), 5 års oppfølging",

                            # SRS22: fornøyd
                            {{var}} %in% c("SRS22_fornoyd_3mnd",
                                           "SRS22_SATISFACTION_SCORE") ~
                              "SRS22-fornøydhetsskår (1-5), 3-6 måneders oppfølging",
                            {{var}} %in% c("SRS22_fornoyd_12mnd",
                                           "SRS22_SATISFACTION_SCORE_patient12mths") ~
                              "SRS22-fornøydhetsskår (1-5), 12 måneders oppfølging",
                            {{var}} %in% c("SRS22_fornoyd_60mnd",
                                           "SRS22_SATISFACTION_SCORE_patient60mths") ~
                              "SRS22-fornøydhetsskår (1-5), 5 års oppfølging",


                            # SRS22: spm 21 - hvor fornøyd?
                            {{var}} %in% c("SRS22_spm21_3mnd",
                                           "SRS22_21") ~
                              "'Er du fornøyd med resultatet av behandlingen?', 3-6 måneders oppfølging",
                            {{var}} %in% c("SRS22_spm21_12mnd",
                                           "SRS22_21_patient12mths") ~
                              "'Er du fornøyd med resultatet av behandlingen?', 12 måneders oppfølging",
                            {{var}} %in% c("SRS22_spm21_60mnd",
                                           "SRS22_21_patient60mths") ~
                              "'Er du fornøyd med resultatet av behandlingen?', 5 år oppfølging",

                            # SRS22: spm 22 - på nytt?
                            {{var}} %in% c("SRS22_spm22_3mnd",
                                           "SRS22_22") ~
                              "'Ville du ønsket samme behandling på nytt?', 3-6 måneders oppfølging",
                            {{var}} %in% c("SRS22_spm22_12mnd",
                                           "SRS22_22_patient12mths") ~
                              "'Ville du ønsket samme behandling på nytt?', 12 måneders oppfølging",
                            {{var}} %in% c("SRS22_spm22_60mnd",
                                           "SRS22_22_patient60mths") ~
                              "'Ville du ønsket samme behandling på nytt?', 5 års oppfølging",


                            # EQ5D

                            # HELSETILSTAND
                            {{var}} %in% c("Helsetilstand",
                                           "HELSETILSTAND_SCALE") ~
                              "Helsetilstandsskår (0-100) preoperativt",
                            {{var}} %in% c("Helsetilstand_3mnd",
                                           "HEALTH_CONDITION_SCALE") ~
                              "Helsetilstandsskår (0-100), 3-6 måneders oppfølging",
                            {{var}} %in% c("Helsetilstand_12mnd",
                                           "HEALTH_CONDITION_SCALE_patient12mths") ~
                              "Helsetilstandsskår (0-100), 12 måneders oppfølging",
                            {{var}} %in% c("Helsetilstand_60mnd",
                                           "HEALTH_CONDITION_SCALE_patient_60_mths") ~
                              "Helsetilstandsskår (0-100), 5 års oppfølging",

                            # KOMPLIKASJONER
                            {{var}} == "Komplikasjoner_3mnd" ~ "Selvrapportert komplikasjon, 3-6 måneders oppfølging",
                            {{var}} == "Komplikasjoner_12mnd" ~ "Selvrapportert komplikasjon, 12 måneders oppfølging",
                            {{var}} == "Komplikasjoner_60mnd" ~ "Selvrapportert komplikasjon, 5 års oppfølging",

                            {{var}} == "Andel operasjoner" ~ "Andel operasjoner"

    ))


  #### Select and return the column of interest---------------------------------
  if (visning == "over_tid") {
    my_data <- data |>
      dplyr::select(c("Sykehus",
                      "CENTREID",
                      all_of({{var}}),
                      "Kjonn",
                      "CURRENT_SURGERY",
                      all_of(case_when({{var}} %in% skjema$tre_mnd_pas ~ "FILLING_DATE_patient3mths",
                                       {{var}} %in% skjema$tre_mnd_lege ~ "FILLING_DATE_surgeon3mths",
                                       {{var}} %in% skjema$tolv_mnd_pas ~ "FILLING_DATE_patient12mths",
                                       {{var}} %in% skjema$tolv_mnd_lege ~ "FILLING_DATE_surgeon12mths",
                                       {{var}} %in% skjema$seksti_mnd_pas ~ "FILLING_DATE_patient12mths",
                                       .default = "SURGERY_DATE")),
                      PID))
    } else {
      my_data <- data |>
        dplyr::select(c("Sykehus",
                      "CENTREID",
                      all_of({{var}}),
                      "Kjonn",
                      "CURRENT_SURGERY"))
    }


  return(list(my_data, gg_data)) # returns a list (the list is unpacked in UI)

}


# Test of the function
##x <- prepVar(RegData, "Kurve_pre", "mm", "2023-01-02", "2024-10-02", 1, 20, "Primæroperasjon", "over_tid")
# Inspect returned data frame (object 1 in list):
##rr <- data.frame(x[1])
## gg_data <- data.frame(x[2])

# Funksjon for å ta ut NA der det ikke er registrert oppfølging enda

#' @title Ta bort na
#'
#' @export

prep_var_na <- function (data, var) {

  data <- data

  oppflg <- data.frame(
    "tre" = c("Helsetilstand_3mnd",
              "HEALTH_CONDITION_SCALE",
              "SRS22_spm22_3mnd",
              "SRS22_22",
              "SRS22_spm21_3mnd",
              "SRS22_21",
              "SRS22_total_3mnd",
              "SRS22_FULL_SCORE",
              "SRS22_funksjon_3mnd",
              "SRS22_FUNCTION_SCORE_patient3mths",
              "SRS22_smerte_3mnd",
              "SRS22_PAIN_SCORE_patient3mths",
              "SRS22_selvbilde_3mnd",
              "SRS22_SELFIMAGE_SCORE_patient3mths",
              "SRS22_mhelse_3mnd",
              "SRS22_MENTALHEALTH_SCORE_patient3mths",
              "SRS22_fornoyd_3mnd",
              "SRS22_SATISFACTION_SCORE",
               "Komplikasjoner_3mnd"),

    "tolv" = c("Helsetilstand_12mnd",
               "HEALTH_CONDITION_SCALE_patient12mths",
               "SRS22_spm22_12mnd",
               "SRS22_22_patient12mths",
               "SRS22_spm21_12mnd",
               "SRS22_21_patient12mths",
               "SRS22_total_12mnd",
               "SRS22_FULL_SCORE_patient12mths",
               "SRS22_funksjon_12mnd",
               "SRS22_FUNCTION_SCORE_patient12mths",
               "SRS22_smerte_12mnd",
               "SRS22_PAIN_SCORE_patient12mths",
               "SRS22_selvbilde_12mnd",
               "SRS22_SELFIMAGE_SCORE_patient12mths",
               "SRS22_mhelse_12mnd",
               "SRS22_MENTALHEALTH_SCORE_patient12mths",
               "SRS22_fornoyd_12mnd",
               "SRS22_SATISFACTION_SCORE_patient12mths",
               "Komplikasjoner_12mnd"),

    "seksti" = c("Helsetilstand_60mnd",
                 "HEALTH_CONDITION_SCALE_patient_60_mths",
                 "SRS22_spm22_60mnd",
                 "SRS22_22_patient60mths",
                 "SRS22_spm21_60mnd",
                 "SRS22_21_patient60mths",
                 "SRS22_total_60mnd",
                 "SRS22_FULL_SCORE_patient60mths",
                 "SRS22_funksjon_60mnd",
                 "SRS22_FUNCTION_SCORE_patient60mths",
                 "SRS22_smerte_60mnd",
                 "SRS22_PAIN_SCORE_patient60mths",
                 "SRS22_selvbilde_60mnd",
                 "SRS22_SELFIMAGE_SCORE_patient60mths",
                 "SRS22_mhelse_60mnd",
                 "SRS22_MENTALHEALTH_SCORE_patient60mths",
                 "SRS22_fornoyd_60mnd",
                 "SRS22_SATISFACTION_SCORE_patient60mths",
                 "Komplikasjoner_60mnd"))

 if (var %in% oppflg$tre) {
   data <- data |>
     dplyr::filter(.data$FOLLOWUP == 3)
 }

  if (var %in% oppflg$tolv) {
    data <- data |>
      dplyr::filter(.data$FOLLOWUP_patient12mths == 12)
  }

  if (var %in% oppflg$seksti) {
    data <- data |>
      dplyr::filter(.data$FOLLOWUP_patient60mths == 60)
  }

  return (data)
}

# nolint start
# sjekk at det fungerer:
## r <- prep_var_na(RegData, "Helsetilstand_3mnd")
# nolint end



