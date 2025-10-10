# There are two functions in this file - lagTabell and laggjenTabell

################################################################################
################################ LAG  TABLE ####################################
################################################################################


#' @title lagTabell
#'
#' @param data data som har vært gjennom prepVar()
#' @param var_reshID reshID (valg kun tilgj. som SC-bruker)
#' @param visning valg av visning
#'
#' @examples
#' tabell <- lagTabell(data, 103240, "egen enhet")
#'
#' @export


lagTabell <- function(data, var_reshID, visning){

  if(visning != "hver enhet"){
    data_sykeh <- data %>%
      dplyr::select(-c(CURRENT_SURGERY)) %>%
      dplyr::filter(CENTREID == {{var_reshID}})

    data_sykeh <- data_sykeh %>%
      dplyr::select(-c(CENTREID, Kjønn)) %>%
      dplyr::add_tally(name = "n") %>%
      dplyr::group_by(data_sykeh[3]) %>%
      dplyr::add_count(name = "by_var") %>%
      dplyr::mutate(Prosent = round(by_var/n*100, 2)) %>%
      dplyr::rename("n pr variabel" = by_var) %>%
      dplyr::distinct()

    data_sykeh <- data_sykeh %>%
      dplyr::relocate(Prosent, .before = n)

  }

  if(visning == "hver enhet"){
    data_sykeh_alle <- data

    data_sykeh_alle <- data_sykeh_alle %>%
      dplyr::select(-c(CENTREID, Kjønn, CURRENT_SURGERY)) %>%
      dplyr::group_by(Sykehus) %>%
      dplyr::add_tally(name = "n") %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Sykehus, data_sykeh_alle[3]) %>%
      dplyr::add_count(name = "by_var") %>%
      dplyr::mutate(Prosent = round(by_var/n*100, 2)) %>%
      dplyr::rename("n pr variabel" = by_var) %>%
      dplyr::distinct()

    data_sykeh_alle <- data_sykeh_alle %>%
      dplyr::relocate(Prosent, .before = n)

  }

  else{
    data_alle <- data

    data_alle <- data_alle %>%
    dplyr::select(-c(CENTREID, Kjønn, CURRENT_SURGERY)) %>%
    dplyr::mutate(Sykehus = recode(Sykehus,
                                   "Haukeland" = "Alle",
                                   "Rikshospitalet" = "Alle",
                                   "St.Olav" = "Alle")) %>%
    dplyr::add_tally(name = "n") %>%
    dplyr::group_by(data_alle[3]) %>%
    dplyr::add_count(name = "by_var") %>%
    dplyr::mutate(Prosent = round(by_var/n*100, 2)) %>%
    dplyr::rename("n pr variabel" = by_var) %>%
    dplyr::distinct()


  data_komplett <- dplyr::full_join(data_sykeh, data_alle)

  data_komplett <- data_komplett %>%
    dplyr::relocate(Prosent, .before = n)

  }


  if(visning == "hver enhet"){
    return(data_sykeh_alle)
  }

  if(visning == "egen enhet"){
    return(data_sykeh)
  }
  if(visning == "hele landet, uten sammenligning"){
    data_alle <- data_alle %>%
      dplyr::relocate(Prosent, .before = n)

    return(data_alle)
  } else {
    return(data_komplett)} # => hele landet med sammenligning
}


################################################################################
############################## LAG GJEN TABELL #################################
################################################################################

#' @title mapping_navn
#' @param data rådata (data som ikke har vært gjennom prepros)
#' @param x_var valgt variabel i UI-delen
#'
#' @examples
#' map <- mapping_navn(raw_regdata, "BMI_kategori")
#'
#' @export

mapping_navn <- function (data, x_var) {


  ny_data <- data.frame(gammelt_navn =
                           c("Helsetilstand",
                             "Helsetilstand_3mnd",
                             "Helsetilstand_12mnd",
                             "Helsetilstand_60mnd",
                             "SRS22_spm22_3mnd",
                             "SRS22_spm22_12mnd",
                             "SRS22_spm22_60mnd",
                             "SRS22_spm21_3mnd",
                             "SRS22_spm21_12mnd",
                             "SRS22_spm21_60mnd",
                             "Alder",
                             "BMI_kategori",
                             "Kurve_pre",
                             "Kurve_post",
                             "Diff_prosent_kurve",
                             "Knivtid",
                             "Liggetid",
                             "Blodtap_100",
                             "Blodtap_200",
                             "SRS22_total",
                             "SRS22_total_3mnd",
                             "SRS22_total_12mnd",
                             "SRS22_total_60mnd",
                             "SRS22_funksjon",
                             "SRS22_funksjon_3mnd",
                             "SRS22_funksjon_12mnd",
                             "SRS22_funksjon_60mnd",
                             "SRS22_smerte",
                             "SRS22_smerte_3mnd",
                             "SRS22_smerte_12mnd",
                             "SRS22_smerte_60mnd",
                             "SRS22_selvbilde",
                             "SRS22_selvbilde_3mnd",
                             "SRS22_selvbilde_12mnd",
                             "SRS22_selvbilde_60mnd",
                             "SRS22_mhelse",
                             "SRS22_mhelse_3mnd",
                             "SRS22_mhelse_12mnd",
                             "SRS22_mhelse_60mnd",
                             "SRS22_fornoyd_3mnd",
                             "SRS22_fornoyd_12mnd",
                             "SRS22_fornoyd_60mnd"),


                         nytt_navn = c("HELSETILSTAND_SCALE",
                                      "HEALTH_CONDITION_SCALE",
                                      "HEALTH_CONDITION_SCALE_patient12mths",
                                      "HEALTH_CONDITION_SCALE_patient_60_mths",
                                      "SRS22_22",
                                      "SRS22_22_patient12mths",
                                      "SRS22_22_patient60mths",
                                      "SRS22_21",
                                      "SRS22_21_patient12mths",
                                      "SRS22_21_patient60mths",
                                      "Alder_num",
                                      "BMI",
                                      "PRE_MAIN_CURVE",
                                      "POST_MAIN_CURVE",
                                      "Diff_prosent_kurve_raw",
                                      "kniv_tid",
                                      "BED_DAYS_POSTOPERATIVE",
                                      "PER_BLOOD_LOSS_VALUE",
                                      "PER_BLOOD_LOSS_VALUE",
                                      "SRS22_MAIN_SCORE",
                                      "SRS22_FULL_SCORE",
                                      "SRS22_FULL_SCORE_patient12mths",
                                      "SRS22_FULL_SCORE_patient60mths",
                                      "SRS22_FUNCTION_SCORE",
                                      "SRS22_FUNCTION_SCORE_patient3mths",
                                      "SRS22_FUNCTION_SCORE_patient12mths",
                                      "SRS22_FUNCTION_SCORE_patient60mths",
                                      "SRS22_PAIN_SCORE",
                                      "SRS22_PAIN_SCORE_patient3mths",
                                      "SRS22_PAIN_SCORE_patient12mths",
                                      "SRS22_PAIN_SCORE_patient60mths",
                                      "SRS22_SELFIMAGE_SCORE",
                                      "SRS22_SELFIMAGE_SCORE_patient3mths",
                                      "SRS22_SELFIMAGE_SCORE_patient12mths",
                                      "SRS22_SELFIMAGE_SCORE_patient60mths",
                                      "SRS22_MENTALHEALTH_SCORE",
                                      "SRS22_MENTALHEALTH_SCORE_patient3mths",
                                      "SRS22_MENTALHEALTH_SCORE_patient12mths",
                                      "SRS22_MENTALHEALTH_SCORE_patient60mths",
                                      "SRS22_SATISFACTION_SCORE",
                                      "SRS22_SATISFACTION_SCORE_patient12mths",
                                      "SRS22_SATISFACTION_SCORE_patient60mths"))

  valgt_data <- ny_data %>%
      dplyr::filter(gammelt_navn == {{x_var}}) %>%
      dplyr::select(nytt_navn)

  valgt_variabel <- valgt_data[1,1]

  return (valgt_variabel)
}

#' @title gjen_var_til_data
#'
#' Denne funksjonen tar ei kolonne fra ei dataramme og legger den til
#' ei annen dataramme. Dette gjøres pr. forløps-id. Kolonnen som legges til
#' får navnet "gjen_var"
#'
#' @param raw_data rå-regdata
#' @param data regdata som har vært gjennom prepros
#' @param gjen_var valgt variabel
#'
#' @examples
#' check <- gjen_var_til_data(raw_regdata, regdata, "Diff_prosent_kurve")
#'
#' @export

gjen_var_til_data <- function (raw_data, data, gjen_var) {

  if (gjen_var %in% c("Alder", "Knivtid", "Diff_prosent_kurve")) {

    gjen_data <- data %>%
      dplyr::mutate(gjen_var = dplyr::case_when({{gjen_var}} == "Alder" ~ Alder_num,
                                                {{gjen_var}} == "Knivtid" ~ kniv_tid,
                                                {{gjen_var}} == "Diff_prosent_kurve" ~ Diff_prosent_kurve_raw))

    return (gjen_data)

  } else {
    valg_rå <- raw_data %>%
      dplyr::select(all_of(gjen_var), MCEID)  # kun velge variablene vi er interessert i og forløps-id

    gjen_data <- dplyr::left_join(data, valg_rå, by = "MCEID")

    colnames(gjen_data)[ncol(gjen_data)] = "gjen_var"

    return (gjen_data)
    }

}


#' @title lag_gjen_tabell
#'
#' @param data data som har vært gjennom prepVar()
#'
#' @examples
#' tabell <- lag_gjen_tabell(data)
#'
#' @export


lag_gjen_tabell <- function (data) {

  gjen <- data %>%
    dplyr::filter(!is.na(gjen_var))

  gjen_pr_sykehus <- gjen %>%
    dplyr::group_by(Sykehus) %>%
    dplyr::summarise(gjennomsnitt = round(mean(gjen_var), 2),
                     median = median(gjen_var)) %>%
    dplyr::ungroup()


  gjen_total <- gjen %>%
    dplyr::summarize("gjennomsnitt nasjonalt" = round(mean(gjen_var), 2),
                     "median nasjonalt" = median(gjen_var))

  gjen_tabell <- merge(gjen_pr_sykehus, gjen_total)


  gjen_n <- gjen %>%
    group_by(Sykehus) %>%
    tally(n = "antall") %>%
    mutate("antall nasjonalt"= sum(antall))

  gjen_tabell2 <- merge(gjen_tabell, gjen_n)

  gjen_tabell2 <- gjen_tabell2 %>%
    dplyr::relocate(antall, .before = "gjennomsnitt nasjonalt")

   return(gjen_tabell2)
}

