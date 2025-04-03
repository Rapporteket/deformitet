# There are two functions in this file - makeTable and makeFreqTable

################################################################################
################################ MAKE TABLE ####################################
################################################################################


#' @title makeTable
#'
#' @export


# New table function based on prepVar


makeTable <- function(data, var_reshID, choice_var){

# Make user's hospital vs. the rest in the table based on reshID of user

  if(choice_var != "hver enhet"){
    data_hosp <- data %>%
      dplyr::select(-c(CURRENT_SURGERY)) %>%
      dplyr::filter(CENTREID == {{var_reshID}})

    data_hosp <- data_hosp %>%
      dplyr::select(-c(CENTREID, Kjønn)) %>%
      dplyr::add_tally(name = "n") %>%
      dplyr::group_by(data_hosp[3]) %>%
      dplyr::add_count(name = "by_var") %>%
      dplyr::mutate(Prosent = round(by_var/n*100, 2)) %>%
      dplyr::rename("n pr variabel" = by_var) %>%
      dplyr::distinct()
  }

  if(choice_var == "hver enhet"){
    data_hosp_all <- data

    data_hosp_all <- data_hosp_all %>%
      dplyr::select(-c(CENTREID, Kjønn, CURRENT_SURGERY)) %>%
      dplyr::group_by(Sykehus) %>%
      dplyr::add_tally(name = "n") %>%
      dplyr::ungroup() %>%
      dplyr::group_by(Sykehus, data_hosp_all[3]) %>%
      dplyr::add_count(name = "by_var") %>%
      dplyr::mutate(Prosent = round(by_var/n*100, 2)) %>%
      dplyr::rename("n pr variabel" = by_var) %>%
      dplyr::distinct()
  }

  else{
    data_all <- data

    data_all <- data_all %>%
    dplyr::select(-c(CENTREID, Kjønn, CURRENT_SURGERY)) %>%
    dplyr::mutate(Sykehus = recode(Sykehus,
                                   "Haukeland" = "Alle",
                                   "Rikshospitalet" = "Alle",
                                   "St.Olav" = "Alle")) %>%
    dplyr::add_tally(name = "n") %>%
    dplyr::group_by(data_all[3]) %>%
    dplyr::add_count(name = "by_var") %>%
    dplyr::mutate(Prosent = round(by_var/n*100, 2)) %>%
    dplyr::rename("n pr variabel" = by_var) %>%
    dplyr::distinct()


  data_full <- dplyr::full_join(data_hosp, data_all)}


  if(choice_var == "hver enhet"){
    return(data_hosp_all)
  }
  if(choice_var == "egen enhet"){
    return(data_hosp)
  }
  if(choice_var == "hele landet, uten sammenligning"){
    return(data_all)
  }
  else{return(data_full)} # => hele landet med sammenligning
  }

# nolint start

# Test to see if it works:
## g <- makeTable(rr, 103240, "enhet")

# nolint end


################################################################################
############################## MAKE FREQ TABLE #################################
################################################################################

#' @title mapping_old_name_new_name
#'
#' @export


# Jeg må bruke rådatatabellen (raw_regdata) _før_ noe har blitt konvertert til
# faktorer osv. Jeg må da lage mapping mellom x_var og navnet på denne kolonnen i
# raw_regdata.

mapping_old_name_new_name <- function (data, x_var) {


  new_data <- data.frame(old_name =
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
                             "BMI_kategori",
                             "Kurve_pre",
                             "Kurve_post",
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

                             #
                             #
                             # "Helsetilstand",
                             # "Helsetilstand 3-6 mnd",
                             # "Helsetilstand 12 mnd",
                             # "Helsetilstand 5 år",
                             # "SRS22 'Samme behandling på nytt?' 3-6 mnd",
                             # "SRS22 'Samme behandling på nytt?' 12 mnd",
                             # "SRS22 'Samme behandling på nytt?' 5 år",
                             # "SRS22 'Fornøyd med resultatet?' 3-6 mnd",
                             # "SRS22 'Fornøyd med resultatet?' 12 mdn",
                             # "SRS22 'Fornøyd med resultatet?' 5 år",
                             # "BMI-kategori", # +ALDER
                             # "Pre-operativ kurve",
                             # "Post-operativ kurve", # + diff kurve
                             # "Liggetid",
                             # "Blodtap pr. 100 ml",
                             # "Blodtap pr. 200 ml",
                             # "SRS22 totalscore preoperativt",
                             # "SRS22 totalscore 3-6 mnd",
                             # "SRS22 totalscore 12 mnd",
                             # "SRS22 totalscore 5 år",
                             # "SRS22 funksjon preoperativt",
                             # "SRS22 funksjon, 3-6 mnd",
                             # "SRS22 funksjon, 12 mnd",
                             # "SRS22 funksjon, 5 år",
                             # "SRS22 smerte preoperativt",
                             # "SRS22 smerte, 3-6 mnd",
                             # "SRS22 smerte, 12 mnd",
                             # "SRS22 smerte, 5 år",
                             # "SRS22 selvbilde preoperativt",
                             # "SRS22 selvbilde, 3-6 mnd",
                             # "SRS22 selvbilde, 12 mnd",
                             # "SRS22 selvbilde, 5 år",
                             # "SRS22 mental helse preoperativt",
                             # "SRS22 mental helse, 3-6 mnd",
                             # "SRS22 mental helse, 12 mnd",
                             # "SRS22 mental helse, 5 år",
                             # "SRS22 tilfredshet, 3-6 mnd",
                             # "SRS22 tilfredshet, 12 mnd",
                             # "SRS22 tilfredshet, 5 år"),

                         new_name = c("HELSETILSTAND_SCALE",
                                      "HEALTH_CONDITION_SCALE",
                                      "HEALTH_CONDITION_SCALE_patient12mths",
                                      "HEALTH_CONDITION_SCALE_patient_60_mths",
                                      "SRS22_22",
                                      "SRS22_22_patient12mths",
                                      "SRS22_22_patient60mths",
                                      "SRS22_21",
                                      "SRS22_21_patient12mths",
                                      "SRS22_21_patient60mths",
                                      "BMI", # + Alder
                                      "PRE_MAIN_CURVE",
                                      "POST_MAIN_CURVE", # + diff kurve
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

  select_data <- new_data %>%
      dplyr::filter(old_name == {{x_var}}) %>%
      dplyr::select(new_name)

  select_value <- select_data[1,1]

  return (select_value)
}

# nolint start
# Test to check
## ggg<- mapping_old_name_new_name(raw_regdata, "BMI_kategori")
#
# nolint end

#' @title add_freq_var_to_dataframe
#'
#' @export

# Denne funksjonen tar ei kolonne fra ei dataramme og legger den til
# ei annen dataramme. Dette gjøres pr. forløps-id. Kolonnen som legges til
# får navnet "freq_var"

add_freq_var_to_dataframe <- function (raw_data, data, freq_var) {

  select_raw <- raw_data %>%
    dplyr::select(all_of(freq_var), MCEID)  # kun velge variablene vi er interessert i og forløps-id

  freq_data <- dplyr::left_join(data, select_raw, by = "MCEID")

  colnames(freq_data)[ncol(freq_data)] = "freq_var"

  return (freq_data)

}

# nolint start
##
## rrr <- add_freq_var_to_dataframe(raw_regdata, regdata, ggg)
# nolint end




## HER KJØRER PREPVAR ##################
## x <- prepVar(rrr, "freq_var", "mm", "2023-01-02", "2024-10-02", 1, 20, "Primæroperasjon")
## xx <- data.frame(x[1])


#' @title make_freq_table
#'
#' @export


make_freq_table <- function (data, kjonn_var) {

  freq <- data %>%
    dplyr::rename(freq_var = 3)

  freq1 <- freq %>%
    dplyr::filter(!is.na(freq_var))

  freq_pr_sykehus <- freq1 %>%
    dplyr::group_by(Sykehus) %>%
    dplyr::tally() %>%
    dplyr::summarize(gjennomsnitt = round(mean(freq_var), 2),
              median = median(freq_var)) %>%
    dplyr::ungroup()

  freq_total <- freq1 %>%
    dplyr::summarize(totalt_gjennomsnitt = round(mean(freq_var), 2),
                     totalt_median = median(freq_var))

 freq_table <- merge(freq_pr_sykehus, freq_total)


 freq_n <- data %>%
   group_by(Sykehus) %>%
   tally() %>%
   mutate(total_n = sum(n))

 freq_table2 <- merge(freq_table, freq_n)

  return(freq_table2)
}

# nolint start
# test:
## d <- make_freq_table(xx)
# nolint end


