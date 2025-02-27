#' @title Preprosessering
#'
#' @export
# List of variables to be prepared: (i) kjønn - GENDER;
# (ii) alder - SURGERY_DATE- BIRTH_DATE;
# (iii) BMI;
# (iv) Pre-operative curve - PRE_MAIN_CURVE;
# (v) Post-operative curve - POST_MAIN_CURVE;
# (vi) liggetid - BED_DAYS_POSTOPERATVE;
# (vii) knivtid - KNIFE_TIME_EXACT_MIN + regne ut KNIFE_TIME_START_HOUR
# KNIFE_TIME_START_MIN - KNIFE_TIME_END_HOUR + KNIFE_TIME_END_MIN
# (viii) blodtap - PER_BLOOD_LOSS_VALUE
# (ix) SRS22 PROM-score - se totalt + for de ulike domenene: SRS22_MAIN_SCORE osv.
# (x) EQ5D_SCORE + evt. for de ulike domenene
# (xi) Helsetilstand scale - HELSETILSTAND_SCALE + i patientfollowup heter dette
# "HEALTH_CONDITION_SCALE"
# (xii) komplikasjoner - de ulike komplikasjonstypene. This is prepared in a
# separate file

##### This function is run on regdata after regdata is returned from
##### les_og_flate_ut()
##### This function returns a complete data frame (no choices at this stage)


pre_pros <- function(regdata){

  regdata <- regdata %>%
    dplyr::select(-starts_with("USERCOMMENT"))


# SYKEHUS:
regdata <- regdata %>%
  dplyr::rename(Sykehus = CENTRESHORTNAME,
                Kjønn = GENDER)  %>%
  dplyr::mutate(Sykehus = dplyr::recode(Sykehus,
                                        "Bergen" = "Haukeland",
                                        "Riksen" = "Rikshospitalet"))


# (i) FOR KJØNN:
# a. Turn into character
regdata$Kjønn <- as.character(regdata$Kjønn)

# b. Rename rows:
regdata <- regdata %>%
  dplyr::mutate(Kjønn = recode(Kjønn, "1" = "mann", "2" = "kvinne"))

# (ii) FOR ALDER:
# a. Find number of years
regdata <- regdata %>%
  dplyr::mutate(Alder =
                  (difftime(SURGERY_DATE, BIRTH_DATE,
                            units = "weeks"))/52)

# b. Divide into age groups (defined by registry)
regdata$Alder <- as.numeric(regdata$Alder)
regdata$Alder_num <- as.integer(regdata$Alder)


regdata$Alder <- cut(regdata$Alder,
                     breaks = c(9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21),
                     labels = c("<9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20+"))


# (iii) FOR BMI:
# a. Make into a factor
regdata <- regdata %>%
  dplyr::mutate(BMI_kategori = as.factor(recode(BMI_CATEGORY,
    "Alvorlig undervekt" = "Alvorlig undervekt\n < 16",
    "Moderat undervekt" = "Undervekt\n (16-17)",
    "Mild undervekt" = "Mild undervekt\n (17-18,5)",
    "Normal" =  "Normal\n (18,5-25)",
    "Overvekt" = "Overvekt\n (25-30)",
    "Moderat fedme, klasse I" =  "Moderat fedme\n, klasse I (30-35)",
    "Fedme, klasse II" = "Fedme, klasse II \n (35-40)",
    "Fedme, klasse III" = "Fedme, klasse III \n (40-50)")))

regdata$BMI_kategori <- ordered(regdata$BMI_kategori,
                                levels =c("Alvorlig undervekt\n < 16",
                                          "Undervekt\n (16-17)",
                                          "Mild undervekt\n (17-18,5)",
                                          "Normal\n (18,5-25)",
                                          "Overvekt\n (25-30)",
                                          "Moderat fedme\n, klasse I (30-35)",
                                          "Fedme, klasse II \n (35-40)",
                                          "Fedme, klasse III \n (40-50)"))


# (iv) FOR PRE-OPERATIV KURVE:
  # a. Transform to numeric
regdata$PRE_MAIN_CURVE <- as.numeric(regdata$PRE_MAIN_CURVE)

  # b. Make curve groups (for easier visualization)
regdata <- regdata %>%
  dplyr::mutate(Kurve_pre = cut(PRE_MAIN_CURVE,
                                breaks = c(0, 40, 44, 49, 54, 59, 64, 69, 110),
                                labels = c("< 40", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "> 70")))


# (v) FOR POST-OPERATIV KURVE:
  # a. Transform to numeric
regdata$POST_MAIN_CURVE <- as.numeric(regdata$POST_MAIN_CURVE)

# b. Make curve groups (for easier visualization)
regdata <- regdata %>%
  dplyr::mutate(Kurve_post = cut(POST_MAIN_CURVE,
                                 breaks = c(0, 9, 19, 29, 39, 110),
                                 labels = c("0-9", "10-19", "20-29", "30-39", "> 40")))

# b. FOR KURVE-DIFFERANSE PROSENT:
regdata <- regdata %>%
  dplyr::mutate(Diff_prosent_kurve = (((PRE_MAIN_CURVE - POST_MAIN_CURVE)/PRE_MAIN_CURVE)*100),
                Diff_prosent_kurve = round(Diff_prosent_kurve, digits = 0),
                Diff_prosent_kurve = cut(Diff_prosent_kurve,
                                         breaks = c(0, 45, 55, 65, 75, 80, 85, 90, 95, 105),
                                         labels = c("0-44", "45-54", "55-64", "65-74", "75-79", "80-84", "85-89", "90-94", "95-100")))


# (vi) FOR LIGGETID:
# a. Transform to numeric
regdata$BED_DAYS_POSTOPERATIVE <- as.numeric(regdata$BED_DAYS_POSTOPERATIVE)

# b. Make curve groups (for easier visualization)
regdata <- regdata %>%
  dplyr::mutate(Liggetid = cut(BED_DAYS_POSTOPERATIVE,
                               breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 40),
                               labels = c("1", "2", "3", "4", "5", "6", "7", "> 7")))


# (vii) FOR KNIVTID:
regdata <- regdata %>%
  tidyr::unite("my_start", KNIFE_TIME_START_HOUR:KNIFE_TIME_START_MIN, sep = ":") %>%
  tidyr::unite("my_end", KNIFE_TIME_END_HOUR:KNIFE_TIME_END_MIN, sep = ":")

regdata$my_start <- strptime(regdata$my_start, "%H:%M")
regdata$my_end <- strptime(regdata$my_end, "%H:%M")

regdata <- regdata %>%
  dplyr::mutate(my_time_mins = my_end-my_start,
                my_time_mins = gsub("mins", "", my_time_mins),
                my_time_mins = as.numeric(my_time_mins),
                my_time_mins = tidyr::replace_na(my_time_mins, 0),
                KNIFE_TIME_EXACT_TIMER = tidyr::replace_na(KNIFE_TIME_EXACT_TIMER, 0),
                KNIFE_TIME_EXACT_MIN= tidyr::replace_na(KNIFE_TIME_EXACT_MIN, 0),
                KNIFE_TIME_EXACT_TIMER = KNIFE_TIME_EXACT_TIMER*60,
                my_time_mins2 = KNIFE_TIME_EXACT_TIMER+KNIFE_TIME_EXACT_MIN,
                my_time_mins2 = tidyr::replace_na(my_time_mins2, 0),
                kniv_tid = my_time_mins+my_time_mins2) %>%
  dplyr::select(-KNIFE_TIME_EXACT_MIN, -KNIFE_TIME_EXACT_TIMER, -my_time_mins,
                -my_time_mins2, -my_start, -my_end)

regdata <- regdata %>%
  dplyr::mutate(Knivtid = cut(kniv_tid,
                              breaks = c(0, 60, 90, 120, 150, 180, 210, 240,
                                         270, 300, 330, 360, 1000),
                              labels = c("1", "1.5", "2", "2.5", "3", "3.5",
                                         "4", "4.5", "5", "5.5", "6", "> 6")))


# (viii) FOR BLODTAP:
# a. Make groups (for easier visualization)
regdata <- regdata %>%
  dplyr::mutate(Blodtap_100 = cut(PER_BLOOD_LOSS_VALUE,
                                  breaks = c(1, 99, 199, 299, 399, 499, 599, 699, 799, 899, 999, 1099, 1199, 1299, 1399, 1499, 2000),
                                  labels = c("< 100", "100-199", "200-299", "300-399", "400-499", "500-599", "600-699", "700-799", "800-899", "900-999", "1000-1099", "1100-1199", "1200-1299", "1300-1399", "1400-1499", "> 1500")))

regdata <- regdata %>%
  dplyr::mutate(Blodtap_200 = cut(PER_BLOOD_LOSS_VALUE,
                                  breaks = c(1, 199, 399, 599, 799, 999, 1199, 1399, 1599, 2000),
                                  labels = c("< 200", "200-399", "400-599", "600-799", "800-999", "1000-1199", "1200-1399", "1400-1599", "> 1600")))


# (ix) FOR SRS22
# MAIN FORM
regdata <- regdata %>%
  dplyr::mutate(SRS22_total = cut(SRS22_MAIN_SCORE,
                                  breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                  labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")))

regdata <- regdata %>%
  dplyr::mutate(SRS22_total_3mnd = cut(SRS22_FULL_SCORE,
                                        breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                        labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")))

regdata <- regdata %>%
  dplyr::mutate(SRS22_total_12mnd = cut(SRS22_FULL_SCORE_patient12mths,
                                       breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                       labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")))

regdata <- regdata %>%
  dplyr::mutate(SRS22_total_60mnd = cut(SRS22_FULL_SCORE_patient12mths,
                                        breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                        labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")))


# FUNCTION
  # Preop; 3-6 mnd; 12 mnd; 60 mnd
regdata <- regdata %>%
  dplyr::mutate(SRS22_funksjon = cut(SRS22_FUNCTION_SCORE,
                                     breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                     labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")),
                SRS22_funksjon_3mnd = cut(SRS22_FUNCTION_SCORE_patient3mths,
                                          breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                          labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")),
                SRS22_funksjon_12mnd = cut(SRS22_FUNCTION_SCORE_patient12mths,
                                           breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                           labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")),
                SRS22_funksjon_60mnd = cut(SRS22_FUNCTION_SCORE_patient60mths,
                                           breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                           labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")))

# PAIN
  # Preop; 3-6 mnd; 12 mnd; 60 mnd
regdata <- regdata %>%
  dplyr::mutate(SRS22_smerte = cut(SRS22_PAIN_SCORE,
                                   breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                   labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")),
                SRS22_smerte_3mnd = cut(SRS22_PAIN_SCORE_patient3mths,
                                        breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                        labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")),
                SRS22_smerte_12mnd = cut(SRS22_PAIN_SCORE_patient12mths,
                                         breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                         labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")),
                SRS22_smerte_60mnd = cut(SRS22_PAIN_SCORE_patient60mths,
                                         breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                         labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")))

# SELF IMAGE
  # Preop; 3-6 mnd; 12 mnd; 60 mnd
regdata <- regdata %>%
  dplyr::mutate(SRS22_selvbilde = cut(SRS22_SELFIMAGE_SCORE,
                                      breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                      labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")),
                SRS22_selvbilde_3mnd = cut(SRS22_SELFIMAGE_SCORE_patient3mths,
                                           breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                           labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")),
                SRS22_selvbilde_12mnd = cut(SRS22_SELFIMAGE_SCORE_patient12mths,
                                            breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                            labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")),
                SRS22_selvbilde_60mnd = cut(SRS22_SELFIMAGE_SCORE_patient60mths,
                                            breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                            labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")))

# MENTAL HEALTH
  # Preop; 3-6 mnd; 12 mnd; 60 mnd
regdata <- regdata %>%
  dplyr::mutate(SRS22_mhelse = cut(SRS22_MENTALHEALTH_SCORE,
                                   breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                   labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")),
                SRS22_mhelse_3mnd = cut(SRS22_MENTALHEALTH_SCORE_patient3mths,
                                        breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                        labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", " 3.5-3.9", "4-4.4", "4.5-5")),
                SRS22_mhelse_12mnd = cut(SRS22_MENTALHEALTH_SCORE_patient12mths,
                                         breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                         labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")),
                SRS22_mhelse_60mnd = cut(SRS22_MENTALHEALTH_SCORE_patient60mths,
                                         breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                         labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")))

# SRS22 - SATISFACTION (21 og 21) - from patient followup
  # Preop; 3-6 mnd; 12 mnd; 60 mnd
regdata <- regdata %>%
  dplyr::mutate(SRS22_fornoyd_3mnd = cut(SRS22_SATISFACTION_SCORE,
                                         breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                         labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")),
                SRS22_fornoyd_12mnd = cut(SRS22_SATISFACTION_SCORE_patient12mths,
                                          breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                          labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")),
                SRS22_fornoyd_60mnd = cut(SRS22_SATISFACTION_SCORE_patient60mths,
                                          breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                          labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")),

               # Question 22
                 SRS22_spm21_3mnd = cut(SRS22_21,
                                        breaks = c(0, 1, 2, 3, 4, 5, 10),
                                        labels = c("Svært misfornøyd", "Litt misfornøyd", "Verken fornøyd eller misfornøyd", "Ganske fornøyd", "Svært godt fornøyd", "Ikke utfylt")),
                 SRS22_spm21_12mnd = cut(SRS22_21_patient12mths,
                                        breaks = c(0, 1, 2, 3, 4, 5, 10),
                                        labels = c("Svært misfornøyd", "Litt misfornøyd", "Verken fornøyd eller misfornøyd", "Ganske fornøyd", "Svært godt fornøyd", "Ikke utfylt")),
                 SRS22_spm21_60mnd = cut(SRS22_21_patient60mths,
                                        breaks = c(0, 1, 2, 3, 4, 5, 10),
                                        labels = c("Svært misfornøyd", "Litt misfornøyd", "Verken fornøyd eller misfornøyd", "Ganske fornøyd", "Svært godt fornøyd", "Ikke utfylt")),

                 # Questions 21
                 SRS22_spm22_3mnd = cut(SRS22_22,
                                        breaks = c(0, 1, 2, 3, 4, 5, 10),
                                        labels = c("Definitivt ikke", "Sannsynligvis ikke", "Usikker", "Sannsynligvis ja", "Definitivt ja", "Ikke utfylt")),
                 SRS22_spm22_12mnd = cut(SRS22_22_patient12mths,
                                         breaks = c(0, 1, 2, 3, 4, 5, 10),
                                         labels = c("Definitivt ikke", "Sannsynligvis ikke", "Usikker", "Sannsynligvis ja", "Definitivt ja", "Ikke utfylt")),
                 SRS22_spm22_60mnd = cut(SRS22_22_patient60mths,
                                         breaks = c(0, 1, 2, 3, 4, 5, 10),
                                         labels = c("Definitivt ikke", "Sannsynligvis ikke", "Usikker", "Sannsynligvis ja", "Definitivt ja", "Ikke utfylt")))

# (x) FOR EQ5D


# (xi) FOR HELSETILSTAND
regdata <- regdata %>%
  dplyr::mutate(Helsetilstand = cut(HELSETILSTAND_SCALE,
                                    breaks = c(0, 15, 30, 45, 60, 75, 90, 100),
                                    labels = c("> 15", "15-30", "31-45", "46-60", "61-75", "76-90", "> 90")),
               # 3-6 mnd
               Helsetilstand_3mnd = cut(HEALTH_CONDITION_SCALE,
                                        breaks = c(0, 15, 30, 45, 60, 75, 90, 100),
                                        labels = c("> 15", "15-30", "31-45", "46-60", "61-75", "76-90", "> 90")),
              # 12 mnd
              Helsetilstand_12mnd = cut(HEALTH_CONDITION_SCALE_patient12mths,
                                        breaks = c(0, 15, 30, 45, 60, 75, 90, 100),
                                        labels = c("> 15", "15-30", "31-45", "46-60", "61-75", "76-90", "> 90")),
              # 60 mnd
              Helsetilstand_60mnd = cut(HEALTH_CONDITION_SCALE_patient60mths,
                                        breaks = c(0, 15, 30, 45, 60, 75, 90, 100),
                                        labels = c("> 15", "15-30", "31-45", "46-60", "61-75", "76-90", "> 90")))


# (xii) FOR KOMPLIKASJONER
  # Procedure complications as indicated by patient
regdata$PROCEDURE_COMPLICATIONS <- as.character(regdata$PROCEDURE_COMPLICATIONS)
regdata$PROCEDURE_COMPLICATIONS_patient12mths <- as.character(regdata$PROCEDURE_COMPLICATIONS_patient12mths)
regdata$PROCEDURE_COMPLICATIONS_patient60mths <- as.character(regdata$PROCEDURE_COMPLICATIONS_patient60mths)

regdata <- regdata %>%
  dplyr::mutate(Komplikasjoner_3mnd = recode(PROCEDURE_COMPLICATIONS, "0" = "Nei", "1" = "Ja", "9" = "Ikke utfylt"))

regdata <- regdata %>%
  dplyr::mutate(Komplikasjoner_12mnd = recode(PROCEDURE_COMPLICATIONS_patient12mths, "0" = "Nei", "1" = "Ja", "9" = "Ikke utfylt"))

regdata <- regdata %>%
  dplyr::mutate(Komplikasjoner_60mnd = recode(PROCEDURE_COMPLICATIONS_patient60mths, "0" = "Nei", "1" = "Ja", "9" = "Ikke utfylt"))


return(regdata)
 }

