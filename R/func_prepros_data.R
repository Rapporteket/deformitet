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

##### This function is run on RegData after RegData is returned from
##### alleRegData()
##### This function returns a complete data frame (no choices at this stage)


pre_pros <- function(RegData){

  RegData <- RegData |>
    dplyr::rename(PID = PATIENT_ID)

  RegData <- RegData |>
    dplyr::select(-dplyr::starts_with("USERCOMMENT"))


  RegData$ErMann = RegData$GENDER
  RegData$ErMann[RegData$ErMann==2] <- 0

  # SYKEHUS:
  #new_name = old_name
RegData <- RegData |>
  dplyr::rename(Sykehus = CENTRESHORTNAME,
                Kjonn = GENDER)
# Dette må gjøres i QReg av registeret
#  |> dplyr::mutate(Sykehus = dplyr::recode(Sykehus, "Bergen" = "Haukeland", "Riksen" = "Rikshospitalet"))


# KJØNN:
RegData$Kjonn <- as.character(RegData$Kjonn)
RegData <- RegData |>
  dplyr::mutate(Kjonn = dplyr::recode(Kjonn, "1" = "mann", "2" = "kvinne"))


# (ii) FOR ALDER:
# a. Find number of years
RegData <- RegData |>
  dplyr::mutate(Alder =
                  (difftime(SURGERY_DATE, BIRTH_DATE,
                            units = "days"))/365.15)

# b. Divide into age groups (defined by registry)
RegData$Alder <- as.numeric(RegData$Alder)
RegData$Alder_num <- as.integer(RegData$Alder)




# -----------Tilrettelegge variabler-----------------------

RegData$Alder <- cut(RegData$Alder,
                     breaks = c(9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 100),
                     labels = c("<9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20+"))


# (iii) FOR BMI:
# a. Make into a factor
RegData <- RegData |>
  dplyr::mutate(BMI_kategori = as.factor(dplyr::recode(BMI_CATEGORY,
    "Alvorlig undervekt" = "Alvorlig undervekt < 16",
    "Moderat undervekt" = "Undervekt (16-17)",
    "Mild undervekt" = "Mild undervekt (17-18,5)",
    "Normal" =  "Normal (18,5-25)",
    "Overvekt" = "Overvekt (25-30)",
    "Moderat fedme, klasse I" =  "Moderat fedme, klasse I (30-35)",
    "Fedme, klasse II" = "Fedme, klasse II (35-40)",
    "Fedme, klasse III" = "Fedme, klasse III (40-50)")))

RegData$BMI_kategori <- ordered(RegData$BMI_kategori,
                                levels =c("Alvorlig undervekt < 16",
                                          "Undervekt (16-17)",
                                          "Mild undervekt (17-18,5)",
                                          "Normal (18,5-25)",
                                          "Overvekt (25-30)",
                                          "Moderat fedme, klasse I (30-35)",
                                          "Fedme, klasse II (35-40)",
                                          "Fedme, klasse III (40-50)"))


# (iv) FOR PRE-OPERATIV KURVE:
  # a. Transform to numeric
RegData$PRE_MAIN_CURVE <- as.numeric(RegData$PRE_MAIN_CURVE)

  # b. Make curve groups (for easier visualization)
RegData <- RegData |>
  dplyr::mutate(Kurve_pre = cut(PRE_MAIN_CURVE,
                                breaks = c(0, 40, 44, 49, 54, 59, 64, 69, 110),
                                labels = c("< 40", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "> 70")))


# (v) FOR POST-OPERATIV KURVE:
  # a. Transform to numeric
RegData$POST_MAIN_CURVE <- as.numeric(RegData$POST_MAIN_CURVE)

# b. Make curve groups (for easier visualization)
RegData <- RegData |>
  dplyr::mutate(Kurve_post = cut(POST_MAIN_CURVE,
                                 breaks = c(0, 9, 19, 29, 39, 110),
                                 labels = c("0-9", "10-19", "20-29", "30-39", "> 40")))

# b. FOR KURVE-DIFFERANSE PROSENT:
RegData <- RegData |>
  dplyr::mutate(Diff_prosent_kurve = (((PRE_MAIN_CURVE - POST_MAIN_CURVE)/PRE_MAIN_CURVE)*100),
                Diff_prosent_kurve_raw = round(.data$Diff_prosent_kurve, digits = 0),
                Diff_prosent_kurve = cut(Diff_prosent_kurve_raw,
                                         breaks = c(0, 45, 55, 65, 75, 80, 85, 90, 95, 105),
                                         labels = c("0-44", "45-54", "55-64", "65-74", "75-79", "80-84", "85-89", "90-94", "95-100")))


# (vi) FOR LIGGETID:
# a. Transform to numeric
RegData$BED_DAYS_POSTOPERATIVE <- as.numeric(RegData$BED_DAYS_POSTOPERATIVE)

# b. Make curve groups (for easier visualization)
RegData <- RegData |>
  dplyr::mutate(Liggetid = cut(BED_DAYS_POSTOPERATIVE,
                               breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 40),
                               labels = c("1", "2", "3", "4", "5", "6", "7", "> 7")))


# (vii) FOR KNIVTID:
RegData <- RegData |>
  tidyr::unite("my_start", KNIFE_TIME_START_HOUR:KNIFE_TIME_START_MIN, sep = ":") |>
  tidyr::unite("my_end", KNIFE_TIME_END_HOUR:KNIFE_TIME_END_MIN, sep = ":")

RegData$my_start <- strptime(RegData$my_start, "%H:%M")
RegData$my_end <- strptime(RegData$my_end, "%H:%M")

RegData <- RegData |>
  dplyr::mutate(my_time_mins = my_end-my_start,
                my_time_mins = gsub("mins", "", my_time_mins),
                my_time_mins = as.numeric(my_time_mins),
                my_time_mins = tidyr::replace_na(my_time_mins, 0),
                KNIFE_TIME_EXACT_TIMER = tidyr::replace_na(KNIFE_TIME_EXACT_TIMER, 0),
                KNIFE_TIME_EXACT_MIN= tidyr::replace_na(KNIFE_TIME_EXACT_MIN, 0),
                KNIFE_TIME_EXACT_TIMER = KNIFE_TIME_EXACT_TIMER*60,
                my_time_mins2 = KNIFE_TIME_EXACT_TIMER+KNIFE_TIME_EXACT_MIN,
                my_time_mins2 = tidyr::replace_na(my_time_mins2, 0),
                kniv_tid = my_time_mins+my_time_mins2) |>
  dplyr::select(-"KNIFE_TIME_EXACT_MIN", -"KNIFE_TIME_EXACT_TIMER", -"my_time_mins",
                -"my_time_mins2", -"my_start", -"my_end")

RegData <- RegData |>
  dplyr::mutate(Knivtid = cut(kniv_tid,
                              breaks = c(0, 60, 90, 120, 150, 180, 210, 240,
                                         270, 300, 330, 360, 1000),
                              labels = c("1", "1.5", "2", "2.5", "3", "3.5",
                                         "4", "4.5", "5", "5.5", "6", "> 6")))


# (viii) FOR BLODTAP:
# a. Make groups (for easier visualization)
RegData <- RegData |>
  dplyr::mutate(Blodtap_100 = cut(PER_BLOOD_LOSS_VALUE,
                                  breaks = c(1, 99, 199, 299, 399, 499, 599, 699, 799, 899, 999, 1099, 1199, 1299, 1399, 1499, 2000),
                                  labels = c("< 100", "100-199", "200-299", "300-399", "400-499", "500-599", "600-699", "700-799", "800-899", "900-999", "1000-1099", "1100-1199", "1200-1299", "1300-1399", "1400-1499", "> 1500")))

RegData <- RegData |>
  dplyr::mutate(Blodtap_200 = cut(PER_BLOOD_LOSS_VALUE,
                                  breaks = c(1, 199, 399, 599, 799, 999, 1199, 1399, 1599, 2000),
                                  labels = c("< 200", "200-399", "400-599", "600-799", "800-999", "1000-1199", "1200-1399", "1400-1599", "> 1600")))


# (ix) FOR SRS22
# MAIN FORM
RegData <- RegData |>
  dplyr::mutate(SRS22_total = cut(SRS22_MAIN_SCORE,
                                  breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                  labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")))

RegData <- RegData |>
  dplyr::mutate(SRS22_total_3mnd = cut(SRS22_FULL_SCORE,
                                        breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                        labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")))

RegData <- RegData |>
  dplyr::mutate(SRS22_total_12mnd = cut(SRS22_FULL_SCORE_patient12mths,
                                       breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                       labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")))

RegData <- RegData |>
  dplyr::mutate(SRS22_total_60mnd = cut(SRS22_FULL_SCORE_patient12mths,
                                        breaks = c(0, 2, 2.4, 2.9, 3.4, 3.9, 4.4, 5),
                                        labels = c("< 2", "2-2.4", "2.5-2.9", "3-3.4", "3.5-3.9", "4-4.4", "4.5-5")))


# FUNCTION
  # Preop; 3-6 mnd; 12 mnd; 60 mnd
RegData <- RegData |>
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
RegData <- RegData |>
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
RegData <- RegData |>
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
RegData <- RegData |>
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
RegData <- RegData |>
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
RegData <- RegData |>
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
RegData$PROCEDURE_COMPLICATIONS <- as.character(RegData$PROCEDURE_COMPLICATIONS)
RegData$PROCEDURE_COMPLICATIONS_patient12mths <- as.character(RegData$PROCEDURE_COMPLICATIONS_patient12mths)
RegData$PROCEDURE_COMPLICATIONS_patient60mths <- as.character(RegData$PROCEDURE_COMPLICATIONS_patient60mths)

RegData <- RegData |>
  dplyr::mutate(Komplikasjoner_3mnd = dplyr::recode(PROCEDURE_COMPLICATIONS, "0" = "Nei", "1" = "Ja", "9" = "Ikke utfylt"))

RegData <- RegData |>
  dplyr::mutate(Komplikasjoner_12mnd = dplyr::recode(PROCEDURE_COMPLICATIONS_patient12mths, "0" = "Nei", "1" = "Ja", "9" = "Ikke utfylt"))

RegData <- RegData |>
  dplyr::mutate(Komplikasjoner_60mnd = dplyr::recode(PROCEDURE_COMPLICATIONS_patient60mths, "0" = "Nei", "1" = "Ja", "9" = "Ikke utfylt"))


return(RegData)
}


