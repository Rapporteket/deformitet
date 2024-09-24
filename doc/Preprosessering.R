##### FIX VARIABLES #####
# Pre-processing of variables before they're being prepared (PrepVar) for table-
# making (makeTable) and plotting (makeHistPlot_gg)

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
# (xii) komplikasjoner - de ulike komplikasjonstypene

regdata <- deformitet::les_og_flate_ut()

pre_pros <- function(regdata){


# FOR NAVN PÅ SYKEHUS:
regdata <- regdata %>%
  rename(Sykehus = CENTRESHORTNAME,
         Kjønn = GENDER)

# (i) FOR KJØNN:
# a. Turn into character
regdata$Kjønn <- as.character(regdata$Kjønn)

# b. Rename rows:
regdata <- regdata %>%
  mutate(Kjønn = recode(Kjønn, "1" = "mann", "2" = "kvinne"))

# (ii) FOR ALDER:
# a. Find number of years
regdata <- regdata %>%
  mutate(Alder = (difftime(SURGERY_DATE, BIRTH_DATE, units = "weeks"))/52)

# b. Divide into age groups (defined by registry)
regdata$Alder <- as.numeric(regdata$Alder)
regdata$Alder <- cut(regdata$Alder,
                     breaks = c(9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21),
                     labels = c("0-9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20+"))


# (iii) FOR BMI:
# a. Make into a factor
regdata <- regdata %>%
  mutate(BMI_kategori = as.factor(recode(BMI_CATEGORY,
    "Alvorlig undervekt" = "Alvorlig undervekt\n < 16",
    "Moderat undervekt" = "Undervekt\n (16-17)",
    "Mild undervekt" = "Mild undervekt\n (17-18,5)",
    "Normal" =  "Normal\n (18,5-25)",
    "Overvekt" = "Overvekt\n (25-30)",
    "Moderat fedme, klasse I" =  "Moderat fedme\n, klasse I (30-35)",
    "Fedme, klasse II" = "Fedme, klasse II \n (35-40)",
    "Fedme, klasse III" = "Fedme, klasse III \n (40-50)")))


# (iv) FOR PRE-OPERATIV KURVE:
  # a. Transform to numeric
regdata$PRE_MAIN_CURVE <- as.numeric(regdata$PRE_MAIN_CURVE)

  # b. Make curve groups (for easier visualization)
regdata <- regdata %>%
  mutate(Kurve_pre = cut(PRE_MAIN_CURVE,
    breaks = c(0, 40, 44, 49, 54, 59, 64, 69, 110),
    labels = c("< 40", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "> 70")))


# (v) FOR POST-OPERATIV KURVE:
  # a. Transform to numeric
regdata$POST_MAIN_CURVE <- as.numeric(regdata$POST_MAIN_CURVE)

# b. Make curve groups (for easier visualization)
regdata <- regdata %>%
  mutate(Kurve_post = cut(POST_MAIN_CURVE,
                          breaks = c(0, 9, 19, 29, 39, 110),
                          labels = c("0-9", "10-19", "20-29", "30-39", "> 40")))

# (vi) FOR LIGGETID:
# a. Transform to numeric
regdata$BED_DAYS_POSTOPERATIVE <- as.numeric(regdata$BED_DAYS_POSTOPERATIVE)

# b. Make curve groups (for easier visualization)
regdata <- regdata %>%
  mutate(Liggetid = cut(BED_DAYS_POSTOPERATIVE,
                          breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 40),
                          labels = c("1", "2", "3", "4", "5", "6", "7", "> 7")))


# (vii) FOR KNIVTID:
regdata <- regdata %>%
  unite("my_start", KNIFE_TIME_START_HOUR:KNIFE_TIME_START_MIN, sep = ":") %>%
  unite("my_end", KNIFE_TIME_END_HOUR:KNIFE_TIME_END_MIN, sep = ":")

regdata$my_start <- strptime(regdata$my_start, "%H:%M")
regdata$my_end <- strptime(regdata$my_end, "%H:%M")

regdata <- regdata %>%
  mutate(my_time_mins = my_end-my_start,
         my_time_mins = gsub("mins", "", my_time_mins),
         my_time_mins = as.numeric(my_time_mins),
         my_time_mins = replace_na(my_time_mins, 0),
         KNIFE_TIME_EXACT_TIMER = replace_na(KNIFE_TIME_EXACT_TIMER, 0),
         KNIFE_TIME_EXACT_MIN= replace_na(KNIFE_TIME_EXACT_MIN, 0),
         KNIFE_TIME_EXACT_TIMER = KNIFE_TIME_EXACT_TIMER*60,
         my_time_mins2 = KNIFE_TIME_EXACT_TIMER+KNIFE_TIME_EXACT_MIN,
         my_time_mins2 = replace_na(my_time_mins2, 0),
         kniv_tid = my_time_mins+my_time_mins2) %>%
  select(-KNIFE_TIME_EXACT_MIN, -KNIFE_TIME_EXACT_TIMER, -my_time_mins,
         -my_time_mins2, -my_start, -my_end)

regdata <- regdata %>%
  mutate(kniv_tid = cut(kniv_tid,
                        breaks = c(0, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330, 360, 1000),
                        labels = c("1", "1.5", "2", "2.5", "3", "3.5", "4", "4.5", "5", "5.5", "6", "> 6"))) #TROR IKKE JEG VIL HA LABELS HER

############ JEG TROR IKKE DET FUNGERER Å LEGGE TIL BREAKS UTEN LABELS #########################
# EVT prøv: labels = NULL????


# (viii) FOR BLODTAP:
# a. Make groups (for easier visualization)
regdata <- regdata %>%
  mutate(Blodtap = cut(PER_BLOOD_LOSS_VALUE,
                        breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500))) ## HAR LYST PÅ MINDRE GRUPPER HER PGA FÅ REGISTRERINGER

# (ix) FOR SRS22
# MAIN FORM
regdata <- regdata %>%
  mutate(SRS22_total = cut(SRS22_MAIN_SCORE,
                           breaks = c(1, 2, 2.5, 3, 3.5, 4, 4.5, 5))) # Jeg synes disse gir mening

# FUNCTION
  # Preop; 3-6 mnd; 12 mnd; 60 mnd
regdata <- regdata %>%
  mutate(SRS22_function = cut(SRS22_FUNCTION_SCORE,
                           breaks = c(1, 2, 2.5, 3, 3.5, 4, 4.5, 5)),
         SRS22_function_3mnd = cut(SRS22_FUNCTION_SCORE_patient3mths,
                              breaks = c(1, 2, 2.5, 3, 3.5, 4, 4.5, 5)),
         SRS22_function_12mnd = cut(SRS22_FUNCTION_SCORE_patient12mths,
                                   breaks = c(1, 2, 2.5, 3, 3.5, 4, 4.5, 5)),
         SRS22_function_60mnd = cut(SRS22_FUNCTION_SCORE_patient60mths,
                                    breaks = c(1, 2, 2.5, 3, 3.5, 4, 4.5, 5))) # Jeg synes disse gir mening

# PAIN
  # Preop; 3-6 mnd; 12 mnd; 60 mnd
regdata <- regdata %>%
  mutate(SRS22_pain = cut(SRS22_PAIN_SCORE,
                              breaks = c(1, 2, 2.5, 3, 3.5, 4, 4.5, 5)),
         SRS22_pain_3mnd = cut(SRS22_PAIN_SCORE_patient3mths,
                          breaks = c(1, 2, 2.5, 3, 3.5, 4, 4.5, 5)),
         SRS22_pain_12mnd = cut(SRS22_PAIN_SCORE_patient12mths,
                          breaks = c(1, 2, 2.5, 3, 3.5, 4, 4.5, 5)),
         SRS22_pain_60mnd = cut(SRS22_PAIN_SCORE_patient60mths,
                          breaks = c(1, 2, 2.5, 3, 3.5, 4, 4.5, 5))) # Jeg synes disse gir mening

# SELF IMAGE
  # Preop; 3-6 mnd; 12 mnd; 60 mnd
regdata <- regdata %>%
  mutate(SRS22_self_image = cut(SRS22_SELFIMAGE_SCORE,
                          breaks = c(1, 2, 2.5, 3, 3.5, 4, 4.5, 5)),
         SRS22_self_image_3mnd = cut(SRS22_SELFIMAGE_SCORE_patient3mths,
                               breaks = c(1, 2, 2.5, 3, 3.5, 4, 4.5, 5)),
         SRS22_self_image_12mnd = cut(SRS22_SELFIMAGE_SCORE_patient12mths,
                                breaks = c(1, 2, 2.5, 3, 3.5, 4, 4.5, 5)),
         SRS22_self_image_60mnd = cut(SRS22_SELFIMAGE_SCORE_patient60mths,
                                breaks = c(1, 2, 2.5, 3, 3.5, 4, 4.5, 5)))

# MENTAL HEALTH
  # Preop; 3-6 mnd; 12 mnd; 60 mnd
regdata <- regdata %>%
  mutate(SRS22_mental_health = cut(SRS22_MENTALHEALTH_SCORE,
                          breaks = c(1, 2, 2.5, 3, 3.5, 4, 4.5, 5)),
         SRS22_mental_health_3mnd = cut(SRS22_MENTALHEALTH_SCORE_patient3mths,
                               breaks = c(1, 2, 2.5, 3, 3.5, 4, 4.5, 5)),
         SRS22_mental_health_12mnd = cut(SRS22_MENTALHEALTH_SCORE_patient12mths,
                                breaks = c(1, 2, 2.5, 3, 3.5, 4, 4.5, 5)),
         SRS22_mental_health_60mnd = cut(SRS22_MENTALHEALTH_SCORE_patient60mths,
                                breaks = c(1, 2, 2.5, 3, 3.5, 4, 4.5, 5))) # Jeg synes disse gir mening

# SRS22 - SATISFACTION (21 og 21) - from patient followup
  # Preop; 3-6 mnd; 12 mnd; 60 mnd
regdata <- regdata %>%
  mutate(SRS22_satisfaction_3mnd = cut(SRS22_SATISFACTION_SCORE,
                                        breaks = c(1, 2, 2.5, 3, 3.5, 4, 4.5, 5)),
         SRS22_satisfaction_12mnd = cut(SRS22_SATISFACTION_SCORE_patient12mths,
                                         breaks = c(1, 2, 2.5, 3, 3.5, 4, 4.5, 5)),
         SRS22_satisfaction_60mnd = cut(SRS22_SATISFACTION_SCORE_patient60mths,
                                         breaks = c(1, 2, 2.5, 3, 3.5, 4, 4.5, 5)),

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
  mutate(Helsetilstand = cut(HELSETILSTAND_SCALE,
                             breaks = c(0, 15, 30, 45, 60, 75, 90, 100)),
         # 3-6 mnd
         Helsetilstand_3mnd = cut(HEALTH_CONDITION_SCALE,
                                  breaks = c(0, 15, 30, 45, 60, 75, 90, 100)),
         # 12 mnd
         Helsetilstand_12mnd = cut(HEALTH_CONDITION_SCALE_patient12mths,
                                   breaks = c(0, 15, 30, 45, 60, 75, 90, 100)),
         # 60 mnd
         Helsetilstand_60mnd = cut(HEALTH_CONDITION_SCALE_patient60mths,
                                   breaks = c(0, 15, 30, 45, 60, 75, 90, 100)),)


# (xii) FOR KOMPLIKASJONER
  # Procedure complications as indicated by patient
regdata$PROCEDURE_COMPLICATIONS <- as.character(regdata$PROCEDURE_COMPLICATIONS)
regdata <- regdata %>%
  mutate(Komplikasjoner_3mnd = recode(PROCEDURE_COMPLICATIONS, "0" = "Nei", "1" = "Ja", "9" = "Ikke utfylt"))

return(regdata)

 }

rm(regdata)

j1 <- pre_pros(regdata)


# MAKE A FUNCTION THAT CREATES THIS DATA SET (VERY COMPLICATED!!)
k <- regdata %>%
  select(PATIENT_ID,
         Komplikasjoner_3mnd,
         COMPLICATIONS_BLEEDING,
         COMPLICATIONS_UTI,
         COMPLICATIONS_PNEUMONIA,
         COMPLICATIONS_DVT,
         COMPLICATIONS_PE,
         COMPLICATIONS_INFECTION_WOUND,
         COMPLICATIONS_INFECTION_DEEP,
         COMPLICATIONS_INFECTION_REOP,
         COMPLICATIONS_NUMBNESS,
         COMPLICATIONS_PAIN,
         COMPLICATIONS_OTHER) %>%
  mutate(Blødning = case_match(COMPLICATIONS_BLEEDING, 1 ~ "blødning", 0 ~ "0"),
         UVI = case_match(COMPLICATIONS_UTI, 1 ~ "uvi", 0 ~ "0"),
         Lunge = case_match(COMPLICATIONS_PNEUMONIA, 1 ~ "lunge", 0 ~ "0"),
         DVT = case_match(COMPLICATIONS_DVT, 1 ~ "DVT", 0 ~ "0"),
         Emboli = case_match(COMPLICATIONS_PE, 1 ~ "emboli", 0 ~ "0"),
         Inf_over = case_match(COMPLICATIONS_INFECTION_WOUND, 1 ~ "inf_over", 0 ~ "0"),
         Inf_dyp = case_match(COMPLICATIONS_INFECTION_DEEP, 1 ~ "inf_dyp", 0 ~ "0"),
         Inf_reop = case_match(COMPLICATIONS_INFECTION_REOP, 1 ~ "inf_reop", 0 ~ "0"),
         Lam = case_match(COMPLICATIONS_NUMBNESS, 1 ~"lam", 0 ~ "0"),
         Smerte = case_match(COMPLICATIONS_PAIN, 1 ~ "smerte", 0 ~ "0"),
         Annet = case_match(COMPLICATIONS_OTHER, 1 ~ "annet", 0 ~ "0"))



k <- k %>%
  select(PATIENT_ID, Blødning, UVI, Lunge, DVT, Emboli, Inf_over, Inf_dyp, Inf_reop, Lam, Smerte, Annet)

ghhg <- k %>%
  pivot_longer(!PATIENT_ID, names_to = "type", values_to = "C")

table(ghhg$type, ghhg$C)





