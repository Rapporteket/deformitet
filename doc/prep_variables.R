# Function for preparing variables for prop-figures (make_hist_gg)

# List of variables to be prepared: (i) kjønn - GENDER;
# (ii) alder - SURGERY_DATE- BIRTH_DATE;
# (iii) BMI;
# (iv) Pre-operative curve - PRE_MAIN_CURVE;
# (v) Post-operative curve - POST_MAIN_CURVE;
# (vi) liggetid - BED_DAYS_POSTOPERATVE & BED_DAYS_TOTAL;
# (vii) knivtid - KNIFE_TIME_EXACT_MIN + regne ut KNIFE_TIME_START_HOUR
# KNIFE_TIME_START_MIN - KNIFE_TIME_END_HOUR + KNIFE_TIME_END_MIN
# (viii) blodtap - PER_BLOOD_LOSS_VALUE
# (ix) SRS22 PROM-score - se totalt + for de ulike domenene: SRS22_MAIN_SCORE osv.
# (x) EQ5D_SCORE + evt. for de ulike domenene
# (xi) Helsetilstand scale - HELSETILSTAND_SCALE + i patientfollowup heter dette
# "HEALTH_CONDITION_SCALE"
# (xii) komplikasjoner - de ulike komplikasjonstypene

regdata <- deformitet::les_og_flate_ut()

deformitet_prep <- function(data, var){

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
                       labels = c("9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20"))


# (iii) FOR BMI:
# a. Make a new data frame containing all categories
  BMI <- data_frame("BMI_kategori" = c("Alvorlig undervekt\n < 16",
                                       "Undervekt\n (16-17)",
                                       "Mild undervekt\n (17-18,5)",
                                       "Normal\n (18,5-25)",
                                       "Overvekt\n (25-30)",
                                       "Moderat fedme\n, klasse I (30-35)",
                                       "Fedme, klasse II \n (35-40)",
                                       "Fedme, klasse III \n (40-50)"),
                    "BMI_kat" = c("Alvorlig undervekt",
                                  "Moderat undervekt",
                                  "Mild undervekt",
                                  "Normal",
                                  "Overvekt",
                                  "Moderat fedme, klasse I",
                                  "Fedme, klasse II",
                                  "Fedme, klasse III"))

# b. Merge the new data frame with the original
  regdata <- merge(BMI, regdata, by.x = "BMI_kat", all.x = TRUE,
                   by.y = "BMI_CATEGORY", all.y = TRUE)


# c. Re-order and factorize
  # regdata$BMI_kategori <- as.factor(regdata$BMI_kategori)

  regdata <- regdata %>%
    arrange(factor(BMI_kategori, levels = (c("Alvorlig undervekt\n < 16",
                                      "Undervekt\n (16-17)",
                                      "Mild undervekt\n (17-18,5)",
                                      "Normal\n (18,5-25)",
                                      "Overvekt\n (25-30)",
                                      "Moderat fedme\n, klasse I (30-35)",
                                      "Fedme, klasse II \n (35-40)",
                                      "Fedme, klasse III \n (40-50)"))))

# d. Deal with NAs
  # Mising NAs from BMI should be deleted
  # Missing NAs from rest of the line of BMI does not have to be deleted


  # regdata <- ifelse(var == "BMI_kategori", is.na(regdata$BMI_kategori))


# (iv) FOR PRE-OPERATIV KURVE:

  if(var == "BMI_kategori"){
    regdata$BMI_kategori = is.na()
  } ## HVORFOR FUNGERER IKKE DETTE??



# SELECT AND RETURN
# Select and return the column of interest
my_data <- regdata %>%
  select(Sykehus, Kjønn, BMI, {{var}})

return(my_data)

}


deformitet_prep(regdata, BMI_kategori)
