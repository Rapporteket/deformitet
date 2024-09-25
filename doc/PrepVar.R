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


# Valg: "Kjønn"; "Sykehus"; "Alder"; "BMI_kategori" ; "Diff_prosent_kurve"; "Kurve_pre"; "Kurve_post";

# regdata <- regdata %>% mutate(Var = ifelse(Var == "X", "sant", "usant"))

regdata <- deformitet::les_og_flate_ut()

deformitet_prep <- function(data, var){

  # (iii) BMI

  regdata <- ifelse(var == "BMI_kategori", regdata[!is.na(regdata$BMI_kategori),], regdata)

  # alternativ måte å gjør det på

  # (iv) PRE-OPERATIV KURVE
  # alternativ måte:
  # regdata <- regdata %>% filter(!is.na(PRE_MAIN_CURVE))
  regdata <- ifelse(var == "BMI_kategori", regdata[!is.na(regdata$BMI_kategori),], regdata)

  # (v) POST-OPERATIV KURVE
  # alternativ måte:
  # regdata <- regdata %>% filter(!is.na(PRE_MAIN_CURVE))
  regdata <- ifelse(var == "BMI_kategori", regdata[!is.na(regdata$BMI_kategori),], regdata)

  # CALCULATE DIFF BETWEEN PRE AND POST
  regdata <- ifelse(var == "Diff_prosent_kurve", regdata %>% mutate(diff = (((Kurve_pre - Kurve_post)/Kurve_pre)*100)), regdata)

  # Liggetid
  # NAs må ordnes

  # Kniv_tid
  # INGEN NAs

  # Blodtap
  # NAs må ordnes

  # SRS22
  # mange ulike NAs må ordnes

  # EQ5D - vet ikke enda

  # Helsetilstand
  # Div. ulike NAs må ordnes

  # Komplikasjoner
  # NAs må ordnes

# SELECT AND RETURN
# Select and return the column of interest
my_data <- regdata %>%
  select(Sykehus, Kjønn, BMI, {{var}})

return(my_data)

}


deformitet_prep(regdata, BMI_kategori)
