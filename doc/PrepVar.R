# Function for preparing variables for prop-figures (make_hist_gg)

# Valg: "Kjønn"; "Sykehus"; "Alder"; "BMI_kategori" ; "Diff_prosent_kurve"; "Kurve_pre"; "Kurve_post";

# regdata <- regdata %>% mutate(Var = ifelse(Var == "X", "sant", "usant"))

regdata <- pre_pros(regdata)


deformitet_prep <- function(regdata, var){
  var <- dplyr::enquo(var)

  # (iii) BMI
gg_data <- data.frame(title = "")

# kan legge inn i df de der jeg skal fjerne NA (Y) og så kan jeg først sjekke det og så ta bort NA


regdata <- regdata %>%
    filter(!is.na({{var}}))

gg_data <- gg_data %>%
  mutate(title = ifelse(as_name(var) == "BMI_kategori", "Andel", "jfdsk"))



  # gg_data <- gg_data %>%
  #   mutate(title = ifelse({{var}} := "BMI_kategori", "Andel operasjoner fordelt på BMI-kategorier", "everything"))

  # # alternativ måte å gjør det på
  #
  # # (iv) PRE-OPERATIV KURVE
  # # alternativ måte:
  # # regdata <- regdata %>% filter(!is.na(PRE_MAIN_CURVE))
  # regdata <- ifelse(var == "BMI_kategori", regdata[!is.na(regdata$BMI_kategori),], regdata)
  #
  # # (v) POST-OPERATIV KURVE
  # # alternativ måte:
  # # regdata <- regdata %>% filter(!is.na(PRE_MAIN_CURVE))
  # regdata <- ifelse(var == "BMI_kategori", regdata[!is.na(regdata$BMI_kategori),], regdata)
  #
  # # CALCULATE DIFF BETWEEN PRE AND POST
  # regdata <- ifelse(var == "Diff_prosent_kurve", regdata %>% mutate(diff = (((Kurve_pre - Kurve_post)/Kurve_pre)*100)), regdata)
  #
  # # Liggetid
  # # NAs må ordnes

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

return(list(my_data, gg_data))

}


res= deformitet_prep(regdata, BMI_kategori)



rm(regdata)
