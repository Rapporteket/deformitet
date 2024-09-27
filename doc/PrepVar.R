# Function for preparing variables for prop-figures (make_hist_gg)

# Valg: "Kjønn"; "Sykehus"; "Alder"; "BMI_kategori" ; "Diff_prosent_kurve"; "Kurve_pre"; "Kurve_post";

# regdata <- regdata %>% mutate(Var = ifelse(Var == "X", "sant", "usant"))

regdata <- pre_pros(regdata)


prep <- function(regdata, var){
  var <- dplyr::enquo(var)

  # (iii) BMI
gg_data <- data.frame(title = "")

# kan legge inn i df de der jeg skal fjerne NA (Y) og så kan jeg først sjekke det og så ta bort NA


regdata <- regdata %>%
    filter(!is.na({{var}}))


gg_data <- gg_data %>%

  #### Legge inn tittle på plot ####

  mutate(title = case_when(as_name(var) == "BMI_kategori" ~ "Andel operasjoner fordelt på BMI-kategorier",

                           # ALDER:
                           as_name(var) == "Alder" ~ "Andel operasjoner fordelt på aldersgrupper",

                           # KURVE:
                           as_name(var) == "Kurve_pre" ~ "Andel operasjoner fordel på pre-operativ kurve",
                           as_name(var) == "Kurve_post" ~ "Andel operasjoner fordelt på post-operativ kurve",
                           as_name(var) == "Diff_prosent_kurve" ~ "Andel operasjoner fordelt på prosentvis korreksjon i kurve",

                           # LIGGETID
                           as_name(var) == "Liggetid" ~ "Andel operasjoner fordelt på liggetid etter operasjon",

                           # KNIVTID
                           as_name(var) == "Knivtid" ~ "Andel operasjoner fordelt på knivtid",

                           # BLODTAP:
                           as_name(var) == "Blodtap_100" ~ "Andel operasjoner fordelt på blodtap",
                           as_name(var) == "Blodtap_200" ~ "Andel operasjoner fordelt på blodtap",

                           # SRS22:total
                           as_name(var) == "SRS22_total" ~ "Andel operasjoner fordelt på total SRS22 skår (1-5) ved innleggelse",

                           # SRS22: funksjon
                           as_name(var) == "SRS22_funksjon" ~ "Andel operasjoner fordelt på SRS22-funksjonsskår (1-5) ved innleggelse",
                           as_name(var) == "SRS22_funksjon_3mnd" ~ "Andel operasjoner fordelt på SRS22-funksjonsskår (1-5) ved 3-6 måneders oppfølging",
                           as_name(var) == "SRS22_funksjon_12mnd" ~ "Andel operasjoner fordelt på SRS22-funksjonsskår (1-5) ved 12 måneders oppfølging",
                           as_name(var) == "SRS22_funksjon_60mnd" ~ "Andel operasjoner fordelt på SRS22-funksjonsskår (1-5) ved 5 års oppfølging",

                           # SRS22: smerte
                           as_name(var) == "SRS22_smerte" ~ "Andel operasjoner fordelt på SRS22-smertesskår (1-5) ved innleggelse",
                           as_name(var) == "SRS22_smerte_3mnd" ~ "Andel operasjoner fordelt på SRS22-smertesskår (1-5) ved 3-6 måneders oppfølging",
                           as_name(var) == "SRS22_smerte_12mnd" ~ "Andel operasjoner fordelt på SRS22-smertesskår (1-5) ved 12 måneders oppfølging",
                           as_name(var) == "SRS22_smerte_60mnd" ~ "Andel operasjoner fordelt på SRS22-smertesskår (1-5) ved 5 års oppfølging",

                           # SRS22: selvbilde
                           as_name(var) == "SRS22_selvbilde" ~ "Andel operasjoner fordelt på SRS22-selvbildesskår (1-5) ved innleggelse",
                           as_name(var) == "SRS22_selvbilde_3mnd" ~ "Andel operasjoner fordelt på SRS22-selvbildesskår (1-5) ved 3-6 måneders oppfølging",
                           as_name(var) == "SRS22_selvbilde_12mnd" ~ "Andel operasjoner fordelt på SRS22-selvbildesskår (1-5) ved 12 måneders oppfølging",
                           as_name(var) == "SRS22_selvbilde_60mnd" ~ "Andel operasjoner fordelt på SRS22-selvbildesskår (1-5) ved 5 års oppfølging",

                           # SRS22: mental helse
                           as_name(var) == "SRS22_mhelse" ~ "Andel operasjoner fordelt på SRS22-mental-helse-skår (1-5) ved innleggelse",
                           as_name(var) == "SRS22_mhelse_3mnd" ~ "Andel operasjoner fordelt på SRS22-mental-helse-skår (1-5) ved 3-6 måneders oppfølging",
                           as_name(var) == "SRS22_mhelse_12mnd" ~ "Andel operasjoner fordelt på SRS22-mental-helse-skår (1-5) ved 12 måneders oppfølging",
                           as_name(var) == "SRS22_mhelse_60mnd" ~ "Andel operasjoner fordelt på SRS22-mental-helse-skår (1-5) ved 5 års oppfølging",

                           # SRS22: fornøyd
                           as_name(var) == "SRS22_fornoyd_3mnd" ~ "Andel operasjoner fordelt på SRS22-fornøydhetsskår (1-5) ved 3-6 måneders oppfølging",
                           as_name(var) == "SRS22_fornoyd_12mnd" ~ "Andel operasjoner fordelt på SRS22-fornøydhetsskår (1-5) ved 12 måneders oppfølging",
                           as_name(var) == "SRS22_fornoyd_60mnd" ~ "Andel operasjoner fordelt på SRS22-fornøydhetsskår (1-5) ved 5 års oppfølging",

                           # SRS22: spm 21 - hvor fornøyd?
                           as_name(var) == "SRS22_spm21_3mnd" ~ "Andel operasjoner fordelt på spørsmålet: 'Er du fornøyd med resultatet av behandlingen?' ved 3-6 måneders oppfølging",
                           as_name(var) == "SRS22_spm21_12mnd" ~ "Andel operasjoner fordelt på spørsmålet: 'Er du fornøyd med resultatet av behandlingen?' ved 12 måneders oppfølging",
                           as_name(var) == "SRS22_spm21_60mnd" ~ "Andel operasjoner fordelt på spørsmålet: 'Er du fornøyd med resultatet av behandlingen?' ved 5 års oppfølging",


                           # SRS22: spm 22 - på nytt?
                           as_name(var) == "SRS22_spm22_3mnd" ~ "Andel operasjoner fordelt på spørsmålet: 'Ville du ønsket samme behandling på nytt?' ved 3-6 måneders oppfølging",
                           as_name(var) == "SRS22_spm22_12mnd" ~ "Andel operasjoner fordelt på spørsmålet: 'Ville du ønsket samme behandling på nytt?' ved 12 måneders oppfølging",
                           as_name(var) == "SRS22_spm22_60mnd" ~ "Andel operasjoner fordelt på spørsmålet: 'Ville du ønsket samme behandling på nytt?' ved 5 års oppfølging",


                           # EQ5D


                           # HELSETILSTAND
                           as_name(var) == "Helsetilstand" ~ "Andel operasjoner fordelt på helsetilstandsskår (0-100) ved innleggelse",
                           as_name(var) == "Helsetilstand_3mnd" ~ "Andel operasjoner fordelt på helsetilstandsskår (1-5) ved 3-6 måneders oppfølging",
                           as_name(var) == "Helsetilstand_12mnd" ~ "Andel operasjoner fordelt på helsetilstandsskår (1-5) ved 12 måneders oppfølging",
                           as_name(var) == "Helsetilstand_60mnd" ~ "Andel operasjoner fordelt på helsetilstandsskår (1-5) ved 5 års oppfølging",
                           ),

         #### Legge inn tittel på lab ####
         xlab = case_when(as_name(var) == "BMI_kategori" ~ "BMI-kategorier",

                           # ALDER:
                           as_name(var) == "Alder" ~ "Aldersgrupper",

                           # KURVE:
                           as_name(var) == "Kurve_pre" ~ "Pre-operativ kurve",
                           as_name(var) == "Kurve_post" ~ "Post-operativ kurve",
                           as_name(var) == "Diff_kurve_prosent" ~ "Post-operativ prosent korreksjon",

                           # LIGGETID
                           as_name(var) == "Liggetid" ~ "Liggetid etter operasjon, oppgitt i dager",

                           # KNIVTID
                           as_name(var) == "Knivtid" ~ "Knivtid, oppgitt i minutter",

                           # BLODTAP:
                           as_name(var) == "Blodtap_100" ~ "Blodtap pr 100ml",
                           as_name(var) == "Blodtap_200" ~ "Blodtap pr 200ml",

                           # SRS22:total
                           as_name(var) == "SRS22_total" ~ "Total SRS22 skår (1-5) ved innleggelse",

                           # SRS22: funksjon
                           as_name(var) == "SRS22_funksjon" ~ "SRS22-funksjonsskår (1-5), innleggelse",
                           as_name(var) == "SRS22_funksjon_3mnd" ~ "SRS22-funksjonsskår (1-5), 3-6 måneders oppfølging",
                           as_name(var) == "SRS22_funksjon_12mnd" ~ "SRS22-funksjonsskår (1-5), 12 måneders oppfølging",
                           as_name(var) == "SRS22_funksjon_60mnd" ~ "SRS22-funksjonsskår (1-5), 5 års oppfølging",

                           # SRS22: smerte
                           as_name(var) == "SRS22_smerte" ~ "SRS22-smertesskår (1-5), innleggelse",
                           as_name(var) == "SRS22_smerte_3mnd" ~ "SRS22-smertesskår (1-5), 3-6 måneders oppfølging",
                           as_name(var) == "SRS22_smerte_12mnd" ~ "SRS22-smertesskår (1-5), 12 måneders oppfølging",
                           as_name(var) == "SRS22_smerte_60mnd" ~ "SRS22-smertesskår (1-5), 5 års oppfølging",

                           # SRS22: selvbilde
                           as_name(var) == "SRS22_selvbilde" ~ "SRS22-selvbildesskår (1-5), innleggelse",
                           as_name(var) == "SRS22_selvbilde_3mnd" ~ "SRS22-selvbildesskår (1-5), 3-6 måneders oppfølging",
                           as_name(var) == "SRS22_selvbilde_12mnd" ~ "SRS22-selvbildesskår (1-5), 12 måneders oppfølging",
                           as_name(var) == "SRS22_selvbilde_60mnd" ~ "SRS22-selvbildesskår (1-5), 5 års oppfølging",

                           # SRS22: mental helse
                           as_name(var) == "SRS22_mhelse" ~ "SRS22-mental-helse-skår (1-5), innleggelse",
                           as_name(var) == "SRS22_mhelse_3mnd" ~ "SRS22-mental-helse-skår (1-5), 3-6 måneders oppfølging",
                           as_name(var) == "SRS22_mhelse_12mnd" ~ "SRS22-mental-helse-skår (1-5), 12 måneders oppfølging",
                           as_name(var) == "SRS22_mhelse_60mnd" ~ "SRS22-mental-helse-skår (1-5), 5 års oppfølging",

                           # SRS22: fornøyd
                           as_name(var) == "SRS22_fornoyd_3mnd" ~ "SRS22-fornøydhetsskår (1-5), 3-6 måneders oppfølging",
                           as_name(var) == "SRS22_fornoyd_12mnd" ~ "SRS22-fornøydhetsskår (1-5), 12 måneders oppfølging",
                           as_name(var) == "SRS22_fornoyd_60mnd" ~ "SRS22-fornøydhetsskår (1-5), 5 års oppfølging",

                           # SRS22: spm 21 - hvor fornøyd?
                           as_name(var) == "SRS22_spm21_3mnd" ~ "'Er du fornøyd med resultatet av behandlingen?', 3-6 måneders oppfølging",
                           as_name(var) == "SRS22_spm21_12mnd" ~ "'Er du fornøyd med resultatet av behandlingen?', 12 måneders oppfølging",
                           as_name(var) == "SRS22_spm21_60mnd" ~ "'Er du fornøyd med resultatet av behandlingen?', 5 år oppfølging",


                           # SRS22: spm 22 - på nytt?
                           as_name(var) == "SRS22_spm22_3mnd" ~ "'Ville du ønsket samme behandling på nytt?', 3-6 måneders oppfølging",
                           as_name(var) == "SRS22_spm22_12mnd" ~ "'Ville du ønsket samme behandling på nytt?', 12 måneders oppfølging",
                           as_name(var) == "SRS22_spm22_60mnd" ~ "'Ville du ønsket samme behandling på nytt?', 5 års oppfølging",


                           # EQ5D


                           # HELSETILSTAND
                           as_name(var) == "Helsetilstand" ~ "Helsetilstandsskår (0-100), innleggelse",
                           as_name(var) == "Helsetilstand_3mnd" ~ "Helsetilstandsskår (1-5), 3-6 måneders oppfølging",
                           as_name(var) == "Helsetilstand_12mnd" ~ "Helsetilstandsskår (1-5), 12 måneders oppfølging",
                           as_name(var) == "Helsetilstand_60mnd" ~ "Helsetilstandsskår (1-5), 5 års oppfølging",
         ))



# SELECT AND RETURN
# Select and return the column of interest


my_data <- regdata %>%
  select(Sykehus, Kjønn, {{var}})

return(list(my_data, gg_data))

}


res= prep(regdata, Kurve_pre)




