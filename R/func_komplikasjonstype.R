#' @title Komplikasjonstyper
#' @export


# Making a function that returns a table of complications
# Returns a dataframe

kompl_data <- function(regdata, reshid, var_kjønn, time1, time2, alder1, alder2, type_op, type_view){

# Make data set smaller and more manageageble
  kompl <- regdata %>%


    dplyr::mutate(Blødning =
                    dplyr::case_match(COMPLICATIONS_BLEEDING, 1 ~ "blødning", 0 ~ "0"),
                  UVI =
                    dplyr::case_match(COMPLICATIONS_UTI, 1 ~ "uvi", 0 ~ "0"),
                  Lunge =
                    dplyr::case_match(COMPLICATIONS_PNEUMONIA, 1 ~ "lunge", 0 ~ "0"),
                  DVT =
                    dplyr::case_match(COMPLICATIONS_DVT, 1 ~ "DVT", 0 ~ "0"),
                  Emboli =
                    dplyr::case_match(COMPLICATIONS_PE, 1 ~ "emboli", 0 ~ "0"),
                  Inf_over =
                    dplyr::case_match(COMPLICATIONS_INFECTION_WOUND, 1 ~ "inf_over", 0 ~ "0"),
                  Inf_dyp =
                    dplyr::case_match(COMPLICATIONS_INFECTION_DEEP, 1 ~ "inf_dyp", 0 ~ "0"),
                  Inf_reop =
                    dplyr::case_match(COMPLICATIONS_INFECTION_REOP, 1 ~ "inf_reop", 0 ~ "0"),
                  Lam =
                    dplyr::case_match(COMPLICATIONS_NUMBNESS, 1 ~"lam", 0 ~ "0"),
                  Smerte =
                    dplyr::case_match(COMPLICATIONS_PAIN, 1 ~ "smerte", 0 ~ "0"),
                  Annet =
                    dplyr::case_match(COMPLICATIONS_OTHER, 1 ~ "annet", 0 ~ "0"))





  # Filter to match user choices:

  ### by gender:

  kompl <- kompl %>%
    dplyr::filter(Kjønn == dplyr::case_when({{var_kjønn}} == "kvinne" ~ "kvinne",
                                            {{var_kjønn}} == "mann" ~ "mann",
                                            {{var_kjønn}} != "kvinne" | {{var_kjønn}} != "mann" ~ Kjønn)) %>%
    dplyr::mutate(Kjønn = dplyr::case_when({{var_kjønn}} == "kvinne" ~ "kvinne",
                                           {{var_kjønn}} == "mann" ~ "mann",
                                           {{var_kjønn}} != "kvinne" | {{var_kjønn}} != "mann" ~ "begge"))

  ### by operation type:

  kompl <- kompl %>%
    dplyr::filter(dplyr::case_when({{type_op}} == "Primæroperasjon" ~ CURRENT_SURGERY == 1,
                                   {{type_op}} == "Reoperasjon" ~ CURRENT_SURGERY == 2,
                                   {{type_op}} == "Begge" ~ CURRENT_SURGERY %in% c(1, 2)))

  ### by surgery date:

  kompl <- kompl %>%
    dplyr::filter(dplyr::between(SURGERY_DATE,
                                 as.Date({{time1}}),
                                 as.Date({{time2}})))

  ### by age:

  # Using column "Alder_num" in which alder is given as an integer

  kompl <- kompl %>%
    dplyr::filter(dplyr::between(Alder_num,
                                 {{alder1}},
                                 {{alder2}}))

  kompl <- kompl %>%
    dplyr::select(PID, Sykehus, CENTREID, Kjønn, Blødning, UVI, Lunge, DVT,
                  Emboli, Inf_over, Inf_dyp, Inf_reop, Lam, Smerte, Annet)


################ TIDYING AND COUNTING ##########################################
#------------------------------------------------------------------------------#

  # # pivot longer
  kompl <- kompl %>%
    tidyr::pivot_longer(!c(PID, Sykehus, Kjønn, CENTREID), names_to = "type", values_to = "Komplikasjonstype") %>%
    dplyr::select(-type)

  # # remove "unknown" and nas
  kompl <- kompl %>%
    dplyr::mutate(Komplikasjonstype = tidyr::replace_na(Komplikasjonstype, "ukjent")) %>%
    dplyr::filter(Komplikasjonstype != "ukjent")

  # remove columns with no complications
  kompl <- kompl %>%
    dplyr::filter(Komplikasjonstype != "0")

  # # make data frames of tables
  kompl_df <- data.frame(table(kompl$Sykehus, kompl$Komplikasjonstype, kompl$Kjønn, kompl$CENTREID))

  # # rename columns
  kompl_df <- kompl_df %>%
    dplyr::rename(Sykehus = Var1,
                  Komplikasjonstype = Var2,
                  Kjønn = Var3,
                  antall = Freq,
                  reshId = Var4)


  if(type_view == "hver enhet"){
    return (kompl_df)

  }

  if(type_view == "egen enhet"){
    kompl_df_hosp <- kompl_df %>%
      dplyr::filter(reshId == {{reshid}})

    return(kompl_df_hosp)
  }

  if(type_view == "hele landet, uten sammenligning"){
    kompl_df_all <- kompl_df %>%
      dplyr::group_by(Komplikasjonstype, Kjønn) %>%
      dplyr::mutate(Antall = sum(antall)) %>%
      dplyr::select(Komplikasjonstype, Kjønn, Antall) %>%
      dplyr::mutate(Sykehus = "Alle") %>%
      dplyr::distinct()

    return(kompl_df_all)

  }
  else{return(kompl_df)} # => hele landet med sammenligning
}

# nolint start
# test
## g <- kompl_data(regdata, 111961, "ee", "2023-01-02", "2024-10-02", 1, 20, "Primæroperasjon", "hele landet, uten sammenligning")
# nolint end


###### MAKE TABLE WITH PERCENTAGES ########

kompl_tbl <- function (data, var_kjønn, kompl_data) {

  data_based_on_UI_choices <- data %>%
    dplyr::group_by(Sykehus, Kjønn) %>%
    dplyr::tally()


  data_based_on_UI_choices <- data_based_on_UI_choices %>%
    dplyr::case_match({{var_kjønn}} != "mann" |
                        {{var_kjønn}} != "kvinne" ~ "begge",
                      {{v}})

  kompl_data <- dplyr::left_join(kompl_data, data_based_on_UI_choices)


  return(data_based_on_UI_choices)

}

##
h <- kompl_tbl(rr, g) #, "Komplikasjoner_3mnd", 111961, "begge", "2023-01-02", "2024-10-02", 1, 20, "Primæroperasjon", "hele landet, uten sammenligning")

