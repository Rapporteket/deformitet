#' @title Komplikasjonstyper
#' @export


# Making a function that returns a table of complications

####### MÅ LEGGE INN NOE FUNKSJONALITET HER PÅ KJØNN #####################

kompl_data <- function(regdata, Sykehus, var_kjønn, time1, time2, alder1, alder2, type_op){

# Make data set smaller and more manageageble
  kompl <- regdata %>%
    # dplyr::select(PATIENT_ID,
    #               Sykehus,
    #               Kjønn,
    #               Alder_num,
    #               SURGERY_DATE,
    #               CURRENT_SURGERY,
    #               Komplikasjoner_3mnd,
    #               COMPLICATIONS_BLEEDING,
    #               COMPLICATIONS_UTI,
    #               COMPLICATIONS_PNEUMONIA,
    #               COMPLICATIONS_DVT,
    #               COMPLICATIONS_PE,
    #               COMPLICATIONS_INFECTION_WOUND,
    #               COMPLICATIONS_INFECTION_DEEP,
    #               COMPLICATIONS_INFECTION_REOP,
    #               COMPLICATIONS_NUMBNESS,
    #               COMPLICATIONS_PAIN,
    #               COMPLICATIONS_OTHER) %>%

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
                                            {{var_kjønn}} != "kvinne" | {{var_kjønn}} != "mann" ~ Kjønn))

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
    dplyr::select(PATIENT_ID, Sykehus, Kjønn, Blødning, UVI, Lunge, DVT,
                  Emboli, Inf_over, Inf_dyp, Inf_reop, Lam, Smerte, Annet)


################ TIDYING AND COUNTING ##########################################
#------------------------------------------------------------------------------#

  # # pivot longer
  kompl <- kompl %>%
    tidyr::pivot_longer(!c(PATIENT_ID, Sykehus, Kjønn), names_to = "type", values_to = "Komplikasjonstype") %>%
    dplyr::select(-type)

  # # remove "unknown" and nas
  kompl <- kompl %>%
    dplyr::mutate(Komplikasjonstype = tidyr::replace_na(Komplikasjonstype, "ukjent")) %>%
    dplyr::filter(Komplikasjonstype != "ukjent")

  # remove columns with no complications
  kompl <- kompl %>%
    dplyr::filter(Komplikasjonstype != "0")

  # # make data frames of tables
  kompl_df <- data.frame(table(kompl$Sykehus, kompl$Komplikasjonstype, kompl$Kjønn))

  # # rename columns
  kompl_df <- kompl_df %>%
    dplyr::rename(Sykehus = Var1,
                  Komplikasjonstype = Var2,
                  Kjønn = Var3,
                  antall = Freq)



  if(var_kjønn != "kvinne" | var_kjønn != "mann"){

        kompl_df <- kompl_df %>%
          dplyr::mutate(Kjønn = "begge")
        }



  # # make me vs. the rest stats
  # kompl_df <- kompl_df %>%
  #   dplyr::mutate(Sykehus  = dplyr::case_when({{Sykehus}} == "Bergen" ~
  #                                               dplyr::recode(Sykehus,
  #                                                             "Riksen" = "Resten",
  #                                                             "St.Olav" = "Resten"),
  #                                             {{Sykehus}} == "Riksen" ~
  #                                               dplyr::recode(Sykehus,
  #                                                             "Bergen" = "Resten",
  #                                                             "St.Olav" = "Resten"),
  #                                             {{Sykehus}} == "St.Olav" ~
  #                                               dplyr::recode(Sykehus,
  #                                                             "Bergen" = "Resten",
  #                                                             "Riksen" = "Resten"),
  #                                             TRUE ~ Sykehus))
  #
  # # pivot wider again
  # kompl_df <- kompl_df %>%
  #   tidyr::pivot_wider(names_from = Sykehus, values_from = antall, values_fn = list)
  #
  # # for the hospitals name
  # if(Sykehus %in% c("Bergen", "Riksen", "St.Olav")){
  #   kompl_df <- kompl_df %>%
  #     tidyr::unnest_wider(Resten, names_sep = ".") %>%
  #     tidyr::unnest_wider(dplyr::case_when({{Sykehus}} == "Bergen" ~ "Bergen",
  #                                          {{Sykehus}} == "Riksen" ~ "Riksen",
  #                                          {{Sykehus}} == "St.Olav" ~ "St.Olav"), names_sep = "") %>%
  #     dplyr::mutate(Resten = Resten.1+Resten.2) %>%
  #     dplyr::select(-Resten.1, -Resten.2)
  #
  #
  #
  #
  #   kompl_df$Sykehus <- sub("1", "", kompl_df$Sykehus)
  #
  #   y <- kompl_df %>%
  #     dplyr::group_by(Sykehus, .drop=FALSE) %>%
  #     dplyr::mutate(antall_pr_Sykehus = sum(antall)) %>%
  #     dplyr::group_by(Sykehus, Komplikasjonstype, .drop=FALSE) %>%
  #     dplyr::mutate(andel = round(antall/antall_pr_Sykehus, 4),
  #            prosent = round(andel*100,2))
  #
  #
  #   y$Sykehus <- sub("1", "", y$Sykehus)
  #
  # }
  # else{
  # x <- kompl_df %>%
  #   tidyr::pivot_longer(!Komplikasjonstype, names_to = "Sykehus", values_to = "antall")
  #
  # x$Antall <- as.numeric(x$antall)
  #
  # y <- x %>%
  #   group_by(Sykehus, .drop=FALSE) %>%
  #   mutate(antall_pr_Sykehus = sum(antall)) %>%
  #   group_by(Sykehus, Komplikasjonstype, .drop=FALSE) %>%
  #   mutate(andel = round(antall/antall_pr_Sykehus, 4),
  #          prosent = round(andel*100, 2))
  #
  # }
  #
  # g <- y %>%
  #   dplyr::relocate(Sykehus, .before = Komplikasjonstype)

  return(kompl_df)
}

# test
## g <- kompl_data(regdata, "Bergen", "mann", "2023-01-02", "2024-10-02", 1, 20, "Primæroperasjon")

