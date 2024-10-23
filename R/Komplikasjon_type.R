#' @title Komplikasjonstyper
#'
#'
#' @export


# Making a function that returns a table of complications



kompl_data <- function(regdata, reshID){

  kompl <- regdata %>%
    dplyr::select(PATIENT_ID,
           Sykehus,
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
    dplyr::mutate(Blødning = case_match(COMPLICATIONS_BLEEDING, 1 ~ "blødning", 0 ~ "0"),
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



  kompl <- kompl %>%
    dplyr::select(PATIENT_ID, Sykehus, Blødning, UVI, Lunge, DVT, Emboli, Inf_over, Inf_dyp, Inf_reop, Lam, Smerte, Annet)

  kompl <- kompl %>%
    tidyr::pivot_longer(!c(PATIENT_ID, Sykehus), names_to = "type", values_to = "Komplikasjonstype") %>%
    dplyr::select(-type)

  kompl <- kompl %>%
    dplyr::mutate(Komplikasjonstype = replace_na(Komplikasjonstype, "ukjent")) %>%
    dplyr::filter(Komplikasjonstype != "ukjent")

  kompl <- kompl %>%
    dplyr::filter(Komplikasjonstype != "0")

  # %>%
  #   mutate(Komplikasjonstype = recode(Komplikasjonstype, "0" = "ingen"))

  kompl_df <- data.frame(table(kompl$Sykehus, kompl$Komplikasjonstype))

  # kompl_df <- kompl_df %>%
  #   group_by(kompl$Sykehus, kompl$Komplikasjonstype, .drop=FALSE) %>%
  #   tally(name = "Antall")

  kompl_df <- kompl_df %>%
    dplyr::rename(Sykehus = Var1,
           Komplikasjonstype = Var2,
           antall = Freq)

  kompl_df <- kompl_df %>%
    dplyr::mutate(Sykehus  = dplyr::case_when(as_name(reshID) == "Bergen" ~ dplyr::recode(Sykehus, "Riksen" = "Resten", "St.Olav" = "Resten"),
                                rlang::as_name(reshID) == "Riksen" ~ dplyr::recode(Sykehus, "Bergen" = "Resten", "St.Olav" = "Resten"),
                                rlang::as_name(reshID) == "St.Olav" ~ dplyr::recode(Sykehus, "Bergen" = "Resten", "Riksen" = "Resten"),
                                TRUE ~ Sykehus))

  kompl_df <- kompl_df %>%
    tidyr::pivot_wider(names_from = Sykehus, values_from = antall, values_fn = list)


  kompl_df <- kompl_df %>%
    tidyr::unnest_wider(Resten, names_sep = ".") %>%
    tidyr::unnest_wider(dplyr::case_when(rlang::as_name(reshID) == "Bergen" ~ "Bergen",
                                         rlang::as_name(reshID) == "Riksen" ~ "Riksen",
                                         rlang::as_name(reshID) == "St.Olav" ~ "St.Olav"), names_sep = "") %>%
    dplyr::mutate(Resten = Resten.1+Resten.2) %>%
    dplyr::select(-Resten.1, -Resten.2)


  ####### IKKE FERDIG HER ##########


    tidyr::pivot_longer(!Komplikasjonstype, names_to = "Sykehus", values_to = "Antall")

  # kompl_df <- kompl_df %>%
  #   group_by(Sykehus, .drop=FALSE) %>%
  #   mutate(Antall_pr_Sykehus = sum(Antall)) %>%
  #   group_by(Sykehus, Komplikasjonstype, .drop=FALSE) %>%
  #   mutate(Andel = Antall/Antall_pr_Sykehus,
  #          Prosent = Andel*100)
  #
  # kompl_df$Sykehus <- sub("1", "", kompl_df$Sykehus)


  return(kompl_df)
}

