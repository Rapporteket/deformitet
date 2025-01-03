#' Calculate counts for kvalitetsiindikator
#'
#' @export


######### FUNCTION FOR CALCULATING COUNTS PR KVALITETSINDIKATOR ################
################################################################################

kval_count <- function(data, var, kjønn, type_op){

  ##### Other filtertings as data set grows ####################################
  # Her kan jeg legge inn andre filter - alder, tidsrom osv. etter hvert som
  # datasettet vokser

  data <- data %>%
    dplyr::filter(dplyr::case_when({{type_op}} == "Primæroperasjon" ~ CURRENT_SURGERY == 1,
                                   {{type_op}} == "Reoperasjon" ~ CURRENT_SURGERY == 2,
                                   {{type_op}} == "Begge" ~ CURRENT_SURGERY %in% c(1, 2)))

  ##### Make tiny data set with counts ###########################################

  my_tiny_data <- data %>%
    dplyr::add_tally(name = "alle") %>% # antall pasienter i datasettet
    dplyr::group_by(Sykehus) %>%
    dplyr::add_tally(name = "per_syk") %>% # antall pasienter per sykehus
    dplyr::ungroup() %>%
    dplyr::group_by(Sykehus, Kjønn) %>%
    dplyr::add_tally(name = "per_syk_kjønn") %>% # antall pasienter per sykehus per kjønn
    dplyr::ungroup() %>%
    dplyr::select(Sykehus, Kjønn, alle, per_syk, per_syk_kjønn) %>%
    dplyr::distinct()


  ###### Filter based on kvalitetsindikatorer ####################################

  kval <- data %>%
    dplyr::filter(dplyr::case_when({{var}} == "PRE_MAIN_CURVE" ~
                                     PRE_MAIN_CURVE > 70,
                                   {{var}} == "Komplikasjoner_3mnd" ~
                                     Komplikasjoner_3mnd == "Ja",
                                   {{var}} == "Liggetid" ~
                                     Liggetid == "> 7"| Liggetid == "7",
                                   {{var}} == "SRS22_spm22_3mnd" ~
                                     SRS22_spm22_3mnd == "Definitivt ja" |
                                     SRS22_spm22_3mnd == "Sannsynligvis ja",
                                   {{var}} == "SRS22_spm21_3mnd" ~
                                     SRS22_spm21_3mnd == "Ganske fornøyd" |
                                     SRS22_spm21_3mnd == "Svært godt fornøyd",
                                   TRUE ~
                                     CURRENT_SURGERY == 1 | CURRENT_SURGERY == 2)) %>%
    dplyr::group_by(Sykehus, Kjønn) %>%
    dplyr::count(name = "antall_kval_syk_kjønn") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Sykehus) %>%
    dplyr::mutate(antall_kval_syk = sum(antall_kval_syk_kjønn))


  ##### Join data with counts based on kvalitetsindikator ########################
  ###### with data based on whole data set #######################################

  jak <- dplyr::left_join(my_tiny_data, kval)


  ######################### Calculate andeler ####################################

  magnus <- jak %>%
    dplyr::mutate(
      andel_per_syk_kjønn =
        round(antall_kval_syk_kjønn/per_syk_kjønn*100, 2),
      andel_per_syk =
        round(antall_kval_syk/per_syk*100, 2))


  ######### Pivot data to get gender like so: F, M, aggregated over both #########
  ## Give the same name to all columns
  andr <- magnus %>%
    dplyr::select(-c(per_syk, antall_kval_syk, andel_per_syk)) %>%
    dplyr::rename(per_syk = per_syk_kjønn,
                  antall_kval_syk = antall_kval_syk_kjønn,
                  andel_per_syk = andel_per_syk_kjønn)

  ## Select only columns relevant for "both" and add "both" to "Kjønn" #########
  eas <- magnus %>%
    dplyr::select(Sykehus, alle, per_syk, antall_kval_syk, andel_per_syk) %>%
    dplyr::mutate(Kjønn = "begge") %>%
    dplyr::distinct()

  ## Join the one with only F and M with both ##################################
  thode <- dplyr::full_join(andr, eas)

  if(kjønn == "begge"){
    thode <- thode %>%
      dplyr::filter(Kjønn == "begge")
  }
  else{
    if(kjønn == "kvinne"){
      thode <- thode %>%
        dplyr::filter(Kjønn == "kvinne")
    }
    else{
      if(kjønn == "mann"){
        thode <- thode %>%
          dplyr::filter(Kjønn == "mann")
      }
    }
  }


  ############# RETURN a data set with Kjønn: F, M, Both with rate and counts ####

  return(thode)
}

# testing:
## kval <-  kval_count(regdata, "PRE_MAIN_CURVE", "jj", "Begge")

