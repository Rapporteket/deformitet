## GJENNOMSNITT OVER TID ##

# Funksjon for å regne gjennomsnitt pr. kvartal, pr. sykehus og nasjonalt
### Siden dette er en kontinuerlig variabel:
### Mapping kjører først
### Deretter kjører prep_var med mapping som input
### Deretter kjører table_freq_time med mapping som input

# nolint start
# Nødvendig data for å teste funksjonene i over_tid
#map_var <- deformitet::mapping_old_name_new_name(regdata, "SRS22_total_3mnd")
#prep_data <- prepVar(regdata, map_var, "mm", "2024-01-01", "2025-01-01", 1, 20, "Begge", "over_tid")
#prep_data_var <- data.frame(prep_data[1])
#gg_data <- data.frame(prep_data[2])
# nolint end

#' @title Tabell - gjennomsnitt over tid
#' @export

table_freq_time <- function(data,
                            var,
                            map_data,
                            tidsenhet = "kvartal",
                            visning = "hele landet",
                            userUnitId) {
  data$PID <- as.character(data$PID)

  if (tidsenhet == "kvartal") {
    data <- data %>%
      dplyr::rename(
        mine = {{ var }},
        date = 6
      ) %>%
      dplyr::mutate(
        quarter = lubridate::floor_date(date, unit = "quarter"),
        tid = lubridate::ymd(.data$quarter)
      )
  } else {
    data <- data %>%
      dplyr::rename(
        mine = {{ var }},
        date = 6
      ) %>%
      dplyr::mutate(
        aar = lubridate::floor_date(date, unit = "year"),
        tid = format(.data$aar, "%Y")
      )
  }

  ## Pr. sykehus

  data_sykehus <- data %>%
    dplyr::filter(!is.na(.data$mine)) %>%
    dplyr::group_by(.data$Sykehus, .data$tid) %>%
    dplyr::summarize(gjen = mean(.data$mine)) %>%
    dplyr::select(c(.data$Sykehus, .data$tid, .data$gjen))

  data_tally <- data %>%
    dplyr::group_by(.data$Sykehus, .data$tid) %>%
    dplyr::add_tally(n = "antall") %>%
    dplyr::select(.data$Sykehus, .data$tid, .data$antall) %>%
    unique()

  data_sykehus <- dplyr::left_join(data_sykehus, data_tally)

  ## Nasjonalt

  data_nasjonalt <- data %>%
    dplyr::filter(!is.na(.data$mine)) %>%
    dplyr::select(-.data$Sykehus) %>%
    dplyr::mutate(Sykehus = "Nasjonalt") %>%
    dplyr::group_by(.data$Sykehus, .data$tid) %>%
    dplyr::summarize(gjen = mean(.data$mine)) %>%
    dplyr::select(c(.data$Sykehus, .data$tid, .data$gjen))

  data_nasjonalt_tally <- data %>%
    dplyr::select(-.data$Sykehus) %>%
    dplyr::mutate(Sykehus = "Nasjonalt") %>%
    dplyr::group_by(.data$Sykehus, .data$tid) %>%
    dplyr::add_tally(n = "antall") %>%
    dplyr::select(.data$Sykehus, .data$tid, .data$antall) %>%
    unique()

  data_nasjonalt <- dplyr::left_join(data_nasjonalt, data_nasjonalt_tally)


  data <- rbind(data_sykehus, data_nasjonalt) # Bind disse to sammen

  map_data <- map_data %>%
    dplyr::add_row(UnitId = "0", orgname = "Nasjonalt")

  data <- merge(data, map_data, by.x = "Sykehus", by.y = "orgname")

  data <- data %>%
    dplyr::filter(dplyr::case_when(
      {{ visning }} == "hele landet, uten sammenligning" ~
        Sykehus == "Nasjonalt",
      {{ visning }} == "egen enhet" ~
        UnitId == {{ userUnitId }},
      {{ visning }} == "hver enhet" ~
        Sykehus != "Nasjonalt",
      .default = .data$Sykehus == .data$Sykehus
    ))

  data <- data %>%
    dplyr::select(-c(.data$UnitId))


  ##### GJØR DET MULIG Å PRINTE KVARTAL PENT #####

  if (tidsenhet == "kvartal") {
    data <- data %>%
      mutate(tid1 = lubridate::year(tid),
             tid_as_character = as.character(tid),
             tid = case_when(str_detect(tid, "01-01") == TRUE ~ paste(tid1, "1", sep = "-"),
                             str_detect(tid, "04-01") == TRUE ~ paste(tid1, "2", sep = "-"),
                             str_detect(tid, "07-01") == TRUE ~ paste(tid1, "3", sep = "-"),
                             str_detect(tid, "10-01") == TRUE ~ paste(tid1, "4", sep = "-")))


    data$tid <- as.factor(data$tid)

    data <- data %>%
      select(-c(tid1, tid_as_character))

  }




  return(data)
}

# nolint start
## Test:
###t <- table_freq_time(prep_data_var, map_var, map_db_resh, "kvartal", "hver enhet", 111961)
# nolint end


# Funksjon for å lage plot

#' @title Plot - gjennomsnitt over tid
#' @export

over_tid_plot <- function(data, # data som kommer fra funksjonen table_freq_time()
                          visning, # valgt visning -> hele landet, egen enhet osv.
                          gg_data, # data med limits på y-aksen
                          map_var, # data som mapper mellom variabel med faktornivå og kontinuerlig variabel
                          tidsenhet, # valgt tidsenhet -> kvartal eller år
                          data_var # data som lagrer UI-valg
                          ) {
  data$Sykehus <- as.factor(data$Sykehus)

  if (visning == "hele landet") {
    data$Sykehus <- relevel(data$Sykehus, "Nasjonalt")
  } else {
    data <- data
  }

  if (tidsenhet == "aar") {
    tid = "År"
  } else {
    tid = "Kvartal"
  }

  limits <- y_limits_gjen(map_var)

  tid_plot <-
    ggplot2::ggplot(data, ggplot2::aes(
      x = .data$tid, y = .data$gjennomsnitt,
      color = .data$Sykehus, group = .data$Sykehus
    )) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(size = 2.2)

  ## GENERELLE TILPASNINGER AV PLOT ##
  tid_plot <- tid_plot +
    ggplot2::theme_bw(base_size = 16) +


    ggplot2::xlab(tid) +
    ggplot2::ylab(gg_data$xlab) +
    ggplot2::ggtitle("Gjennomsnitt over tid") +
    ggplot2::labs(caption = paste0("**Valgte variabler:**", "\n", data_var[1,], ", Kjønn: ", data_var[2,], "\n",
                                   "Dato: ", data_var[3,], "-", data_var[4,], "\n",
                                   "Alder: ", data_var[5,], "-", data_var[6,], "\n",
                                   "Type operasjon: ", data_var[7,]))+

    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.title.y = ggplot2::element_text(size = 16),
      plot.caption = ggplot2::element_text(color = "#87189D", # add caption
                                          face = "italic")) +

    ggplot2::scale_color_manual(
      values = # adding chosen colors
        c("darkblue", "#6CACE4", "#ADDFB3", "#87189D")
    ) +

    ggplot2::ylim(limits$ymin, limits$ymax)

  return(tid_plot)
}

# nolint start
# Test for å sjekke om det fungerer:
## r <- over_tid_plot(t, "egen enhet", gg_data, map_var, "kvartal")
## r
# nolint end


# Oversikt variabler pr. skjema
skjema <- data.frame(
  pre_op_pas = c(
    "Helsetilstand",
    "HELSETILSTAND_SCALE",
    "SRS22_total",
    "SRS22_MAIN_SCORE",
    "SRS22_funksjon",
    "SRS22_FUNCTION_SCORE",
    "SRS22_smerte",
    "SRS22_PAIN_SCORE",
    "SRS22_selvbilde",
    "SRS22_SELFIMAGE_SCORE",
    "SRS22_mhelse",
    "SRS22_MENTALHEALTH_SCORE",
    "",
    "",
    "",
    "",
    "",
    ""
  ),
  pre_op_lege = c(
    "BMI_kategori",
    "BMI",
    "Kurve_pre",
    "PRE_MAIN_CURVE",
    "Kurve_post",
    "POST_MAIN_CURVE",
    "Diff_prosent_kurve_raw",
    "Liggetid",
    "BED_DAYS_POSTOPERATIVE",
    "Blodtap_100",
    "PER_BLOOD_LOSS_VALUE",
    "Blodtap_200",
    "PER_BLOOD_LOSS_VALUE",
    "Knivtid",
    "kniv_tid",
    "",
    "",
    ""
  ),
  tre_mnd_pas = c(
    "Helsetilstand_3mnd",
    "HEALTH_CONDITION_SCALE",
    "SRS22_spm22_3mnd",
    "SRS22_22",
    "SRS22_spm21_3mnd",
    "SRS22_21",
    "SRS22_total_3mnd",
    "SRS22_FULL_SCORE",
    "SRS22_funksjon_3mnd",
    "SRS22_FUNCTION_SCORE_patient3mths",
    "SRS22_smerte_3mnd",
    "SRS22_PAIN_SCORE_patient3mths",
    "SRS22_selvbilde_3mnd",
    "SRS22_SELFIMAGE_SCORE_patient3mths",
    "SRS22_mhelse_3mnd",
    "SRS22_MENTALHEALTH_SCORE_patient3mths",
    "SRS22_fornoyd_3mnd",
    "SRS22_SATISFACTION_SCORE"
  ),
  tre_mnd_lege = c(
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    ""
  ),
  tolv_mnd_pas = c(
    "Helsetilstand_12mnd",
    "HEALTH_CONDITION_SCALE_patient12mths",
    "SRS22_spm22_12mnd",
    "SRS22_22_patient12mths",
    "SRS22_spm21_12mnd",
    "SRS22_21_patient12mths",
    "SRS22_total_12mnd",
    "SRS22_FULL_SCORE_patient12mths",
    "SRS22_funksjon_12mnd",
    "SRS22_FUNCTION_SCORE_patient12mths",
    "SRS22_smerte_12mnd",
    "SRS22_PAIN_SCORE_patient12mths",
    "SRS22_selvbilde_12mnd",
    "SRS22_SELFIMAGE_SCORE_patient12mths",
    "SRS22_mhelse_12mnd",
    "SRS22_MENTALHEALTH_SCORE_patient12mths",
    "SRS22_fornoyd_12mnd",
    "SRS22_SATISFACTION_SCORE_patient12mths"
  ),
  tolv_mnd_lege = c(
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    ""
  ),
  seksti_mnd_pas = c(
    "Helsetilstand_60mnd",
    "HEALTH_CONDITION_SCALE_patient_60_mths",
    "SRS22_spm22_60mnd",
    "SRS22_22_patient60mths",
    "SRS22_spm21_60mnd",
    "SRS22_21_patient60mths",
    "SRS22_total_60mnd",
    "SRS22_FULL_SCORE_patient60mths",
    "SRS22_funksjon_60mnd",
    "SRS22_FUNCTION_SCORE_patient60mths",
    "SRS22_smerte_60mnd",
    "SRS22_PAIN_SCORE_patient60mths",
    "SRS22_selvbilde_60mnd",
    "SRS22_SELFIMAGE_SCORE_patient60mths",
    "SRS22_mhelse_60mnd",
    "SRS22_MENTALHEALTH_SCORE_patient60mths",
    "SRS22_fornoyd_60mnd",
    "SRS22_SATISFACTION_SCORE_patient60mths"
  ),
  andre = c(
    "Alder",
    "Alder_num",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    ""
  )
)

usethis::use_data(skjema, overwrite = TRUE)


####### Function to set appropriate limits on y-axis ##############
#' @title Y-axis limits
#' @export

y_limits_gjen <- function(var) {
  data <- data.frame(ymin = "", ymax = "")
  data <- data %>%
    dplyr::mutate(
      ymin = dplyr::case_when(
        stringr::str_detect(var, "SRS22") == TRUE ~ 1,
        stringr::str_detect(var, "SCALE") == TRUE ~ 0,
        stringr::str_detect(var, "Alder") == TRUE ~ 5,
        stringr::str_detect(var, "BMI") == TRUE ~ 10,
        stringr::str_detect(var, "PRE_MAIN_CURVE") == TRUE ~ 20,
        stringr::str_detect(var, "POST_MAIN_CURVE") == TRUE ~ 0,
        stringr::str_detect(var, "kurve") == TRUE ~ 0,
        stringr::str_detect(var, "tid") == TRUE ~ 0,
        stringr::str_detect(var, "BED") == TRUE ~ 0,
        stringr::str_detect(var, "BLOOD") == TRUE ~ 100
      ),
      ymax = dplyr::case_when(
        stringr::str_detect(var, "SRS22") == TRUE ~ 5,
        stringr::str_detect(var, "SCALE") == TRUE ~ 100,
        stringr::str_detect(var, "Alder") == TRUE ~ 80,
        stringr::str_detect(var, "BMI") == TRUE ~ 35,
        stringr::str_detect(var, "PRE_MAIN_CURVE") == TRUE ~ 110,
        stringr::str_detect(var, "POST_MAIN_CURVE") == TRUE ~ 50,
        stringr::str_detect(var, "kurve") == TRUE ~ 110,
        stringr::str_detect(var, "tid") == TRUE ~ 600,
        stringr::str_detect(var, "BED") == TRUE ~ 12,
        stringr::str_detect(var, "BLOOD") == TRUE ~ 1000
      )
    )
}

# nolint start
# Test to see if it works:
## rr <- r("PER_BLOOD_LOSS_VALUE")
# nolint end

# Funksjon for å sjekke størrelse

sjekk_antall <- function(data, data1, date1, date2, tidsenhet) {
  if (tidsenhet == "kvartal") {
    true_data <- data %>%
      dplyr::filter(dplyr::between(.data$SURGERY_DATE, as.Date(date1), as.Date(date2))) %>%
      dplyr::mutate(quarter = lubridate::floor_date(.data$SURGERY_DATE, unit = "quarter")) %>%
      dplyr::select(.data$quarter) %>%
      unique() %>%
      dplyr::add_tally(n = "n_quarter") %>%
      dplyr::select(.data$quarter, .data$n_quarter)

    true_quarter <- true_data$n_quarter[1]

    sample_data <- data1 %>%
      dplyr::group_by(.data$Sykehus) %>%
      dplyr::add_tally(n = "n_quarter") %>%
      dplyr::mutate(check = dplyr::if_else(.data$n_quarter == true_quarter, TRUE, FALSE))

    sample_quarter <- sample_data$n_quarter[1]

    check <- dplyr::if_else(true_quarter == 0, "Drop",
      dplyr::if_else(is.na(sample_quarter), "Drop", "Keep")
    )
    return(check)

  } else {
    true_data <- data %>%
      dplyr::filter(dplyr::between(.data$SURGERY_DATE, as.Date(date1), as.Date(date2))) %>%
      dplyr::mutate(year = lubridate::floor_date(.data$SURGERY_DATE, unit = "year")) %>%
      dplyr::select(.data$year) %>%
      unique() %>%
      dplyr::add_tally(n = "n_year") %>%
      dplyr::select(.data$year, .data$n_year)

    true_year <- true_data$n_year[1]

    sample_data <- data1 %>%
      dplyr::group_by(.data$Sykehus) %>%
      dplyr::add_tally(n = "n_year") %>%
      dplyr::mutate(check = dplyr::if_else(.data$n_year == true_year, TRUE, FALSE))

    sample_year <- sample_data$n_year[1]

    # check <- if_else(FALSE %in% sample_data$check, "Drop",
    #                  if_else(true_year == 0, "Drop",
    #                          if_else(is.na(sample_year), "Drop", "Keep")))

    check <- dplyr::if_else(true_year == 0, "Drop",
      dplyr::if_else(is.na(sample_year), "Drop", "Keep")
    )

    return(check)
  }
}

# nolint start
# test for å se om det fungerer:
## r  <- sjekk_antall(regdata, t, "2024-01-01", "2025-01-01", "aar")
# nolint end
