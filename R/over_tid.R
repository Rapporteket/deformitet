
## GJENNOMSNITT OVER TID ##

# Funksjon for å regne gjennomsnitt pr. kvartal, pr. sykehus og nasjonalt
### Siden dette er en kontinuerlig variabel:
### Mapping kjører først
### Deretter kjører prep_var med mapping som input
### Deretter kjører table_freq_time med mapping som input

#map_var <- deformitet::mapping_old_name_new_name(regdata, "SRS22_total_3mnd")
#prep_data <- prepVar(regdata, map_var, "mm", "2024-01-01", "2025-01-01", 1, 20, "Begge", "over_tid")
#prep_data_var <- data.frame(prep_data[1])
#gg_data <- data.frame(prep_data[2])
#

# plot <- over_tid_plot(table_data, "Alle avdelinger", "Haukeland")
# plot

#' @title Tabell - gjennomsnitt over tid
#' @export

table_freq_time <- function (data,
                             var,
                             map_data,
                             tidsenhet = "kvartal",
                             visning = "hele landet",
                             userUnitId) {

  data$PID <- as.character(data$PID)


  if (tidsenhet == "kvartal") {

  data <- data %>%
    dplyr::rename(mine = {{var}},
                    date = 6) %>%
    dplyr::mutate(quarter = lubridate::floor_date(date, unit = "quarter"),
                  tid = ymd(quarter))

  } else {

    data <- data %>%
      dplyr::rename(mine = {{var}},
                    date = 6) %>%
      dplyr::mutate(aar = lubridate::floor_date(date, unit = "year"),
                    tid = format(aar, "%Y"))
  }

  ## Pr. sykehus

  data_sykehus <- data %>%
    dplyr::filter(!is.na(mine)) %>%
    dplyr::group_by(Sykehus, tid) %>%
    dplyr::summarize(gjen = mean(mine)) %>%
    dplyr::select(c(Sykehus, tid, gjen))

  data_tally <- data %>%
    dplyr::group_by(Sykehus, tid) %>%
    dplyr::add_tally(n = "antall") %>%
    select(Sykehus, tid, antall) %>%
    unique()

  data_sykehus <- left_join(data_sykehus, data_tally)

  ## Nasjonalt

  data_nasjonalt <- data %>%
    dplyr::filter(!is.na(mine)) %>%
    dplyr::select(-Sykehus) %>%
    dplyr::mutate(Sykehus = "Nasjonalt") %>%
    dplyr::group_by(Sykehus, tid) %>%
    dplyr::summarize(gjen = mean(mine)) %>%
    dplyr::select(c(Sykehus, tid, gjen))

  data_nasjonalt_tally <- data %>%
    dplyr::select(-Sykehus) %>%
    dplyr::mutate(Sykehus = "Nasjonalt") %>%
    dplyr::group_by(Sykehus, tid) %>%
    dplyr::add_tally(n = "antall") %>%
    select(Sykehus, tid, antall) %>%
    unique()

  data_nasjonalt <- left_join(data_nasjonalt, data_nasjonalt_tally)


  data <- rbind(data_sykehus, data_nasjonalt) # Bind disse to sammen

  map_data <- map_data %>%
    dplyr::add_row(UnitId = "0", orgname = "Nasjonalt")

  data <- merge(data, map_data, by.x = "Sykehus", by.y = "orgname")

  data <- data %>%
    dplyr::filter(case_when({{visning}} == "hele landet, uten sammenligning" ~
                              Sykehus == "Nasjonalt",
                            {{visning}} == "egen enhet" ~
                              UnitId == {{userUnitId}},
                            {{visning}} == "hver enhet" ~
                              Sykehus != "Nasjonalt",
                            .default = Sykehus == Sykehus))

  data <- data %>%
    select(-c(UnitId))

  return(data)
}

# nolint start
## Test:
###t <- table_freq_time(prep_data_var, map_var, map_db_resh, "aar", "hver enhet", 111961)
# nolint end


# Funksjon for å lage plot

#' @title Plot - gjennomsnitt over tid
#' @export

over_tid_plot <- function (data, visning, gg_data, map_var) {

  data$Sykehus <- as.factor(data$Sykehus)

  if (visning == "hele landet") {
    data$Sykehus <- relevel(data$Sykehus, "Nasjonalt")
    } else {
      data <- data
      }

  limits <- y_limits_gjen(map_var)

  tid_plot <-
    ggplot2::ggplot(data, aes(x = tid, y = gjen,
                              color = Sykehus, group = Sykehus))+
    geom_line(linewidth = 1.2)+
    geom_point(size = 2.2)

  ## GENERELLE TILPASNINGER AV PLOT ##
  tid_plot = tid_plot +
    theme_bw(base_size = 16)+

    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_text(size = 16))+

    xlab("Tid")+
    ylab(gg_data$xlab)+
    ggtitle("Gjennomsnitt over tid")+

    scale_color_manual(values = # adding chosen colors
                         c("darkblue", "#6CACE4", "#ADDFB3", "#87189D"))+

    ylim(limits$ymin, limits$ymax)

  return(tid_plot)

}

# nolint start
# Test for å sjekke om det fungerer:
##r <- over_tid_plot(t, "egen enhet", gg_data, map_var)
##r
# nolint end



# Oversikt variabler pr. skjema
skjema <- data.frame(pre_op_pas = c("Helsetilstand",
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
                                    ""),
                     pre_op_lege = c("BMI_kategori",
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
                                     ""),
                     tre_mnd_pas = c("Helsetilstand_3mnd",
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
                                     "SRS22_SATISFACTION_SCORE"),
                     tre_mnd_lege = c("",
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
                                      ""),
                     tolv_mnd_pas = c("Helsetilstand_12mnd",
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
                                      "SRS22_SATISFACTION_SCORE_patient12mths"),
                     tolv_mnd_lege = c("",
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
                                       ""),
                     seksti_mnd_pas = c("Helsetilstand_60mnd",
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
                                        "SRS22_SATISFACTION_SCORE_patient60mths"),
                     andre = c("Alder",
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
                               ""))

usethis::use_data(skjema, overwrite = TRUE)


####### Function to set appropriate limits on y-axis ##############
#' @title Y-axis limits
#' @export

y_limits_gjen <- function (var) {

  data <- data.frame(ymin = "", ymax = "")
  data <- data %>%
    dplyr::mutate(ymin = case_when(str_detect(var, "SRS22") == TRUE ~ 1,
                                   str_detect(var, "SCALE") == TRUE ~ 0,
                                   str_detect(var, "Alder") == TRUE ~ 5,
                                   str_detect(var, "BMI") == TRUE ~ 10,
                                   str_detect(var, "PRE_MAIN_CURVE") == TRUE ~ 20,
                                   str_detect(var, "POST_MAIN_CURVE") == TRUE ~ 0,
                                   str_detect(var, "kurve") == TRUE ~ 0,
                                   str_detect(var, "tid") == TRUE ~ 0,
                                   str_detect(var, "BED") == TRUE ~ 0,
                                   str_detect(var, "BLOOD") == TRUE ~ 100),
                  ymax = case_when(str_detect(var, "SRS22") == TRUE ~ 5,
                                   str_detect(var, "SCALE") == TRUE ~ 100,
                                   str_detect(var, "Alder") == TRUE ~ 80,
                                   str_detect(var, "BMI") == TRUE ~ 35,
                                   str_detect(var, "PRE_MAIN_CURVE") == TRUE ~ 110,
                                   str_detect(var, "POST_MAIN_CURVE") == TRUE ~ 50,
                                   str_detect(var, "kurve") == TRUE ~ 110,
                                   str_detect(var, "tid") == TRUE ~ 600,
                                   str_detect(var, "BED") == TRUE ~ 12,
                                   str_detect(var, "BLOOD") == TRUE ~ 1000))

}

# nolint start
# Test to see if it works:
## rr <- r("PER_BLOOD_LOSS_VALUE")
# nolint end

# Funksjon for å sjekke størrelse

sjekk_antall <- function (data, data1, date1, date2, tidsenhet) {

  if (tidsenhet == "kvartal") {

  true_data <- data %>%
    filter(between(SURGERY_DATE, as.Date(date1), as.Date(date2))) %>%
    dplyr::mutate(quarter = lubridate::floor_date(SURGERY_DATE, unit = "quarter")) %>%
    select(quarter) %>%
    unique() %>%
    add_tally(n = "n_quarter") %>%
    select(quarter, n_quarter)

  true_quarter <- true_data$n_quarter[1]

  sample_data <- data1 %>%
    group_by(Sykehus) %>%
    add_tally(n = "n_quarter") %>%
    dplyr::mutate(check = if_else(n_quarter == true_quarter, TRUE, FALSE))

  sample_quarter <- sample_data$n_quarter[1]

  check <- if_else(true_quarter == 0, "Drop",
                           if_else(is.na(sample_quarter), "Drop", "Keep"))

  return(check)

  } else {

    true_data <- data %>%
      filter(between(SURGERY_DATE, as.Date(date1), as.Date(date2))) %>%
      dplyr::mutate(year = lubridate::floor_date(SURGERY_DATE, unit = "year")) %>%
      select(year) %>%
      unique() %>%
      add_tally(n = "n_year") %>%
      select(year, n_year)

    true_year <- true_data$n_year[1]

    sample_data <- data1 %>%
      group_by(Sykehus) %>%
      add_tally(n = "n_year") %>%
      dplyr::mutate(check = if_else(n_year == true_year, TRUE, FALSE))

    sample_year <- sample_data$n_year[1]

    # check <- if_else(FALSE %in% sample_data$check, "Drop",
    #                  if_else(true_year == 0, "Drop",
    #                          if_else(is.na(sample_year), "Drop", "Keep")))

    check <- if_else(true_year == 0, "Drop",
                     if_else(is.na(sample_year), "Drop", "Keep"))

    return(check)

  }
}

# nolint start
# test for å se om det fungerer:
##r  <- sjekk_antall(regdata, t, "2024-01-01", "2025-01-01", "aar")
# nolint end
