#' @title Utvalgsfunksjon
#'
#' @export

utvalg_basic <- function (data, unit, gender, type_op, tid1, tid2, alder1, alder2) {

  # Filter by unit

  data <- data %>%
    dplyr::filter(dplyr::case_when({{unit}} == 103240 ~ CENTREID == 103240,
                                   {{unit}} == 102467 ~ CENTREID == 102467,
                                   {{unit}} == 111961 ~ CENTREID == 111961,
                                   {{unit}} == "alle" ~ CENTREID %in% c(103240, 102467, 111961)))


  # Filter by gender

  data <- data %>%
    dplyr::filter(Kjønn == dplyr::case_when({{gender}} == "kvinne" ~ "kvinne",
                                            {{gender}} == "mann" ~ "mann",
                                            {{gender}} != "kvinne" | {{gender}} != "mann" ~ Kjønn))

  # Filter by operation type

  data <- data %>%
    dplyr::filter(dplyr::case_when({{type_op}} == "Primæroperasjon" ~ CURRENT_SURGERY == 1,
                                   {{type_op}} == "Reoperasjon" ~ CURRENT_SURGERY == 2,
                                   {{type_op}} == "Begge" ~ CURRENT_SURGERY %in% c(1, 2)))

# Add filter on surgery date--------------------------------------------------

data <- data %>%
  dplyr::filter(dplyr::between(SURGERY_DATE,
                               as.Date({{tid1}}),
                               as.Date({{tid2}})))

# Add filter on age-----------------------------------------------------------

# Using column "Alder_num" in which alder is given as an integer

data <- data %>%
  dplyr::filter(dplyr::between(Alder_num,
                               {{alder1}},
                               {{alder2}}))


return (data)

}

# nolint start
## TEST AT DET FUNGERER:
##
##
##g <- utvalg_basic(regdata, 111961, "m", "Begge", "2023-01-01", "2024-12-01", 1, 100)
# nolint end
