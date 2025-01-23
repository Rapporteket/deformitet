#' @title Utvalgsfunksjon
#'
#' @export

utvalg_basic <- function (data, gender, tid1, tid2, alder1, alder2) {

  # = regdata,
  #                         gender = "kvinne",
  #                         type_op = "Begge",
  #                         tid1 = min(regdata$SURGERY_DATE),
  #                         tid2 = max(regdata$SURGERY_DATE),
  #                         alder1 = min(regdata$Alder_num),
  #                         alder2 = max(regdata$Alder_num)) {


  # Filter by gender

  data <- data %>%
    dplyr::filter(Kjønn == dplyr::case_when({{gender}} == "kvinne" ~ "kvinne",
                                            {{gender}} == "mann" ~ "mann",
                                            {{gender}} != "kvinne" | {{gender}} != "mann" ~ Kjønn))

  # Filter by operation type

  # data <- data %>%
  #   dplyr::filter(CURRENT_SURGERY == dplyr::case_when({{type_op}} == "Primæroperasjon" ~ 1,
  #                                                     {{type_op}} == "Reoperasjon" ~ 2,
  #                                                     {{type_op}} != "Primæroperasjon" |
  #                                                       {{type_op}} != "Reoperasjon" ~ CURRENT_SURGERY))

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
## g <- utvalg_basic(regdata, "mm") #, "Begge", "2023-01-01", "2024-12-01", 1, 100)
# nolint end
