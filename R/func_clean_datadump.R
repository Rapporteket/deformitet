#' Clean data for module data dump
#'
#' @title Clean_datadump
#'
#' @export


clean_datadump <- function(data, var1a, var1b, var2, var3a, var3b, userRole, userUnitId) {


  data <- data %>%
    dplyr::filter(dplyr::between(SURGERY_DATE, as.Date({{var1a}}), as.Date({{var1b}})))


  data <- data %>%
    dplyr::filter(Kjønn == dplyr::case_when({{var2}} == "kvinne" ~ "kvinne",
                                            {{var2}} == "mann" ~ "mann",
                                            {{var2}} != "kvinne" | {{var2}} != "mann" ~ Kjønn))

  data <- data %>%
    dplyr::filter(dplyr::between(Alder_num, {{var3a}}, {{var3b}}))


  if (userRole != "SC") {
    data <- data %>%
      dplyr::select(-contains(c("mths", "mnd"))) %>%
      dplyr::filter(CENTREID == userUnitId)
  }

  return(data)
}
