#' Clean data for module data dump
#'
#' @title Clean_datadump
#'
#' @param data datasett
#' @param dato1 brukervalg - dato min
#' @param dato2 brukervalg - dato max
#' @param kjoenn brukervalg - kjønn
#' @param alder1 brukervalg - alder min
#' @param alder2 brukervalg - alder max
#' @param userRole brukerrolle
#' @param userUnitId brukertilhørighet
#' @return datasett filtrert på brukervalg
#' @export


clean_datadump <- function(data, dato1, dato2, kjoenn, alder1, alder2, userRole, userUnitId) {


  data <- data %>%
    dplyr::filter(dplyr::between(SURGERY_DATE, as.Date({{dato1}}), as.Date({{dato2}})))


  data <- data %>%
    dplyr::filter(Kjønn == dplyr::case_when({{kjoenn}} == "kvinne" ~ "kvinne",
                                            {{kjoenn}} == "mann" ~ "mann",
                                            {{kjoenn}} != "kvinne" | {{kjoenn}} != "mann" ~ Kjønn))

  data <- data %>%
    dplyr::filter(dplyr::between(Alder_num, {{alder1}}, {{alder2}}))


  if (userRole != "SC") {
    data <- data %>%
      dplyr::select(-dplyr::contains(c("mths", "mnd"))) %>%
      dplyr::filter(CENTREID == userUnitId)
  }

  return(data)
}
