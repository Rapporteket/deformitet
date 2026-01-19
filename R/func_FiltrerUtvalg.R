#' Filtrer data for module data dump
#' Funksjonen filtrerer data
#'
#' @title Filtrer datadump
#'
#' @param data datasett
#' @param dato1 brukervalg - dato min
#' @param dato2 brukervalg - dato max
#' @param userRole brukerrolle
#' @param userUnitId brukertilhørighet
#' @return datasett filtrert på brukervalg
#' @export


filtrer_datadump <- function(data, dato1, dato2, userRole, userUnitId) { #

  data <- data |>
    dplyr::filter(dplyr::between(.data$SURGERY_DATE, as.Date({{dato1}}), as.Date({{dato2}})))


  if (userRole != "SC") {
    data <- data |>
      dplyr::select(-dplyr::contains(c("mths", "mnd"))) |>
      dplyr::filter(.data$CENTREID == userUnitId)
  }

  return(data)
}



#' @title Utvalgsfunksjon
#'
#' @export

utvalg_basic <- function(data, user_unit, gender, type_op, tid1, tid2, alder1, alder2, bruk_av_funk) {

  # Filter by unit (if desirable)

  if (bruk_av_funk != "ikke_filtrer_reshId") {
    data <- data |>
      dplyr::filter(.data$CENTREID == user_unit)
  } else {
    data <- data
  }

  # Filter by gender

  data <- data |>
    dplyr::filter(.data$Kjonn == dplyr::case_when({{gender}} == "kvinne" ~ "kvinne",
                                                  {{gender}} == "mann" ~ "mann",
                                                  {{gender}} != "kvinne" | {{gender}} != "mann" ~ Kjonn))

  # Filter by operation type

  data <- data |>
    dplyr::filter(dplyr::case_when({{type_op}} == "Primæroperasjon" ~ CURRENT_SURGERY == 1,
                                   {{type_op}} == "Reoperasjon" ~ CURRENT_SURGERY == 2,
                                   {{type_op}} == "Begge" ~ CURRENT_SURGERY %in% c(1, 2)))

  # Add filter on surgery date--------------------------------------------------

  data <- data |>
    dplyr::filter(dplyr::between(.data$SURGERY_DATE,
                                 as.Date({{tid1}}),
                                 as.Date({{tid2}})))

  # Add filter on age-----------------------------------------------------------

  # Using column "Alder_num" in which alder is given as an integer
  data <- data |>
    dplyr::filter(dplyr::between(.data$Alder_num,
                                 {{alder1}},
                                 {{alder2}}))


  return(data)

}

# nolint start
## TEST AT DET FUNGERER:
##
##
##g <- utvalg_basic(RegData, 111961, "mann", "Primæroperasjon", "2023-01-01", "2025-12-01", 1, 100, "ikke_filtrer_reshId")

# nolint end
