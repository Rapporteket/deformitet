################################################################################
## FUNKSJON SOM LAGER TABELL MED REGISTRERINGER PR OPERASJONSDATO ##
################################################################################

#' @title tabell registreringer
#' @param date1 dato-valg (min) gjort av brukeren
#' @param date2 dato-valg (max) gjort av brukeren
#' @param data datasett -> datasett som har vært gjennom prePros()
#' @examples
#' \donttest{
#' try(tbl_reg("01-09-2024", "01-01-2025", RegData))
#' }
#'
#' @export

tbl_reg <- function(date1, date2, data) {

  d1 <- as.Date(date1, format = "%d-%m-%Y")
  d2 <- as.Date(date2, format = "%d-%m-%Y")

  data <- data |>
    dplyr::filter(dplyr::between(SURGERY_DATE, d1, d2))

  data <- data |>
    group_by(lubridate::year(SURGERY_DATE), lubridate::month(SURGERY_DATE)) |>
    count(Sykehus) |>
    rename(mnd = `lubridate::month(SURGERY_DATE)`,
           aar = `lubridate::year(SURGERY_DATE)`)

  reg_tbl <- data |>
    pivot_wider(names_from = c(mnd, aar),names_sep = "-", values_from = n) |>

    mutate_all(~replace(., is.na(.), 0)) |>
    mutate(Totalt = rowSums(across(where(is.numeric))))



  return (reg_tbl)

}


################################################################################
## FUNKSJON SOM LAGER OVERSIKT OVER REGISTRERINGER PR SKJEMA ##
################################################################################

#' @title tabell registreringer pr skjema
#' @param date1 dato-valg (min) gjort av brukeren
#' @param date2 dato-valg (max) gjort av brukeren
#' @param data data -> data som har vært gjennom prePros()
#' @examples
#' \donttest{
#' try(tbl_skjema_reg("01-09-2024", "01-01-2025", RegData))
#' }
#'
#' @export

tbl_skjema_reg <- function (date1, date2, data) {

  tbl_skjema <- data |>
    dplyr::filter(dplyr::between(SURGERY_DATE,
                                 as.Date({{date1}}, format = "%d-%m-%Y"),
                                 as.Date({{date2}}, format = "%d-%m-%Y"))) |>
    group_by(Sykehus) |>
    mutate(personopplysninger = sum(!is.na(REGISTERED_DATE)),
           Skjema_1a_Pasientoppl_preop = sum(!is.na(FILLING_DATE)),
           Skjema_2a_Sykepleier_lege_preop = sum(!is.na(SURGERY_DATE)),
           Skjema_1a_Pasientoppl_3mnd = sum(!is.na(FOLLOWUP)),
           Skjema_2a_Sykepleier_lege_3mnd = sum(!is.na(FOLLOWUP_surgeon3mths)),
           Skjema_1a_Pasientoppl_12mnd = sum(!is.na(FOLLOWUP_patient12mths)),
           Skjema_2a_Sykepleier_lege_12mnd = sum(!is.na(FOLLOWUP_surgeon12mths)),
           Skjema_1a_Pasientoppl_60mnd = sum(!is.na(FOLLOWUP_patient60mths))) |>
    select(c(Sykehus,
             personopplysninger,
             Skjema_1a_Pasientoppl_preop,
             Skjema_2a_Sykepleier_lege_preop,
             Skjema_1a_Pasientoppl_3mnd,
             Skjema_2a_Sykepleier_lege_3mnd,
             Skjema_1a_Pasientoppl_12mnd,
             Skjema_2a_Sykepleier_lege_12mnd,
             Skjema_1a_Pasientoppl_60mnd)) |>
    unique()

  return(tbl_skjema)
}
