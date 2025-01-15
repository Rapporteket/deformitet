#' Hent tabell fra Deformitets database
#'
#' @param tabellnavn Navn på tabell som skal lastes inn.
#'                   Kan ha følgende verdier:
#'                   mce
#'                   mce_patient_data
#'                   patient
#'                   patientfollowup
#'                   patientform
#'                   surgeonfollowup
#'                   surgeonform
#' @export
deformitetHentTabell <- function(tabellnavn = "surgeonform") {
  registryName <- "deformitet"
  dbType <- "mysql"
  query <- paste0("SELECT * FROM ", tabellnavn)

  tabell <- rapbase::loadRegData(registryName, query, dbType)
  return(tabell)
}
