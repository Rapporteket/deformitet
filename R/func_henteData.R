#' Hent datatabell fra Deformitets database
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
defHentData <- function(tabellnavn = "surgeonform") {
  query <- paste0("SELECT * FROM ", tabellnavn)
  tabell <- rapbase::loadRegData(registryName = "data",
                                 query = query)
  return(tabell)
}



#' Hente og koble sammen "alle" tabeller
#' Ferdigstilte data
#'
#' @export
alleRegData <- function(egneVarNavn=0) {

  mce <- defHentData("mce")
  centre <- defHentData("centre") # %>%
 #   dplyr::filter(ID != "TESTNO" & ID != "TESTNO2" & ID != "TESTNO3") # Take out test hospitals
  patient <- defHentData("patient")
  patient_followup <- defHentData("patientfollowup")
  patient_form <- defHentData("patientform")
  surgeon_followup <- defHentData("surgeonfollowup")
  surgeon_form <- defHentData("surgeonform")

  if (egneVarNavn==1){

    FriendlyVarTab  <- defHentData('friendlyvars')
    FriendlyVarTab <- FriendlyVarTab[!is.na(FriendlyVarTab$USER_SUGGESTION),
                            c("FIELD_NAME", "VAR_ID", "TABLE_NAME", "USER_SUGGESTION", "REGISTRATION_TYPE")]
    #Egenvalgte navn omfatter disse:
    # TABLE_NAME: PATIENT, SURGEONFORM, PATIENTFOLLOWUP, PATIENTFORM, SURGEONFOLLOWUP
    # REGISTRATION_TYPE:
    # PATIENT, PATIENTFOLLOWUP, PATIENTFOLLOWUP12, PATIENTFORM,
    # SURGEONFORM, SURGEONFOLLOWUP SURGEONFOLLOWUP12
    #rename: new_name = old_name

    egneNavn <- function(dataTab = patient, tabNavn = 'PATIENT', FriendlyVarTab){
      Navn <- FriendlyVarTab$FIELD_NAME[FriendlyVarTab$TABLE_NAME==tabNavn]
      names(Navn) <- FriendlyVarTab$USER_SUGGESTION[FriendlyVarTab$TABLE_NAME==tabNavn]
      data <- rename(dataTab, all_of(Navn))
      return(data)
      }

    #patient <- egneNavn(patient, 'PATIENT', FriendlyVarTab)
    Navn <- FriendlyVarTab$FIELD_NAME[FriendlyVarTab$TABLE_NAME=='PATIENT']
    names(Navn) <- FriendlyVarTab$USER_SUGGESTION[FriendlyVarTab$TABLE_NAME=='PATIENT']
    patient <- rename(patient, all_of(Navn))

    # surgeon_form <- egneNavn(data = surgeon_form, tabNavn = 'SURGEONFORM', FriendlyVarTab)
    Navn <- FriendlyVarTab$FIELD_NAME[FriendlyVarTab$TABLE_NAME=='SURGEONFORM']
    names(Navn) <- FriendlyVarTab$USER_SUGGESTION[FriendlyVarTab$TABLE_NAME=='SURGEONFORM']
    test <- rename(surgeon_form, all_of(Navn))

    patient_form <- defHentData("patientform")

    patient_followup <- defHentData("patientfollowup")
    surgeon_followup <- defHentData("surgeonfollowup")

  }

  RegData <- merge(mce, centre, by.y = "ID", by.x = "CENTREID", all.y = TRUE) %>%
    merge(surgeon_form %>% dplyr::filter(STATUS == 1),
          by = "MCEID", suffixes = c("", "_surgeon")) %>%
    merge(patient_form %>% dplyr::filter(STATUS == 1),
          by = "MCEID", suffixes = c("", "_patient_form"), all.x = TRUE) %>%
    merge(patient, by.x = "PATIENT_ID", suffixes = c("", "_patient"),
          by.y = "ID") %>%
    merge(patient_followup %>% dplyr::filter(FOLLOWUP == 3 & STATUS == 1),
          suffixes = c("", "_patient3mths"), by = "MCEID", all.x = TRUE) %>%
    merge(patient_followup %>% dplyr::filter(FOLLOWUP == 12 & STATUS == 1),
          suffixes = c("", "_patient12mths"), by = "MCEID", all.x = TRUE) %>%
    merge(patient_followup %>% dplyr::filter(FOLLOWUP == 60 & STATUS == 1),
          suffixes = c("", "_patient60mths"), by = "MCEID", all.x = TRUE) %>%
    merge(surgeon_followup %>% dplyr::filter(FOLLOWUP == 3 & STATUS == 1),
          suffixes = c("", "_surgeon3mths"), by = "MCEID", all.x = TRUE) %>%
    merge(surgeon_followup %>% dplyr::filter(FOLLOWUP == 12 & STATUS == 1),
          suffixes = c("", "_surgeon12mths"), by = "MCEID", all.x = TRUE)

  return(RegData)
}




