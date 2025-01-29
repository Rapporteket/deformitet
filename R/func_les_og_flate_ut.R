#' Les inn data og lag utflatet dataramme
#'
#' @export

mergeRegData <- function(mce, centre, mce_patient_data, patient, 
                         patient_followup, patient_form, surgeon_followup, surgeon_form) {
  regData <- merge(mce, mce_patient_data, by = "MCEID") %>% # nested merge
    merge(centre, by.y = "ID", by.x = "CENTREID", all.y = TRUE) %>%
    merge(surgeon_form %>% dplyr::filter(STATUS == 1), # filter by status == 1
          by = "MCEID", suffixes = c("", "_surgeon")) %>% # merge by MCEID
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

  return(regData)
}



les_og_flate_ut <- function() {

  tryCatch(
    {
      mce <- deformitet::deformitetHentTabell("mce")

      centre <- deformitet::deformitetHentTabell("centre") %>%
        dplyr::filter(ID != "TESTNO" & ID != "TESTNO2" & ID != "TESTNO3")

      # Take out test hospitals
      mce_patient_data <- deformitet::deformitetHentTabell("mce_patient_data")

      patient <- deformitet::deformitetHentTabell("patient")

      patient_followup <- deformitet::deformitetHentTabell("patientfollowup")

      patient_form <- deformitet::deformitetHentTabell("patientform")

      surgeon_followup <- deformitet::deformitetHentTabell("surgeonfollowup")
      surgeon_form <- deformitet::deformitetHentTabell("surgeonform")

      regData <- mergeRegData(
        mce, centre, mce_patient_data, patient,
        patient_followup, patient_form, surgeon_followup, surgeon_form
      )
    },
    error = function(e) {
      regData <- readRDS("../dev/fake_data_deformitet.rds")
      return(regData)
    }
  )
  return(regData)
}
