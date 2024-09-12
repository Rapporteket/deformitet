# Use function "deformitetHentTabell" from package "deformitet"
# deformitet::deformitetHentData
# Load in each table separately

mce <- deformitet::deformitetHentTabell("mce")

mce_patient_data <- deformitet::deformitetHentTabell("mce_patient_data")

patient <- deformitet::deformitetHentTabell("patient")

patient_followup <- deformitet::deformitetHentTabell("patientfollowup")

patient_form <- deformitet::deformitetHentTabell("patientform")

surgeon_followup <- deformitet::deformitetHentTabell("surgeonfollowup")

surgeon_form <- deformitet::deformitetHentTabell("surgeonform")


# Making one big file
# Left joining each table together by the biggest table
# This means that some of the values will by "NA" just due to the structure of the biggest table
# BIG problem: MCEID is not present across tables... so how am I to map these?

# Trying different things:
ls_df <- list(mce,
              mce_patient_data,
              patient,
              patient_followup,
              patient_form,
              surgeon_followup,
              surgeon_form)

f <- merge(ls_df, by = "MCEID", all = TRUE)

ls_df %>%
  purrr::reduce(dplyr::left_join, by = "MCEID")
