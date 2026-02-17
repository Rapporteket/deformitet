devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE)

setwd("C:/Users/lro2402unn/RegistreGIT/nger")
setwd('../data')
sship::dec("c://Users/lro2402unn/RegistreGIT/data/deformitet12bfc19c0.sql.gz__20260217_090623.tar.gz",
           keyfile = "c://Users/lro2402unn/.ssh/id_rsa")

# source c://Users/lro2402unn/RegistreGIT/data/deformitet12bfc19c0.sql;

library(deformitet)
source("dev/sysSetenv.R")
deformitet::kjorDeformApp(browser = TRUE)



# Få andelGrVar opp å kjøre:

raw_regdata <- alleRegData()

RegData <- alleRegData(egneVarNavn = 0)
RegData <- preprosData(RegData=RegData, egneVarNavn = 0)

#### Clean and tidy data:
regData <- pre_pros(raw_regdata)

RegData <- alleRegData(egneVarNavn = 0)
RegData <- preprosData(RegData=RegData, egneVarNavn = 0)
kvalInd <- c('liggetidPostOp', 'fornoydBeh2aar', 'reOp')
for (var in kvalInd) {
  valgtVar <- var
  figAndelerGrVar(RegData=RegData, hentData=0, preprosess=0,
                valgtVar=valgtVar,
                datoFra='2023-01-01', datoTil=Sys.Date(),
                Ngrense=10, reshID=0, outfile=paste0(valgtVar, '.pdf'))
}
