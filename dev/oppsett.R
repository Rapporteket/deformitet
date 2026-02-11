devtools::install("../rapbase/.")
devtools::install(upgrade = FALSE)

setwd("C:/Users/lro2402unn/RegistreGIT/nger")
setwd('../data')
sship::dec("c://Users/lro2402unn/RegistreGIT/data/intensiv16d066500.sql.gz__20260210_092836.tar.gz",
           keyfile = "c://Users/lro2402unn/.ssh/id_rsa")

# source c://Users/lro2402unn/RegistreGIT/data/intensiv16d066500.sql;

library(deformitet)
source("dev/sysSetenv.R")
deformitet::kjorDeformApp(browser = TRUE)



# Få andelGrVar opp å kjøre:

raw_regdata <- alleRegData()

RegData <- alleRegData(egneVarNavn = 0)
RegData <- preprosData(RegData=RegData, egneVarNavn = 0)

#### Clean and tidy data:
regData <- pre_pros(raw_regdata)

valgtVar <- 'liggetidPostOp' #
valgtVar <- 'fornoydBeh2aar'
figAndelerGrVar(RegData=0, hentData=1, preprosess=1,
                            valgtVar=valgtVar, minald=0, maxald=130, erMann=9,
                            datoFra='2023-01-01', datoTil=Sys.Date(),
                            Ngrense=10, reshID=0, outfile='') # paste0(valgtVar, '.pdf'))
