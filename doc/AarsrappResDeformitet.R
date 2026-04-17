# Årsrapportresultater

#Deformitet
#library(deformitet)
setwd('../Aarsrapp/NKR')
library(xtable)
aarsRappAar <- 2025

startAar <- 2023
datoFra <- paste0(startAar,'-01-01')
datoFra1aar <- paste0(aarsRappAar,'-01-01')
datoFra2aar <- paste0(aarsRappAar-1,'-01-01')
datoFra3aar <- paste0(aarsRappAar-2,'-01-01')
datoTil12mnd <- paste0(aarsRappAar-1,'-12-31')
datoTil <- paste0(aarsRappAar,'-12-31')
aar <- aarsRappAar
aar2 <- (aarsRappAar-1):aarsRappAar
aar2_12mnd <- (aarsRappAar-2):(aarsRappAar-1)
tidlAar <- aarsRappAar-1
tidlAar2 <- (aarsRappAar-3):(aarsRappAar-2)

source("C:/Users/lro2402unn/RegistreGIT/deformitet/dev/sysSetenv.R")
DefDataRaa <- alleRegData(egneVarNavn = 0)
DefData <- preprosData(RegData=DefDataRaa, egneVarNavn = 0)
DefData <- utvalgEnh(RegData=DefData, datoTil=datoTil)$RegData
DefData1aar <- utvalgEnh(RegData=DefData, datoFra = datoFra1aar)$RegData


#---------Kvalitetsindikatorer----------
# 1 års reoperasjonsrate
# Andel pasienter reoperert innen 1 år etter primæroperasjon.
# Høy måloppnåelse: < 5%
#Kan bare ta med pasienter operert tom året før årsrapp. Slå sammen to år

figAndelerGrVar(RegData=DefData, valgtVar = 'reOp',
                datoFra = datoFra3aar , datoTil=datoTil12mnd,
                outfile = 'Def_reOp2aarSh.pdf')

# 2. Liggetid
# Andel pasienter utskrevet innen (til og med) 6. postoperative dag
# Høy måloppnåelse: >90%
figAndelerGrVar(RegData=DefData1aar, valgtVar = 'liggetidPostOp',
                 outfile = 'Def_liggetidPostOpSh.pdf')

# 3.	Pasientfornøydhet etter 12mnd (2år)
# Andel pasienter som på SRS-22 spørsmål nr 21 svarer "svært godt fornøyd" (verdi 5) eller "ganske fornøyd" (verdi 4).
# Høy måloppnåelse: >90%,  Moderat: 70-90%,  Lav: <70%
# Pasienter operert i 2023
#Må komme klart fram i figuren hvordan fornøydhet er bestemt
figAndelerGrVar(RegData=DefData, valgtVar = 'fornoydBeh2aar',
                datoFra = datoFra3aar, datoTil = paste0(aarsRappAar-2,'-12-31'),
                outfile = 'Def_fornoydBeh2aarSh.pdf')


#-------Andre resultater-----------
#April 2026: Bare andel per enhet finnes som standardisert figur. Viser de andre resultatene som tabeller.

tabAnt <- table(DefData[,c('ShNavn', "Aar")])
tabAnt <- rbind(tabAnt,
                'Hele landet' = table(DefData$Aar))
xtable::xtable(tabAnt,
               caption = 'Antall registreringer per år')

#Tabell med pasienttilfredshet, fordeling
grtxt <- c("Svært misfornøyd","Litt misfornøyd","Verken eller","Ganske fornøyd","Svært fornøyd","Ikke svart")
DefData$Fornoyd2aar <- factor(DefData$SRS22_21_patient12mths, levels = c(1:5,9),
                                 labels = grtxt)
ind <- which(DefData$Aar %in% (aarsRappAar-2))
tab <- table(DefData$SRS22_21_patient12mths[ind])
row.names(tab) <- grtxt
xtable::xtable(rev(tab),
               caption = 'Pasienttilfredshet')

# postoperativ liggetid - gj.sn. per enhet og år. Per år
tabPOligg <- tapply(DefData$BED_DAYS_POSTOPERATIVE,
       INDEX = list(as.factor(DefData$ShNavn), as.factor(DefData$Aar)),
       FUN = mean, na.rm=T)
tabPOligg <- round(rbind(tabPOligg,
                   'Hele landet' = tapply(DefData$BED_DAYS_POSTOPERATIVE,
                                          INDEX = as.factor(DefData$Aar),
                                          FUN = mean, na.rm=T)), 1)
xtable::xtable(tabPOligg,
               caption = 'Gjennomsnittlig postoperativ liggetid per enhet og år.')


#Tabell med oversikt over årsak til reoperasjon

res <- varTilrettelegg(RegData = DefData, valgtVar='reopAarsak', figurtype='andeler')
variable <- res$variable
DefData <- res$RegData
Ngr <- apply(DefData[ ,variable], MARGIN=2,
                                FUN=function(x) sum(x == 1, na.rm=T))
N <- apply(DefData[ ,variable], MARGIN=2,
                              FUN=function(x) sum(x %in% 0:1, na.rm=T))
AggVerdier <- paste0(sprintf('%.1f', 100*Ngr/N), ' %')

names(AggVerdier) <- res$grtxt
tab <- cbind('Antall' = Ngr,
             Andel = AggVerdier)
row.names(tab) <- res$grtxt
xtable::xtable(tab,
               caption = 'Årsak til reoperasjon, 2023-2025')

#Figur for å vise enkelte pasienter reopereres flere ganger. Fordelingsfigur?

tab <- table(table(DefData$PasientID)-1)[-1]
#names(tab) <- c('Reoperasjoner', 'Pasienter')
xtable::xtable(tab,
               'Antall reoperasjoner, 2023-2025')



#  Kompletthet for variabler i e-post 1.april



