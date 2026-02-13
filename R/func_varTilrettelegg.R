#' Funksjon for å tilrettelegge variabler for beregning.
#'
#' Funksjonen gjør utvalg og tilrettelegger variabler (gitt ved valgtVar) til
#' videre bruk. Videre bruk kan eksempelvis være beregning av aggregerte
#' verdier.
#' Funksjonen gjør også filtreringer som å fjerne ugyldige verdier for den
#' valgte variabelen, samt ta høyde for avhengigheter med
#' andre variabler. Det er også her man angir aksetekster og titler for den
#' valgte variabelen.
#'
#' @param valgtVar parameter som angir hvilke(n) variabel man ønsker å
#' tilrettelegge for videre beregning.
#' @param figurtype Hvilken figurtype det skal tilrettelegges variabler for:
#'                'andeler', 'andelGrVar', 'andelTid', 'gjsnGrVar', 'gjsnTid'
#'
#' @return Definisjon av valgt variabel.
#'
#' @export
#'

varTilrettelegg  <- function(RegData, valgtVar, figurtype='andeler'){
   #grVar='',

  "%i%" <- intersect

  #----------- Figurparametre ------------------------------
  #cexgr <- 1	#Kan endres for enkeltvariable
  retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
  flerevar <- 0
  grtxt <- ''		#Spesifiseres for hver enkelt variabel
  grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
  varTxt <- ''
  xAkseTxt <- ''	#Benevning
  if (figurtype == 'andelGrVar') {xAkseTxt <- 'Andel operasjoner (%)'}
  ytxt1 <- ''
  sortAvtagende <- FALSE  #Sortering av resultater. FALSE-laveste best
  tittel <- 'Mangler tittel'
  deltittel <- ''
  variabler <- 'Ingen'
  KIekstrem <- NULL
  RegData$Variabel <- 0

  #--------------- Definere variabler ------------------------------

  tittel <- '' #I AndelerGrVar og GjsnGrVar genereres tittel i beregningsfunksjonen

  #-------------------------------------

  # reoperasjon, liggetid og fornøydhet


   if (valgtVar=='reOp') { #AndelGrVar,AndelTid,
     # 1.	1 års reoperasjonsrate
     # Andel pasienter reoperert innen 1 år etter primæroperasjon.
     # Høy måloppnåelse: < 5%

     # Beregnes ved å se på differansen mellom CURRENT_SURGERY=1 og 2 for samme PasientID?
    # •	Tilhører reoperasjon innen ett år samme forløp som den første operasjonen, eller blir det to forløp?
        # Det blir opprettet et nytt forløp ved reoperasjon, men kun for registering av skjema ved reoperasjon.
        # PROM data og kontroll intervaller følger fortsatt primæroperasjonen.
# reoperert1år / alle pasienter

     tittel <- 'Reoperert innen 1 år etter primæroperasjon'
     # source("dev/sysSetenv.R")
     # RegData <- alleRegData(egneVarNavn = 0)
     # RegData <- preprosData(RegData=RegData, egneVarNavn = 0)
     # RegData$Variabel <- 0
     pasIDreop <- unique(RegData$PasientID[which(RegData$CURRENT_SURGERY==2)])

     RegData_pas2op <-  RegData[which(RegData$PasientID %in% pasIDreop),
                                c("OpDato", "PasientID", "CURRENT_SURGERY", "MCEID",'PARENT')]

     RegDataPas <- RegData_pas2op |> dplyr::group_by(PasientID) |>
       dplyr::summarise(
         PasientID = PasientID[1],
         Dager = sort(OpDato[CURRENT_SURGERY ==2])[1] - OpDato[CURRENT_SURGERY == 1]
       )
     RegData <- RegData[which(RegData$CURRENT_SURGERY == 1),]
     RegData$Variabel[
     RegData$PasientID %in% RegDataPas$PasientID[RegDataPas$Dager < 365]] <- 1

     subtxt <- 'reoperasjon'
     TittelVar <- 'Reoperert innen 1år'
     ytxt1 <- 'reoperasjon'

   }


  if (valgtVar=='liggetidPostOp') { #fordeling, andelGrVar
    # Andel pasienter utskrevet innen (til og med) 6. postoperative dag
    # Høy måloppnåelse: >90%

    tittel <- 'Utskrevet innen ei uke'

    RegData <- RegData[which(RegData$BED_DAYS_POSTOPERATIVE>=0),]
    # RegData$VariabelGr <- cut(RegData$BED_DAYS_POSTOPERATIVE, breaks=gr, include.lowest=TRUE, right=FALSE)
    # grtxt <- c(0:6, '7+')
     RegData$Variabel[RegData$BED_DAYS_POSTOPERATIVE <= 6] <- 1
    # gr <- c(0:7,100)
    xAkseTxt <- 'Antall liggedøgn'
    subtxt <- 'døgn'
    TittelVar <- 'Liggetid etter operasjon'
    ytxt1 <- 'liggetid'
    sortAvtagende <- TRUE
  }

  if (valgtVar %in% c('fornoydBeh3mnd','fornoydBeh2aar')) { #Andeler #AndelGrVar #AndelTid
    # 3.	Pasientfornøydhet etter 12mnd (2år)
    # Andel pasienter som på SRS-22 spørsmål nr 21 svarer "svært godt fornøyd" (verdi 5) eller "ganske fornøyd" (verdi 4).
    # Høy måloppnåelse: >90%, # Moderat: 70-90%, # Lav: <70%
    # SRS22_21	Fornoyd2ar	21. Er du fornøyd med resultatet av behandlingen?	Ja

    RegData$VariabelGr <- switch(valgtVar,
                      fornoydBeh3mnd = RegData$SRS22_21,
                      fornoydBeh2aar = RegData$SRS22_21_patient12mths)

    grtxt <- c("Svært fornøyd","Ganske fornøyd","Verken eller","Litt misfornøyd","Svært misfornøyd","Ikke svart")
    gr <- c(1:5,9)
    RegData <- RegData[which(RegData$VariabelGr %in% gr), ]
    retn <- 'H'

    if (figurtype=='fordeling') {
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = gr)
      }

    if (figurtype == 'andeler') {
      RegData <- RegData[RegData$VariabelGr %in% 1:5, ]
      RegData$Variabel[RegData$VariabelGr %in% 5:4] <- 1
      varTxt <- 'fornøyde'
      }
    tittel <- switch(valgtVar,
                     fornoydBeh3mnd = 'Fornøyd med resultatet av behandlingen, 3 mnd' ,
                     fornoydBeh2aar = 'Fornøyd med resultatet av behandlingen, 2 år')
    sortAvtagende <- TRUE
  }


  # ---------------------------- Eksempler fra nakke: SKAL FJERNES! :-) --------------------
  if (valgtVar %in% c('FornoydBeh3mnd','FornoydBeh12mnd')) { #Andeler #AndelGrVar #AndelTid
    #3/12mndSkjema. Andel med Fornøyd/litt fornøyd (1,2)
    #Kode 1:5,9: 'Fornøyd', 'Litt fornøyd', 'Verken eller', 'Litt misfornøyd', 'Misfornøyd', 'Ukjent')
    grtxt <- c('Fornøyd', 'Litt fornøyd', 'Verken eller', 'Litt misfornøyd', 'Misfornøyd', 'Ukjent')
    RegData <- switch(valgtVar,
                      FornoydBeh3mnd = RegData[which(RegData$OppFolgStatus3mnd==1), ],
                      FornoydBeh12mnd = RegData[which(RegData$OppFolgStatus12mnd==1), ])
    indDum <- which(RegData[ ,valgtVar] %in% 1:5)
    retn <- 'H'
    RegData$VariabelGr <- 9
    RegData$VariabelGr[indDum] <- RegData[indDum, valgtVar]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5,9))
    if (figurtype %in% c('andelGrVar', 'andelTid' )) {
      RegData <- RegData[indDum, ]
      RegData$Variabel[which(RegData[ ,valgtVar] %in% 1:2)] <- 1
      varTxt <- 'fornøyde'
      tittel <- switch(valgtVar,
                       FornoydBeh3mnd = 'Fornøyd med behandlinga på sykehuset, 3 mnd' ,
                       FornoydBeh12mnd = 'Fornøyd med behandlinga på sykehuset, 12 mnd')
    }
    tittel <- switch(valgtVar,
                     FornoydBeh3mnd = 'Fornøyd med behandlinga på sykehuset, 3 mnd' ,
                     FornoydBeh12mnd = 'Fornøyd med behandlinga på sykehuset, 12 mnd')
    sortAvtagende <- TRUE
  }


    if (valgtVar=='Alder') {	#Fordeling, #AndelGrVar,AndelTid, GjsnGrVar, GjsnTid
    RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
    xAkseTxt <- 'alder (år)'
    tittel <- 'Alder ved innleggelse'
    if (figurtype %in% c('andelTid', 'andelGrVar')) {
      #Pasienter over 70 år
      RegData$Variabel[which(RegData$Alder >= 70)] <- 1
      varTxt <- 'pasienter >=70år'
      tittel <- 'Pasienter over 70 år'
    }
    if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) {
      RegData$Variabel <- RegData$Alder  	#GjsnTid, GjsnGrVar
	  xaksetxt <- 'alder (år)'
	  deltittel <- 'alder ved innleggelse'}
    if (figurtype == 'andeler') {	#Fordelingsfigur
      gr <- c(seq(0, 100, 10),150)
      gr <- c(0,seq(20,90,10),150)
      RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)
      grtxt <- c('0-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90+')	#c(levels(RegData$VariabelGr)[-length(gr)], '90+')	#c(names(AndelLand)[-length(gr)], '90+')
      xAkseTxt <- 'Aldersgrupper (år)'}
    sortAvtagende <- FALSE
    KIekstrem <- c(0,110)

  }


  if (valgtVar=='ASAgrad') {#Andeler,  #AndelGrVar  #AndelTid
    #Legeskjema. Andel av de som har ASA-grad 3-5
    grtxt <- c('I:Ingen','II:Moderat', 'III:Alvorlig', 'IV:Livstruende', 'V:Døende', 'Ukjent')
    indDum <- which(RegData[, valgtVar] %in% 1:5)
    if (figurtype %in% c('andelGrVar', 'andelTid' )) {
      RegData <- RegData[indDum,]
      RegData$Variabel[which(RegData[ ,valgtVar] %in% 3:5)] <- 1
      varTxt <- 'med ASA>II'
      tittel <- 'Pasienter med ASA-grad III-V'
    }
    if (figurtype == 'andeler') {
      RegData$VariabelGr <- 99
      RegData$VariabelGr[indDum] <- RegData[indDum, valgtVar]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5,99))
      tittel <-  'ASA-grad (komorbiditet)'
    }
    xAkseTxt <- 'Sykdomsgrad'
  }

  if (valgtVar=='BMI') { #Andeler #AndelGrVar  #AndelTid
    #Pasientskjema.
    RegData <- RegData[intersect(which(RegData$PasientSkjemaStatus == 1), which(RegData$BMI > 0)), ]
    gr <- c(-1, 0, 18.5, 25, 30, 1000)
    RegData$VariabelGr <- -1
    RegData$VariabelGr <- cut(RegData[,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c('', '<18,5', levels(RegData$VariabelGr)[3:(length(gr)-2)],'30+')
    grtxt2 <- c('Ukjent', 'Undervekt', 'Normalvekt', 'Overvekt', 'Fedme')
    xAkseTxt <- "Body Mass Index"
    tittel <-  'Pasientenes BMI (Body Mass Index)'
    if (figurtype %in% c('andelGrVar', 'andelTid' )) {
      RegData$Variabel[which(RegData[ ,valgtVar] >30)] <- 1
      varTxt <- 'med BMI>30'
      tittel <- 'Pasienter med fedme  (BMI>30)'
    }
  }

    if (valgtVar=='EnhverKompl3mnd') { #AndelGrVar  #AndelTid
    #Pasientskjema. Alle komplikasjoner, 3mnd.
    indSkjema <- which(RegData$OppFolgStatus3mnd == 1)
    RegData <- RegData[intersect(which(RegData[,valgtVar] %in% 0:1), indSkjema), ]
    RegData$Variabel <- RegData[ ,valgtVar]
    varTxt <- 'komplikasjoner'
    tittel <- 'Komplikasjoner (totalt) 3 mnd. etter operasjon'
  }
  if (valgtVar == 'EqAngstPreOp') { #Andeler
    RegData <- RegData[which(RegData$PasientSkjemaStatus == 1), ]
    grtxt <- c('Ingen', 'Litt', 'Mye', 'Ukjent')
    RegData$VariabelGr <- 9
    indDum <- which(as.numeric(RegData$EqAngstPreOp) %in% 1:3)
    RegData$VariabelGr[indDum] <- RegData$EqAngstPreOp[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:3,9))
    xAkseTxt <- 'Grad av engstelighet/deprimerthet'	#Tilstand i forhold til angst'
    tittel <-  'Problemer med angst/depresjon'
  }
  if (valgtVar=='ErstatningPreOp') { #Andeler #AndelGrVar #AndelTid
    #Pasientskjema. Andel med ErstatningPreOp 1 el 3
    #Kode 1:4,9: 'Ja', 'Nei', 'Planlegger', 'Innvilget', 'Ukjent'
    RegData <- RegData[which(RegData$PasientSkjemaStatus == 1), ]
    grtxt <- c('Ja', 'Nei', 'Planlegger', 'Innvilget', 'Ukjent')
    tittel <- 'Har søkt/planlegger å søke erstatning før operasjon'
    indDum <- which(RegData$ErstatningPreOp %in% 1:4)
    RegData$VariabelGr <- 9
    RegData$VariabelGr[indDum] <- RegData$ErstatningPreOp[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:4,9))
    if (figurtype %in% c('andelGrVar', 'andelTid' )) {
      RegData <- RegData[indDum, ]
      RegData$Variabel[which(RegData[ ,valgtVar] %in% c(1,3))] <- 1
      varTxt <- 'søkt erstatning'
    }
  }


  if (valgtVar == 'Inngrep'){
    grtxt <- c('Ikke klassifiserbar', 'Fremre diskektomi, prolaps', 'Bakre dekompresjon',
               'Fremre dekomp. SS u/prolaps', 'Bakre fusjon', 'Korporektomi', 'Andre inngrep') #for verdiene 0:6
    RegData <- RegData[which(RegData$Inngrep %in% 0:6),]
    RegData$VariabelGr <- factor(RegData$Inngrep, levels = 0:6)
    tittel <- 'Inngrepstyper'
    retn <- 'H'
  }
if (valgtVar=='KnivtidTotalMin') { #GjsnTid #GjsnGrVar#Legeskjema.
		RegData <- RegData[which(RegData[ ,valgtVar]>0), ]
		RegData$Variabel <- RegData[ ,valgtVar]
		KIekstrem <- c(0, 500)
		tittel <- 'Total knivtid'
		ytxt1 <- '(minutter)'
	deltittel <- 'total knivtid'
	xAkseTxt <- 'minutter'
	}


  if (valgtVar=='KomplinfekDyp3mnd') { #AndelGrVar, AndelTid
    #3MndSkjema. Andel med KomplinfekDyp3mnd=1
    #Kode 0,1: Nei, Ja +tomme
    RegData <- RegData[intersect(which(RegData$OppFolgStatus3mnd == 1), which(RegData$KomplinfekDyp3mnd %in% 0:1)), ]
    RegData$Variabel <- RegData[ ,valgtVar]
    varTxt <- 'dype infeksjoner'
    tittel <- 'Pasientrapportert dyp infeksjon, 3 mnd.'
  }
  if (valgtVar=='KomplinfekOverfl3mnd') { #AndelGrVar, AndelTid
    #3MndSkjema. Andel med KomplinfekOverfl3mnd=1
    #Kode 0,1: Nei, Ja +tomme
    RegData <- RegData[intersect(which(RegData$OppFolgStatus3mnd == 1), which(RegData$KomplinfekOverfl3mnd %in% 0:1)), ]
    RegData$Variabel <- RegData[ ,valgtVar]
    varTxt <- 'overfladiske infeksjoner'
    tittel <- 'Overfladisk infeksjon, 3 mnd.'
  }
  if (valgtVar=='Komplinfek') { #AndelTid, #AndelGrVar
    #3MndSkjema. Andel med KomplinfekDyp3mnd=1
    #Kode 0,1: Nei, Ja +tomme
    ind <- intersect(which(RegData$OppFolgStatus3mnd == 1),
      union(which(RegData$KomplinfekDyp3mnd %in% 0:1), which(RegData$KomplinfekOverfl3mnd %in% 0:1)))
    RegData <- RegData[ind, ]
    RegData$Variabel[union(which(RegData$KomplinfekDyp3mnd==1), which(RegData$KomplinfekOverfl3mnd==1))] <- 1
    varTxt <- 'infeksjoner'
    tittel <- 'Pasientrapportert dyp eller overfladisk infeksjon, 3 mnd.'
    }

  if (valgtVar=='KomplStemme3mnd') { #AndelTid, #AndelGrVar
    #3MndSkjema. Andel med KomplStemme3mnd=1
    #Kode 0,1: Nei, Ja +tomme
    RegData <- RegData[which(RegData$OppFolgStatus3mnd == 1) %i%
                         which(RegData$KomplStemme3mnd %in% 0:1) %i%
                         which(RegData$OprMetodeTilgangFremre==1) # %i%which(RegData$OprIndikMyelopati==0)
                       , ]
    RegData$Variabel <- RegData[ ,valgtVar]
    varTxt <- 'med stemmevansker'
    tittel <- 'Stemmevansker, fremre tilgang, 3 mnd.'
  }
  if (valgtVar=='KomplStemme12mnd') { #AndelGrVar,
    #3MndSkjema. Andel med KomplStemme12mnd=1
    #Kode 0,1: Nei, Ja +tomme
    RegData <- RegData[which(RegData$OppFolgStatus12mnd == 1) %i%
                         which(RegData$KomplStemme12mnd %in% 0:1) %i%
                         which(RegData$OprMetodeTilgangFremre==1), ]
    RegData$Variabel <- RegData[ ,valgtVar]
    tittel <- 'Stemmevansker, fremre tilgang, 12 mnd.'
  }



  if (valgtVar=='LiggeDognTotalt') { #Andeler #GjsnTid #GjsnGrVar
    #Legeskjema.
	#For opphold registrert som dagkirurgi uten at liggedogn er reg., settes liggedogn=0
    #dagind <- which( (is.na(RegData$Liggedogn) | is.nan(RegData$Liggedogn))  & RegData$Dagkirurgi==1)
    #RegData$Liggedogn[dagind]<-0
    tittel <- 'Totalt antall liggedøgn'
	RegData <- RegData[which(RegData[ ,valgtVar]>-1), ]
		RegData$Variabel <- RegData[ ,valgtVar]
    gr <- c(0:7,100)
    RegData$VariabelGr <- cut(RegData$LiggeDognTotalt, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c(0:6, '7+')
		KIekstrem <- c(0, 30)
		ytxt1 <- '(døgn)'
	deltittel <- 'antall liggedøgn, totalt'
	xAkseTxt <- 'dager'
	}

  if (valgtVar == 'Morsmal') { #Andeler
    RegData <- RegData[which(RegData$PasientSkjemaStatus == 1), ]
    grtxt <- c('Norsk', 'Samisk', 'Annet', 'Ukjent')
    RegData$VariabelGr <- 9
    indDum <- which(RegData$Morsmal %in% 1:3)
    RegData$VariabelGr[indDum] <- RegData$Morsmal[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:3,9))
    tittel <- 'Morsmål'
  }

	if (valgtVar=='NDIscorePreOp') { #GjsnTid #GjsnGrVar
		#Pasientkjema.
	  RegData <- RegData[which(RegData$PasientSkjemaStatus == 1), ]
	  KIekstrem <- c(0,100)
		indVar <- which(RegData[ ,valgtVar] >= KIekstrem[1])
		indSkjema <- which(RegData$PasientSkjemaStatus==1)
		RegData <- RegData[intersect(indVar, indSkjema), ]
		RegData$Variabel <- RegData[ ,valgtVar]
		tittel <- 'NDI før operasjon'
		ytxt1 <- '(NDI-skåring)'
	deltittel <- 'NDI før operasjon'
	xAkseTxt <- 'skåring'
	}


	if (valgtVar=='NDIendr12mnd') { #GjsnTid, GjsnGrVar
		#Pasientkjema og 12mndskjema. Lav skår, lite plager -> forbedring = nedgang.
		KIekstrem <- c(-100,100)
		RegData$Variabel <- RegData$NDIscorePreOp - RegData$NDIscore12mnd
		indVar <- which(RegData$Variabel >= KIekstrem[1])
		indSkjema <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus12mnd==1)
		RegData <- RegData[intersect(indVar, indSkjema), ]
		tittel <- 'Forbedring av NDI, 12 mnd. etter operasjon'
		deltittel <- 'forbedring av NDI, 12 mnd. etter operasjon'
 		ytxt1 <- '(endring av NDI-skår)'
 		sortAvtagende <- TRUE
		}

	if (valgtVar=='NDIendr3mnd') { #GjsnTid, GjsnGrVar
		#Pasientkjema og 3mndskjema. Lav skår, lite plager -> forbedring = nedgang.
		KIekstrem <- c(-100,100)
		RegData$Variabel <- RegData$NDIscorePreOp - RegData$NDIscore3mnd
		indVar <- which(RegData$Variabel >= KIekstrem[1])
		indSkjema <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus3mnd==1)
		RegData <- RegData[intersect(indVar, indSkjema), ]
		tittel <- 'Forbedring av NDI, 3 mnd. etter operasjon'
	 deltittel <- 'forbedring av NDI, 3 mnd. etter operasjon'
 	ytxt1 <- '(endring av NDI-skår)'
		}

  if (valgtVar == 'NRSsmerteArmEndr12mnd') { #AndelGrVar #GjsnGrVar
    #Pasientskjema.
    KIekstrem <- c(-10,10)
    RegData$NRSEndr <- 100*(RegData$NRSsmerteArmPreOp - RegData$NRSsmerteArm12mnd)/RegData$NRSsmerteArmPreOp
    indPas <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus12mnd==1)
    indVar <- which(is.finite(RegData$NRSEndr))
    RegData <- RegData[intersect(indPas ,indVar), ]
    RegData$Variabel[which(RegData$NRSEndr >=30)] <- 1
    tittel <- 'Minst 30% forbedring av NRS-arm, 12 mnd.'
	varTxt <- 'med NRSendr.>30%'
	if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) {
		RegData$Variabel <- RegData$NRSsmerteArmPreOp - RegData$NRSsmerteArm12mnd} #RegData$NRSEndr
	deltittel <- 'NRS, armsmerte, endring 12 mnd.'
	xAkseTxt <- 'skåring'
	sortAvtagende <- TRUE
	}
  if (valgtVar == 'NRSsmerteArmEndr3mnd') { #AndelGrVar #GjsnGrVar, GjsnTid
    #Pasientskjema.
    KIekstrem <- c(-10,10)
    RegData$NRSEndr <- 100*(RegData$NRSsmerteArmPreOp - RegData$NRSsmerteArm3mnd)/RegData$NRSsmerteArmPreOp
    indPas <- which(RegData$PasientSkjemaStatus==1 & RegData$OppFolgStatus3mnd==1)
    indVar <- which(is.finite(RegData$NRSEndr)) #Kun de som har reg både før og etter
    RegData <- RegData[intersect(indPas ,indVar), ]
    RegData$Variabel[which(RegData$NRSEndr >=30)] <- 1
    tittel <- 'Minst 30% forbedring av NRS-arm, 3 mnd.'
    if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) {
      RegData$Variabel <- RegData$NRSsmerteArmPreOp - RegData$NRSsmerteArm3mnd}
    deltittel <- 'NRS, armsmerte, endring 3 mnd.'
    tittel <- 'NRS, armsmerte, endring 3 mnd. etter'
    xAkseTxt <- 'skåring'
  }

	if (valgtVar == 'NRSsmerteArmPreOp') { #GjsnGrVar, GjsnTid
	#Pasientskjema.
	  KIekstrem <- c(0,10)
	  indPas <- which(RegData$PasientSkjemaStatus==1)
	indVar <- which(RegData[ ,valgtVar] >-1)
	RegData <- RegData[intersect(indPas ,indVar), ]
	RegData$Variabel <- RegData[ ,valgtVar]
	deltittel <- 'NRS, arm før operasjon'
	xAkseTxt <- 'skåring'
	}

  if (valgtVar %in% c('NytteOpr3mnd', 'NytteOpr12mnd')) { #Andeler #AndelTid  #AndelGrVar
    #3/12mndSkjema. Andel med helt bra/mye bedre (1:2)
    #Kode 1:7,9: ''Helt bra', 'Mye bedre', 'Litt bedre', 'Uendret', 'Litt verre', 'Mye verre',
    #				'Verre enn noen gang', 'Ukjent')
    grtxt <- c('Klart bedre', 'Små endringer', 'Klart verre', 'Ukjent')
    tittel <- switch(valgtVar,
                     NytteOpr3mnd = 'Nytte av operasjon, 3 mnd',
                     NytteOpr12mnd = 'Nytte av operasjon, 12 mnd')
    RegData <- switch(valgtVar,
                      NytteOpr3mnd = RegData[which(RegData$OppFolgStatus3mnd==1), ],
                      NytteOpr12mnd = RegData[which(RegData$OppFolgStatus12mnd==1), ])
    retn <- 'H'
    RegData$VariabelGr <- 9
    indDum <- which(RegData[ , valgtVar] %in% 1:7)
    RegData$VariabelGr[indDum] <- RegData[indDum, valgtVar]
    oldvalues <- c(1:7,9)
    newvalues <- c(1,1,2,2,2,3,3,4)
    #levels=c('Klart bedre','Klart bedre', 'Små endringer', 'Små endringer', 'Små endringer',
    # 'Klart verre', 'Klart verre', 'Ukjent'))  # Make this a factor
    RegData$VariabelGr <- factor(newvalues[ match(RegData$VariabelGr, oldvalues) ], levels=1:4)
    if (figurtype %in% c('andelGrVar', 'andelTid' )) {
      RegData <- RegData[indDum, ]
      RegData$Variabel[which(RegData[ ,valgtVar] %in% 1:2)] <- 1
      varTxt <- '"mye bedre" og "helt bra"'
      tittel <- switch(valgtVar,
                       NytteOpr3mnd = 'Helt bra eller mye bedre, 3 mnd.' ,
                       NytteOpr12mnd = 'Helt bra eller mye bedre, 12 mnd.')

    }
  }
  if (valgtVar %in% c('NytteOpr3mndAlleKat', 'NytteOpr12mndAlleKat')) { #Andeler
    #3/12mndSkjema. Andel med helt bra/mye bedre (1:2)
    #Kode 1:7,9: ''Helt bra', 'Mye bedre', 'Litt bedre', 'Uendret', 'Litt verre', 'Mye verre',
    #				'Verre enn noen gang', 'Ukjent')
    grtxt <- c('Helt bra', 'Mye bedre', 'Litt bedre', 'Uendret', 'Litt verre', 'Mye verre',
               				'Verre enn noen gang', 'Ukjent')
    RegData$Nytte <- switch(valgtVar,
                            'NytteOpr3mndAllekat' = RegData$NytteOpr3mnd,
                            'NytteOpr12mndAlleKat' =RegData$NytteOpr12mnd)
    tittel <- switch(valgtVar,
                     'NytteOpr3mndAlleKat' = 'Nytte av operasjon, 3 mnd. etter',
                     'NytteOpr12mndAlleKat' = 'Nytte av operasjon, 12 mnd. etter')
    RegData$VariabelGr <- 9
    ind <- switch(valgtVar,
                  'NytteOpr3mndAlleKat' = which(RegData$OppFolgStatus3mnd==1),
                  'NytteOpr12mndAlleKat' = which(RegData$OppFolgStatus12mnd==1))
    RegData <- RegData[ind, ]
    retn <- 'H'
    indDum <- which(RegData$Nytte %in% 1:7)
    RegData$VariabelGr[indDum] <- RegData$Nytte[indDum]
  }

  if (valgtVar %in% c('Verre3mnd','Verre12mnd')) { #AndelTid  #AndelGrVar
    #3/12mndSkjema. Andel med helt mye verre og noen sinne (6:7)
    #Kode 1:7,9: ''Helt bra', 'Mye bedre', 'Litt bedre', 'Uendret', 'Litt verre', 'Mye verre',
    #				'Verre enn noen gang', 'Ukjent')
    indSkjema <- switch(valgtVar,
                        Verre3mnd = which(RegData$NytteOpr3mnd %in% 1:7) %i% which(RegData$OppFolgStatus3mnd==1),
                        Verre12mnd = which(RegData$NytteOpr12mnd %in% 1:7) %i% which(RegData$OppFolgStatus12mnd==1))
    RegData <- RegData[indSkjema, ]
    indVar <- switch(valgtVar,
                     Verre3mnd = which(RegData$NytteOpr3mnd %in% 6:7),
                     Verre12mnd = which(RegData$NytteOpr12mnd %in% 6:7))
    RegData$Variabel[indVar] <- 1
    varTxt <- 'med klar forverring'
    tittel <- switch(valgtVar,
                     Verre3mnd = 'Mye verre/verre enn noen gang, 3 mnd.' ,
                     Verre12mnd = 'Mye verre/verre enn noen gang, 12 mnd.')
  }
  if (valgtVar == 'OperasjonsKategori') { #Andeler
    retn <- 'H'
    grtxt <- c('Elektiv', 'Øhjelp', 'Subakutt', 'Ukjent')
    indDum <- which(RegData$OperasjonsKategori %in% 1:3)
    RegData$VariabelGr <- 9
    RegData$VariabelGr[indDum] <- RegData$OperasjonsKategori[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:3,9))
    tittel <- 'Hastegrad'
  }
  if (valgtVar %in% c('Oppf3mnd', 'Oppf12mnd', 'Oppf3og12mnd')) { #AndelGrVar, -Tid
    #Oppfølgingsskjema: OppFolgStatus12mnd, OppFolgStatus3mnd
    trekkfraDager <- ifelse(valgtVar == 'Oppf3mnd', 100, 400)
    RegData <- RegData[RegData$InnDato < min(max(RegData$InnDato), Sys.Date()-trekkfraDager), ]
    ind <- switch(valgtVar,
                  Oppf3mnd = which(RegData$OppFolgStatus3mnd==1),
                  Oppf12mnd = which(RegData$OppFolgStatus12mnd==1),
                  Oppf3og12mnd = which(RegData$OppFolgStatus3mnd==1 & RegData$OppFolgStatus12mnd==1 ))

    RegData$Variabel[ind] <- 1

    tittel <- paste0('Svart på oppfølging, ',
                     switch(valgtVar,
                            Oppf3mnd = '3 mnd. etter',
                            Oppf12mnd = '12 mnd. etter',
                            Oppf3og12mnd = 'både 3 og 12 mnd. etter'))
    sortAvtagende <- T
  }

  if (valgtVar == 'regForsinkelse') {  #Fordeling, Andeler
    #Verdier: 0-3402
    RegData <- RegData[which(RegData$DiffUtFerdig > -1), ]
    tittel <- switch(figurtype,
                     andeler='Tid fra utskriving til ferdigstilt registrering',
                     andelGrVar = 'Mer enn 30 dager fra utskriving til ferdig registrering') #
    subtxt <- 'døgn'
    xAkseTxt <- 'dager'
    sortAvtagende <- FALSE
    RegData <- RegData[RegData$InnDato < min(max(RegData$InnDato), Sys.Date()-30), ]

    if (figurtype == 'andeler') {	#Fordelingsfigur
      gr <- c(seq(0,98,7), 2000)
      RegData$VariabelGr <- cut(RegData$DiffUtFerdig, breaks=gr, include.lowest=TRUE, right=FALSE)
      #plot(RegData$VariabelGr)
      grtxt <- c(1:14, '>3 mnd.')
      subtxt <- 'innen gitte uker etter utskriving'
    }

    if (figurtype %in% c('andelTid', 'andelGrVar')) {
      RegData$Variabel[which(RegData$DiffUtFerdig >90)] <- 1
      tittel <- 'Registrert for sent for 3 mnd. oppfølging'
      varTxt <- 'for sent registrert'
      sortAvtagende <- F}
    KImaalGrenser <- c(0,3,10,100)
  }

  if (valgtVar == 'SivilStatus') { #Andeler
    RegData <- RegData[which(RegData$PasientSkjemaStatus ==1), ]
    grtxt <- c('Gift', 'Samboer', 'Enslig', 'Ukjent')
    RegData$VariabelGr <- 9
    indDum <- which(RegData$SivilStatus %in% 1:3)
    RegData$VariabelGr[indDum] <- RegData$SivilStatus[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:3,9))
    tittel <- 'Sivilstatus'
  }
  if (valgtVar == 'SmertestillBrukPreOp') { #Andeler
    RegData <- RegData[which(RegData$PasientSkjemaStatus ==1), ]
    # 1 - Sjeldnere enn hver uke, 2 - Hver uke, 3 - Daglig, 4 - Flere ganger daglig, 9 - Ikke utfylt
    grtxt <- c('Aldri', 'Sjeldnere enn ukentlig', 'Ukentlig', 'Daglig', 'Flere ganger daglig', 'Ukjent')
    RegData$VariabelGr <- 9
    RegData$VariabelGr[which(RegData$SmertestillPreOp == 0)] <- 0
    indDum <- which(RegData$SmertestillBrukPreOp %in% 1:4)
    RegData$VariabelGr[indDum] <- RegData$SmertestillBrukPreOp[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0:4,9))
    tittel <- 'Hyppighet av smertestillende før operasjonen'
    retn <- 'H'
  }
  if (valgtVar == 'SmertestillPreOp') { #AndelTid  #AndelGrVar
    #PasientSkjema. Andel med SmertestillPreOp=1
    #Kode 0,1,9: Nei, Ja Ukjent
    RegData <- RegData[intersect(which(RegData$SmertestillPreOp %in% 0:1), which(RegData$PasientSkjemaStatus ==1)), ]
    RegData$Variabel <- RegData$SmertestillPreOp
    varTxt <- 'på smertestillende'
    tittel <- 'Bruker smertestillende før operasjon'
  }
  if (valgtVar == 'Snuser') { #Andeler
    RegData <- RegData[which(RegData$PasientSkjemaStatus ==1), ]
    grtxt <- c('Nei', 'Ja', 'Ukjent')
    RegData$VariabelGr <- 9
    indDum <- RegData$Snuser %in% 0:1
    RegData$VariabelGr[indDum] <- RegData$Snuser[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,9))
    tittel <- 'Bruker pasienten snus?'
  }
  if (valgtVar == 'SymptVarighetNakkeHode') { #Andeler #AndelTid #AndelGrVar
    #PasientSkjema. Andel med SymptVarighetNakkeHode 4 el 5
    #Kode 1:5,9: 'Ingen', '<3 mnd', '3-12 mnd', '1-2 år', '>2 år', 'Ukjent'
    tittel <- 'Varighet av nakke-/hodesmerter'
    grtxt <- c('Ingen', '<3 mnd', '3-12 mnd', '1-2 år', '>2 år', 'Ukjent')
    RegData <- RegData[which(RegData$PasientSkjemaStatus ==1), ]
    indDum <- which(RegData[,valgtVar] %in% 1:5)
    RegData$VariabelGr <- 9
    RegData$VariabelGr[indDum] <- RegData[indDum,valgtVar]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5,9))
    if (figurtype %in% c('andelGrVar', 'andelTid')) {
      RegData <- RegData[indDum,]
      RegData$Variabel[which(RegData[,valgtVar] %in% 4:5)] <- 1
      varTxt <- 'med varighet >1 år'
      tittel <- 'Varighet av hode-/nakkesmerter minst ett år'
    }
  }

  if (valgtVar == 'Utdanning') { #Andeler  #AndelGrVar
    #PasientSkjema. Andel med Utdanning 4 el 5
    #Kode 1:5,9: 'Grunnskole++, 7-10år','Real-, yrkes- el vg skole', 'Allmennfaglig vg skole',
    #Høyskole/universitet, <4 år', 'Høyskole/universitet, 4år+', 'Ukjent'
    grtxt <- c('Grunnskole, 7-10år','Real-, yrkes- el vg skole',
               'Allmennfaglig vg skole','Høyskole/universitet, <4 år','Høyskole/universitet, 4år+', 'Ukjent')
    tittel <- 'Utdanningsnivå'
    RegData <- RegData[which(RegData$PasientSkjemaStatus ==1), ]
    indDum <- which(RegData$Utdanning %in% 1:5)
    RegData$VariabelGr <- 9
    RegData$VariabelGr[indDum] <- RegData$Utdanning[indDum]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5,9))
    if (figurtype %in% c('andelGrVar', 'andelTid')) {
      RegData <- RegData[indDum, ]
      RegData$Variabel[which(RegData[ ,valgtVar] %in% 4:5)] <- 1
      varTxt <- 'med høyere utdanning'
      tittel <- 'Pasienter med høyskole-/universitetsutdannelse'
    }
    retn <- 'H'
  }




   #-------------- SAMMENSATTE variabler
      #For flerevar=1 må vi omdefinere variablene slik at alle gyldige registreringer
      #(dvs. alle registreringer som skal telles med) er 0 eller 1. De som har oppfylt spørsmålet
      # er 1, mens ugyldige registreringer er NA. Det betyr at hvis vi skal ta bort registreringer
      # som i kategorier av typen "Ukjent" kodes disse som NA, mens hvis de skal være med kodes de
      # som 0.
      #Vi sender tilbake alle variabler som indikatorvariable, dvs. med 0,1,NA

 # if (valgtVar %in% c('Komorbiditet', 'KomplOpr', 'Kompl3mnd', 'OprIndik', 'OprIndikSmerter',
#                      'OprIndikMyelopati', 'Radiologi')){


      if (valgtVar=='OprIndik') {
        tittel <- 'Operasjonsårsak'
        retn <- 'H'
		flerevar <-  1
        #OprIndiasjonasjonUfylt <>1 - tom variabel,
        #Svært få (ca 20 av 3000) har tom registrering. Setter derfor felles N lik alle reg.
		  #01.03.2018: Mange har tom registrering... Må derfor skille på N for de ulike variabler
		variabler <- c('OprIndikParese', 'OprIndikMyelopati', 'OprIndikSmerter', 'SmerteMyelo', 'OprIndikAnnet')
        grtxt <- c('Pareser', 'Myelopati', 'Smerter', 'Sm. og Myelop.', 'Annet')
        #Smerter og Myelopati
		indSmerter <- which(RegData$OprIndikSmerter == 1)
    indMyelopati <- which(RegData$OprIndikMyelopati == 1)
		RegData$SmerteMyelo <- NA
		RegData$SmerteMyelo[(RegData$OprIndikSmerter %in% 0:1) & (RegData$OprIndikMyelopati %in% 0:1)] <- 0
    RegData$SmerteMyelo[ (RegData$OprIndikSmerter == 1) & (RegData$OprIndikMyelopati == 1)] <- 1
            # ind01 <- which(RegData[ ,variabler] %in% 0:1, arr.ind = T) #Alle ja/nei
            # ind1 <- which(RegData[ ,variabler] == 1, arr.ind=T) #Ja i alle variabler
            # RegData[ ,variabler] <- NA
            # RegData[ ,variabler][ind01] <- 0
            # RegData[ ,variabler][ind1] <- 1
            # xAkseTxt <- 'Andel opphold (%)'
            #
      }

      if (valgtVar=='Radiologi') {
        retn <- 'H'
        flerevar <- 1
        #RadilogiUnderokelseUfylt  - tom variabel,
        #RadiologiRtgCcolFunkOpptak  - tom variabel,
        #Svært få har tom registrering. Setter derfor felles N lik alle reg.
        tittel <- 'Radiologisk undersøkelse'
        grtxt <- c('CT', 'MR', 'Myelografi', 'Røntgen-Ccol')
		variabler <- c('RadiologiCt', 'RadiologiMr', 'RadiologiMyelografi', 'RadiologiRtgCcol')
           ind01 <- which(RegData[ ,variabler] != -1, arr.ind = T) #Alle ja/nei
            # ind1 <- which(RegData[ ,variabler] == 1, arr.ind=T) #Ja i alle variabler
            # RegData[ ,variabler] <- NA
            # RegData[ ,variabler][ind01] <- 0
            # RegData[ ,variabler][ind1] <- 1
            xAkseTxt <- 'Andel opphold (%)'
      }

      if (valgtVar=='Komorbiditet') {
        tittel <- 'Komorbiditet'
        retn <- 'H'
        flerevar <- 1
        RegData <- RegData[which(RegData$AndreRelSykdommer %in% 0:1), ] #Alle videre variabler utfylt
        variabler <- c('SykdReumatisk','SykdAnnenendokrin', 'SykdAnnet','SykdCarpalTunnelSyndr', 'SykdCerebrovaskular',
                      'SykdDepresjonAngst', 'SykdHjertekar', 'SykdHodepine', 'SykdHypertensjon',
					  'SykDiabetesMellitus', 'SykdKreft', 'SykdKroniskLunge', 'SykdKroniskNevrologisk',
					  'SykdKrSmerterMuskelSkjelSyst', 'SykdOsteoporose', 'SykdSkulderImpigment',
					  'SykdWhiplashNakke', 'AndreRelSykdommer')
        grtxt <- c('Annen Reumatisk', 'Annen endokrin', 'Andre', 'Carpal TS', 'Cerebrovaskulær',
					'Depresjon/Angst', 'Hjerte-/Karsykd.', 'Hodepine', 'Hypertensjon',
					'Diabetes', 'Kreft', 'Kr. lungesykdom', 'Kr. nevrologisk',
					'Kr. muskel/skjelettsm.', 'Osteoporose', 'Skuldersyndrom',
					'Whiplash/skade', 'Tot. komorb')
        RegData$SykdReumatisk <- 0
		    indSykdReumatisk <- (RegData$SykdAnnenreumatisk ==1 | (RegData$SykdBechtrew==1 | RegData$SykdReumatoidartritt==1))
        RegData$SykdReumatisk[indSykdReumatisk] <- 1
        # ind01 <- which(RegData[ ,variabler] != -1, arr.ind = T) #Alle ja/nei
        # ind1 <- which(RegData[ ,variabler] == 1, arr.ind=T) #Ja i alle variabler
        # RegData[ ,variabler] <- NA
        # RegData[ ,variabler][ind01] <- 0
        # RegData[ ,variabler][ind1] <- 1
      }

      if (valgtVar=='KomplOpr') {
        tittel <- 'Komplikasjoner ved operasjon'
        retn <- 'H'
        flerevar <- 1
        variabler <- c('PerOpKomplAnafylaksiI','PerOpKomplAnnet','PerOpKomplBlodning','PerOpKomplDurarift',
                      'PerOpKomplFeilplasseringImplant','PerOpKomplKardioVaskulare','PerOpKomplMedullaskade',
                      'PerOpKomplNerverotSkade','PerOpKomplAnnenNerveskade','PerOpKomplOpFeilNivaa',
                      'PerOpKomplRespiratorisk','PerOpKomplOsofagusSkade','PerOpEnhverKompl')
       grtxt <- c('Anafylaksi','Annet','Blødning','Durarift','Feilplassering, impl.','Kardiovaskulære','Medullaskade',
                   'Nerverotskade','Nerveskade','Op. feil nivå','Respiratorisk','Øsofagusskade','Komplikasjoner, alle')
      }

      if (valgtVar=='Kompl3mnd') {
        tittel <- 'Komplikasjoner 3 mnd. etter operasjon'
        retn <- 'H'
        flerevar <- 1
        RegData <- RegData[which(RegData$OppFolgStatus3mnd == 1), ]
        variabler <- c('KomplDVT3mnd', 'KomplinfekDyp3mnd', 'KomplLungeEmboli3mnd', 'KomplinfekOverfl3mnd',
                      'KomplPneumoni3mnd', 'KomplStemme3mnd', 'KomplSvelging3mnd', 'KomplUVI3mnd', 'KomplKraftsvikt3mnd',
                      'EnhverKompl3mnd')
        grtxt <- c('DVT', 'Dyp infeksjon', 'Lungeemboli', 'Overfladisk infeksjon',
                   'Pneumoni', 'Stemmevansker', 'Svelgevansker', 'UVI', 'Kraftsvikt', 'Totalt, 3 mnd.')
      }

      if (valgtVar=='OprIndikSmerter') {
        tittel <- 'Operasjonsårsak: Smerter'
        retn <- 'H'
        flerevar <- 1
        RegData <- RegData[which(RegData$OprIndikSmerter %in% 0:1), ]
        variabler <- c('OprIndikSmerter', 'OprIndikSmerteLokArm', 'OprIndikSmerteLokNakke', 'SmerteArmNakke' )
        grtxt <- c('Smerter', '...Arm', '...Nakke', '...Arm og Nakke')
        RegData$SmerteArmNakke <- 0
        RegData$SmerteArmNakke[RegData$OprIndikSmerteLokArm == 1 & RegData$OprIndikSmerteLokNakke == 1] <- 1
      }

      if (valgtVar=='OprIndikMyelopati') {
        tittel <- 'Operasjonsårsak: Myelopati'
        retn <- 'H'
        flerevar <- 1
        RegData <- RegData[which(RegData$OprIndikMyelopati %in% 0:1), ]
        variabler <- c('OprIndikMyelopati', 'OprIndikMyelopatiMotorisk', 'OprIndikMyelopatiSensorisk',
                      'MotorSensor')
        grtxt <- c('Myelopati', '...Motorisk', '...Sensorisk', '...Begge deler')
        RegData$MotorSensor <- 0
        RegData$MotorSensor[RegData$OprIndikMyelopatiMotorisk & RegData$OprIndikMyelopatiSensorisk] <- 1
      }



    UtData <- list(RegData=RegData, grtxt=grtxt, varTxt=varTxt, xAkseTxt=xAkseTxt,
                   tittel=tittel, varTxt=varTxt, flerevar=flerevar, #KImaalGrenser=KImaalGrenser,
                   variabler=variabler, sortAvtagende=sortAvtagende,
                 retn=retn, ytxt1=ytxt1, deltittel=deltittel, KIekstrem=KIekstrem)
  #RegData inneholder nå variablene 'Variabel' og 'VariabelGr'
  return(invisible(UtData))

}
