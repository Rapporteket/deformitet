#' Søylediagram med andeler for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med andeler av en variabel for en valgt
#' grupperingsvariabel, foreløpig bare sykehus.
#' Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable
#' enn sykehus
#'
#' @param Ngrense Minste antall registreringer for at ei gruppe skal bli vist
#' @param valgtVar Variabelen det skal vises resultat for.
#' @param RegData dataramme
#' @param hentData hente data? 0-nei, 1-ja
#' @param preprosess preprosessere data? 0-nei, 1-ja
#' @param minald minimum alder
#' @param maxald maksimum alder
#' @param datoFra startdato
#' @param datoTil sluttdato
#' @param erMann kjønn 1=mann, 0=kvinne
#' @param reshID avdelingas reshid
#' @param outfile Format på figurfila: pdf, png,...
#' ''-standard, viser figur på skjerm
#' @param ... mulige tilleggsvariabler
#'
#' @return Figur som viser andel av en variabel for gitt grupperingsvariabel.
#' Grupperingsvariabelen (grVar) er som regel den enkelte sykehusenhet.
#'
#' @export

figAndelerGrVar <- function(RegData=0, hentData=0, preprosess=0,
                            valgtVar='liggetidPostOp', minald=0, maxald=130, erMann=9,
                            datoFra='2023-01-01', datoTil=Sys.Date(),
                            Ngrense=10, reshID=0, outfile='',...) {

  if (hentData == 1) {
    RegData <- alleRegData(egneVarNavn = 0)
  }

  # Preprosessere data
  if (preprosess==1){
    RegData <- preprosData(RegData=RegData, egneVarNavn = 0)
  }

  '%i%' <- intersect
  #----------- Figurparametre ------------------------------

  DeformVarSpes <- varTilrettelegg(RegData=RegData, valgtVar=valgtVar,
                                   figurtype = 'andeler')
  RegData <- DeformVarSpes$RegData
  sortAvtagende <- DeformVarSpes$sortAvtagende
  Tittel <- DeformVarSpes$tittel
  KImaalGrenser <- NA

  #Gjør utvalg
  DeformUtvalg <- utvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil,
                                minald=minald, maxald=maxald, erMann=erMann)
  RegData <- DeformUtvalg$RegData
  utvalgTxt <- DeformUtvalg$utvalgTxt
  fargepalett <- DeformUtvalg$fargepalett
  grVar <- 'ShNavn'

  if(dim(RegData)[1] > 0) {
    RegData <- RegData[which(RegData[ ,grVar] != ''),] #Tar ut registreringer uten grupperingsnavn
    RegData[ ,grVar] <- as.factor(RegData[ ,grVar])
    Ngr <- table(RegData[ ,grVar])
    Ngrtxt <- as.character(Ngr)
  }	else {Ngr <- 0}

  N <- dim(RegData)[1]
  AntGr <- length(which(Ngr >= Ngrense))	#Alle som har gyldig resultat
  AndelHele <- round(100*sum(RegData$Variabel)/N, 2)
  Nvar <- tapply(RegData$Variabel, RegData[ ,grVar], sum, na.rm=T)
  AndelerGr <- round(100*Nvar/Ngr,2)

  GrNavn <- names(Ngr)
  xAkseTxt <- "Andel opphold (%)"

  indGrUt <- which(Ngr < Ngrense)
  if (sum(indGrUt)>0) {
    AndelGrUt <- NA #sum(AndelerGr[indGrUt]*Ngr[indGrUt], na.rm = T)/sum(Ngr[indGrUt])
    AndelerGr <- c(AndelerGr[-indGrUt],AndelGrUt)
    GrUtNavn <- paste0(length(indGrUt), ' avd. med N<',Ngrense)
    Ngrtxt <- c(Ngr[-indGrUt],sum(Ngr[indGrUt]))
    GrNavn <- c(GrNavn[-indGrUt], GrUtNavn)
  }

  if (valgtVar == 'liggetidPostOp' ) {KImaalGrenser <- c(0,90,100)}
  if (valgtVar == 'fornoydBeh2aar' ) {KImaalGrenser <- c(0,70,90,100)}

sortInd <- order(as.numeric(AndelerGr), decreasing=sortAvtagende, na.last = FALSE)
AndelerGrSort <- c( NA, AndelerGr[sortInd])
GrNavnSort <- GrNavn[sortInd]
Ngrtxt <- Ngrtxt[sortInd]

andeltxtUsort <- paste0(sprintf('%.1f',AndelerGr), ' %')
andeltxt <- c('', andeltxtUsort[sortInd])
GrNavnOgAnt <- c('', paste0(GrNavnSort,' (',Ngrtxt , ')'))


FigDataParam <- list(AggVerdier=AndelerGrSort,
                     AggTot=AndelHele,
                     N=N,
                     Ngr=as.numeric(Ngrtxt),
                     Nvar=Nvar[sortInd],
                     soyletxt=andeltxt,
                     grtxt=GrNavnSort,
                     Tittel=Tittel,
                     utvalgTxt=utvalgTxt,
                     fargepalett =fargepalett
)


#-----------Figur---------------------------------------
if 	( max(Ngr) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
  FigTypUt <- rapFigurer::figtype(outfile)
  farger <- FigTypUt$farger
  plot.new()
  if (!is.null(dim(RegData))) { #>0
    tekst <- paste0('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene')
  } else {tekst <- 'Ingen registrerte data for dette utvalget'}
  title(main=Tittel)
  text(0.5, 0.6, tekst, cex=1.2)
  legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
  if ( outfile != '') {dev.off()}

} else {

  #--------------------------FIGUR---------------------------------------------------
  #Innparametre: ...
  #----------- Figurparametre ------------------------------
  cexShNavn <- 1.1

  FigTypUt <- rapFigurer::figtype(outfile, height=3*800, fargepalett=fargepalett)
  farger <- FigTypUt$farger
  #Tilpasse marger for å kunne skrive utvalgsteksten
  NutvTxt <- length(utvalgTxt)
  vmarg <- max(0, strwidth(GrNavnOgAnt, units='figure', cex=cexShNavn)*0.8)
  #NB: strwidth oppfører seg ulikt avh. av device...
  par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

  xmax <- min(max(AndelerGrSort, na.rm=T),100)*1.15
  pos <- rev(barplot(rev(as.numeric(AndelerGrSort)), horiz=T, border=NA, col=farger[4],
                     xlim=c(0,xmax), ylim=c(0.05, 1.25)*(length(GrNavnOgAnt)), font.main=1,
                     las=1, cex.names=cexShNavn))
  posOver <- max(pos)+0.5

  #Legge på målnivå
  if (!is.na(KImaalGrenser[1])) {
    antMaalNivaa <- length(KImaalGrenser)-1
    rekkef <- 1:antMaalNivaa
    if (sortAvtagende == TRUE) {rekkef <- rev(rekkef)}
    fargerMaalNiva <-  c('#4fc63f', '#fbf850', '#c6312a')[rekkef]
    tetth <- c(100, 70,20)[rekkef]
    maalOppTxt <- c('Høy', 'Moderat til lav', 'Lav')[rekkef]
    if (antMaalNivaa==3) {maalOppTxt[2] <- 'Moderat' }
    rect(xleft=KImaalGrenser[1:antMaalNivaa], xright=KImaalGrenser[2:(antMaalNivaa+1)],
         ybottom=0, ytop=max(pos), col = fargerMaalNiva[1:antMaalNivaa],
         density = tetth, angle = 60, border = NA)

    legend(x=1, y=posOver, yjust = 1,
           ncol=antMaalNivaa+1,
           density = c(NA, tetth),
           angle = c(NA,rep(60, antMaalNivaa)),
           fill=c('white', fargerMaalNiva[1:antMaalNivaa]),
           xpd=TRUE, border=NA, box.col='white', pt.cex=1.5,
           legend=c('Måloppnåelse:', maalOppTxt[1:antMaalNivaa])) #,
  }
  #pos <-
    rev(barplot(rev(as.numeric(AndelerGrSort)), horiz=T, border=NA, col=farger[4],
                     xlim=c(0,xmax), ylim=c(0.05, 1.4)*(length(GrNavnOgAnt)), # ylim=c(0.05, 1.25)*(length(GrNavnOgAnt)),
                     font.main=1, las=1, cex.names=cexShNavn, add=T))
  mtext('Andel (%)', side=1, line=2)

  #Linje for hele landet/utvalget:
  lines(x=rep(AndelHele, 2), y=c(0.1, max(pos)), col=farger[2], lwd=2)
  legend(x=max(AndelerGrSort, na.rm = T), y=posOver, xjust=1, yjust = 0.2,
         cex=0.95, lwd=2, col=farger[2],
         legend=paste0('Hele landet', ' (', sprintf('%.1f',AndelHele), '%), ', 'N=', N),
         bty='o', bg='white', box.col='white')
  mtext(at=pos+max(pos)*0.0045, GrNavnOgAnt,
        side=2, las=1, cex=cexShNavn, adj=1, line=0.25)	#Legge på navn som eget steg


  title(Tittel, line=1, font.main=1, cex.main=1.3)

  text(x=AndelerGrSort+xmax*0.01, y=pos+0.1, andeltxt,
       las=1, cex=0.9, adj=0, col=farger[1])	#Andeler, hvert sykehus

  mtext(at=posOver-0.5, paste0('(N)' ), side=2, las=1, adj=1, line=0.25)

  #Tekst som angir hvilket utvalg som er gjort
  mtext(utvalgTxt, side=3, las=1, cex=1, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

    par('fig'=c(0, 1, 0, 1))
  if ( outfile != '') {dev.off()}
  #----------------------------------------------------------------------------------
}
return(invisible(FigDataParam))
}

