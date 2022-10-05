
#'@title KM function
#'
#'@import pMineR
#'@import survival


#===========================================================
# KaplanMeier
#===========================================================
KM_FOMM<-function( fromState, toState,ObjDL,
                       passingThrough=c(), passingNotThrough=c(), stoppingAt=c(),
                       stoppingNotAt=c(), PDVAt=c(), withPatientID=c() , UM="mins" )  {
  out<-ObjDL$getData()
  MM.pat.process<-out$pat.process
  MM.csv.parameters<-list()
  MM.csv.parameters$csv.column.names <- out$csv.column.names
  MM.csv.parameters$csv.IDName <- out$csv.IDName
  MM.csv.parameters$csv.EVENTName <- out$csv.EVENTName
  MM.csv.parameters$csv.dateColumnName <- out$csv.dateColumnName
  MM.csv.parameters$csv.date.format <- out$csv.date.format
  MM.csv.parameters$csv.max.pMineR.internal.ID.Evt <- out$csv.max.pMineR.internal.ID.Evt


  res <- lapply( MM.pat.process , function(x)  {
    wrklst <- list()
    wrklst$inPath <- FALSE
    wrklst$fromRow <- NA
    wrklst$toRow <- NA
    eventsInPath <- c()
    # browser()
    # Riproduci il calcolo, fra gli stati 'from' e 'to'
    for(riga in seq(1,nrow(x))) {

      # trigger the begin
      if( x[ riga , MM.csv.parameters[["csv.EVENTName"]] ] == fromState & is.na(wrklst$fromRow) ) {
        wrklst$inPath <-TRUE
        wrklst$fromRow <- riga
      }
      # trigger the end (if they have a begin)
      if( x[ riga , MM.csv.parameters[["csv.EVENTName"]] ] == toState ) {
        if(wrklst$inPath == TRUE ) {
          wrklst$inPath <- FALSE
          wrklst$toRow <- riga
        }
      }
      # trigger the PDV (if they have a begin)
      if( x[ riga , MM.csv.parameters[["csv.EVENTName"]] ] %in% PDVAt ) {
        if(wrklst$inPath == TRUE ) {
          wrklst$inPath <- FALSE
          wrklst$toRow <- riga
          eventsInPath <- c( eventsInPath ,  x[ riga , MM.csv.parameters[["csv.EVENTName"]] ] )
        }
      }
      if(wrklst$inPath == TRUE) {
        eventsInPath <- c( eventsInPath ,  x[ riga , MM.csv.parameters[["csv.EVENTName"]] ] )
      }
    }
    # ora verifica se le transizioni soddisfano le condizioni dei parametri in ingresso
    possibleCandidate <- TRUE
    if (is.na(wrklst$toRow)) {
      possibleCandidate <- FALSE
    }
    ultimoStato <- x[ nrow(x) , MM.csv.parameters[["csv.EVENTName"]] ]
    if( !is.na(wrklst$fromRow) & !is.na(wrklst$toRow)  ) {
      # browser()
      if( FALSE %in% (passingThrough %in% eventsInPath) & length(passingThrough)>0 ) {
        possibleCandidate <- FALSE
      }
      if( TRUE %in% (passingNotThrough %in% eventsInPath) & length(passingNotThrough)>0 ) {
        possibleCandidate <- FALSE
      }
      if( FALSE %in% (ultimoStato %in% stoppingAt) & length(stoppingAt)>0 ) {
        possibleCandidate <- FALSE
      }
      if( TRUE %in% (ultimoStato %in% stoppingNotAt) & length(stoppingNotAt)>0 ) {
        possibleCandidate <- FALSE
      }
    }

    if( length(withPatientID) > 0 ) {
      if( !(unique(x[,MM.csv.parameters$csv.IDName]) %in% withPatientID) ) {
        possibleCandidate <- FALSE
      }
    }

    # qui ragionare su withPatientID
    if( TRUE %in% (PDVAt %in% eventsInPath) ) {
      event.censored <- 0
    } else {
      event.censored <- 1
    }

    deltaT<-NA
    if( !is.na(wrklst$fromRow) & !is.na(wrklst$toRow) ) {
      deltaT <- x[ wrklst$toRow, "pMineR.deltaDate" ] - x[ wrklst$fromRow, "pMineR.deltaDate" ]
    }

    lista.res <- list( "eligible" = possibleCandidate,
                       "event.censored" = event.censored,
                       "deltaT" = deltaT,
                       "arr.evt" = eventsInPath,
                       "error" = 0)
    return(lista.res)
  })


  # browser()
  matrice.KM <- c()
  for( ID in names(res) ) {
    if( res[[ID]]$eligible == TRUE ) {
      matrice.KM <- rbind( matrice.KM, c(ID, res[[ID]]$deltaT, res[[ID]]$event.censored ))
    }
  }

  if(!is.null(matrice.KM)){
    # browser()
    colnames(matrice.KM)<-c("ID","time","outcome")
    matrice.KM <- as.data.frame(matrice.KM)



    ####################### INIZIO MODIFICHE #####################################
    if(class(matrice.KM$outcome)=="factor"){
      matrice.KM$outcome <- as.numeric(levels(matrice.KM$outcome))[matrice.KM$outcome]
    }else{
      matrice.KM$outcome <- as.numeric(matrice.KM$outcome)
    }

    if(class(matrice.KM$time)=="factor"){
      matrice.KM$time <- as.numeric(levels(matrice.KM$time))[matrice.KM$time]
    }else{
      matrice.KM$time <- as.numeric(matrice.KM$time)
    }
    ##################### FINE MODIFICHE ###############################################


    if( UM == "days") matrice.KM$time <- matrice.KM$time / 1440
    if( UM == "hours") matrice.KM$time <- matrice.KM$time / 60
    if( UM == "weeks") matrice.KM$time <- matrice.KM$time / (1440 * 7)
    if( UM == "months") matrice.KM$time <- matrice.KM$time / (43800)


    KM0 <- survival::survfit(survival::Surv(time, outcome)~1,   data=matrice.KM)
    to_ret<-list("table"=matrice.KM, "KM"=KM0, "ID"=matrice.KM$ID, "error"=0 )

  }else{
    to_ret<-NULL
  }

  return(to_ret)

}

