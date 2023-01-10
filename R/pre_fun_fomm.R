#'@title function for cov analysis xFOMM
#'
#'@import pMineR
#'@import graphics
#'@import stats


# covariate<-colnames(all.data[[1]])[15]


#PARAM INPUT
#passingThrough-->array,
#passingNotThrough-->array,
#arr.from
#lst.to


pre_fun_fomm<-function(ObjDL,
                       FOMM,
                       covariate,
                       arr.from,
                       lst.to,
                       lst.passingThrough=c(),
                       lst.passingNotThrough=c(),
                       covariate.type ='attribute',
                       is.numerical=TRUE
                       # abs.threshold=NA,
                       # baseline da aggiungere quando inseriamo logica plot puntuale
){


  out<-ObjDL$getData()
  MM.pat.process<-out$pat.process
  MM.csv.parameters<-list()
  MM.csv.parameters$csv.column.names <- out$csv.column.names
  MM.csv.parameters$csv.IDName <- out$csv.IDName
  MM.csv.parameters$csv.EVENTName <- out$csv.EVENTName
  MM.csv.parameters$csv.dateColumnName <- out$csv.dateColumnName
  MM.csv.parameters$csv.date.format <- out$csv.date.format
  MM.csv.parameters$csv.max.pMineR.internal.ID.Evt <- out$csv.max.pMineR.internal.ID.Evt


  ######################################################## CONTROLLI FORMALI #################################################################################

  #1) check se numero di input == numero di output
  if(length(lst.to)!=0 & (length(arr.from) != length(lst.to))){
    stop('Il numero di nodi from deve essere uguale al numero di array di nodi to')
  }


  if(identical(arr.from,character(0))){
    stop('non ci sono nodi di input che superano la soglia')
  }



  ###################################### CREAZIONE STRUTTURA DATI ###########################################################################################

  # distinguiamo 3 casi: A) non ho lst.to                 --> non ho nodi di end, non posso tracciare percorsi,
  #                                                           verranno plottati solo GRAFICI PUNTUALI (sia numerical che categorical)

  #                      B) ho list.to e cov numerica     --> posso polottare andamento temporale della variabile categorica nei path
  #                                                           individuati da: arr.from[i](=nodo di start) -> lst.to[[i]][j](=nodo di end)
  #                                                           con --> i=1:n (n= #nodi di start)   e  j=1:m (m #nodi di end riferiti all'i-esimo nodo di start)
  #
  #                      C) ho list.to e cov categorica   --> Errore: con varibili categoriche posso rientrare solo in caso A

  if(length(lst.to)==0){
    #caso A) GRAFICI PUNTUALI
    df_tot<-NULL


  }else if(length(lst.to)!=0 & is.numerical){
    #CASO B) GRAFICI ANDAMENTALI
    #input array start list end
    tmp.start<-lapply(1:length(arr.from), function(ind.fromState){
      fromState<-arr.from[ind.fromState]

      #a partire dall'i-esimo evento di start calcolo tutti i suoi path
      tmp.end<-lapply(1:length(lst.to[[ind.fromState]]), function(ind.toState){
        toState<-lst.to[[ind.fromState]][ind.toState]

        #a questo punto ho il path che inizia con from state e termina con toState:
        #in res sto salvando tutti gli id che seguono il path che sto esaminando
        res1 <- lapply( MM.pat.process , function(x)  {
          wrklst <- list()
          wrklst$inPath <- FALSE
          wrklst$fromRow <- NA
          wrklst$toRow <- NA
          eventsInPath <- c()

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
          arr.passingThrough<-lst.passingThrough[[ind.fromState]][lst.passingThrough[[ind.fromState]]!=""]
          arr.passingNotThrough<-lst.passingNotThrough[[ind.fromState]][lst.passingNotThrough[[ind.fromState]]!=""]

          if( !is.na(wrklst$fromRow) & !is.na(wrklst$toRow)  ) {
            if( FALSE %in% (arr.passingThrough %in% eventsInPath) & length(arr.passingThrough)>0 ) {
              possibleCandidate <- FALSE
            }
            if( TRUE %in% (arr.passingNotThrough %in% eventsInPath) & length(arr.passingNotThrough)>0 ) {
              possibleCandidate <- FALSE
            }
          }

          # if( length(withPatientID) > 0 ) {
          #   if( !(unique(x[,MM.csv.parameters$csv.IDName]) %in% withPatientID) ) {
          #     possibleCandidate <- FALSE
          #   }
          # }

          lista.res <- list( "eligible" = possibleCandidate,
                             eventsInPath)
          return(lista.res)
        })

        #qui ho tutti gli id
        matrice.id <- c()
        for( ID in names(res1) ) {
          if( res1[[ID]]$eligible == TRUE ) {
            matrice.id <- rbind( matrice.id, c(ID, res1[[ID]]$deltaT, res1[[ID]]$event.censored ))
          }
        }

        if(is.null(matrice.id)){
          df.cov.id<-list()

        }else{
          df.cov.id<-lapply(1:nrow(matrice.id), function(i){
            row_to_sel<-which(MM.pat.process[[matrice.id[i]]][MM.csv.parameters$csv.EVENTName]==fromState |
                                MM.pat.process[[matrice.id[i]]][MM.csv.parameters$csv.EVENTName]==toState )
            sub.path<-MM.pat.process[[matrice.id[i]]][min(row_to_sel):max(row_to_sel),]
            sub.path$pMineR.deltaDate<-sub.path$pMineR.deltaDate-sub.path$pMineR.deltaDate[1]
            sub.path<-sub.path[which(!is.na(sub.path[covariate])),c(MM.csv.parameters$csv.IDName, covariate,"pMineR.deltaDate")]
            return(sub.path)
          })



        }

        #df totale con tutti i paz dell'i-esimo path
        df.cov.tot <- do.call('rbind', df.cov.id)
        df.cov.tot$NODO_IN<-paste("path",ind.fromState,":",fromState)
        df.cov.tot$NODO_OUT<-toState
        return(df.cov.tot)
      })

      tmp.end.tot<-do.call('rbind', tmp.end)
      return(tmp.end.tot)
    })



    df_tot<-do.call('rbind',tmp.start)
    #controllo per capire se ci sono paz che percorrono almeno uno dei path inseriti
    if(nrow(df_tot)>1){
      if(class(df_tot[[covariate]])=="factor"){
        df_tot[[covariate]]<-as.numeric(levels(df_tot[[covariate]]))[df_tot[[covariate]]]
      }else{
        df_tot[[covariate]] <- as.numeric(df_tot[[covariate]])
      }
      colnames(df_tot) <- c('ID', 'covariate', 'time','nodo_IN', 'nodo_OUT')
    }else{
      df_tot<-NULL
    }








  }else{
    #CASO C) GRAFICI ANDAMENTALI PER VARIABILI CATEGORICHE:ERRORE
    df_tot<-NULL
  }
  return(df_tot)



}












