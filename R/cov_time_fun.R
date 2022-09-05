#'@title function for cov analysis
#'
#'@import pMineR
#'@import graphics
#'@import stats





cov_time_fun<-function(ObjDL,
                       ObjCFM,
                       covariate,
                       arr.from,
                       lst.to,
                       covariate.type ='attribute',
                       is.numerical=TRUE,
                       abs.threshold=NA,
                       UM="days",
                       plot.points=TRUE,
                       plot.RegressionLine=FALSE,
                       plot.ci.mean=FALSE,
                       alfa=0.05,
                       delta=10,
                       points.symbols=20,
                       size.symbols=1.5,
                       line.width=2,
                       y.int.legend=0.6,
                       legend.text.size=0.6,
                       legend.position='topleft'
                       #baseline da aggiungere quando inseriamo logica plot puntuale
                       ){

  ObjDL.out<-ObjDL$getData()
  CFMstructure<-ObjCFM$getDataStructure()
  sub.path <- ObjCFM$selectSubCohorts(arr.from,lst.to)

  ######################################################## CONTROLLI FORMALI #################################################################################

  #1) check se numero di input == numero di output
  if(length(lst.to)!=0 & (length(arr.from) != length(lst.to))){
    stop('Il numero di nodi from deve essere uguale al numero di array di nodi to')
  }

  #2) Controllo che il numero di pazienti nel nodo START sia > soglia
  if(!is.na(abs.threshold)){
    ind.to.keep<-c()
    for (i in c(1:length(arr.from))) {
      if(CFMstructure$lst.nodi[[arr.from[i]]]$hits >= abs.threshold){
        ind.to.keep<-c(ind.to.keep,i)
      }
    }
    arr.from<-arr.from[ind.to.keep]
    if(length(lst.to)!=0 ){
      lst.to<-lst.to[[ind.to.keep]]
    }
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

    #scorro su nodi from
    df_tot <- lapply(1:length(arr.from), function(ind.from){
      id.paz.from<-names(sub.path[[arr.from[ind.from]]])
      arr.node.to<-lst.to[[ind.from]]

      #scorro sugli elementi di nodi to relativi al nodo from considerato:
      #df_path è una lista che contine tanti df quanti sono i nodi di end riferiti all'i-esimo nodo start,
      # in cui per ogni paz in quel percorso riporto il valore della coordinata

      df_path<-lapply(1:length(arr.node.to), function(ind.to){

        #check nodo su stesso path

        id.to.check<-unlist(lapply(arr.node.to[-which(arr.node.to==arr.node.to[ind.to])], function(nodo){ObjCFM$findReacheableNodes(nodo)[1,]}))
        if(!arr.node.to[ind.to] %in% id.to.check){
          #per ogni percorso nodo.in-nodo.out estraggo popolazione e valore della covariata:
          #id.paz.out sono id paz nell'intersezione nodo.start-nodo.end:

          id.paz.out<-id.paz.from[which(id.paz.from %in% CFMstructure$lst.nodi[[arr.node.to[ind.to]]]$IPP)]
          #per ogni ID prendo valore di cov e tempi
          mat_cov <- lapply(id.paz.out, function(ID){
            a <- sub.path[[arr.from[ind.from]]][[ID]]
            if(covariate.type == 'attribute'){
              a <- a[which(!is.na(a[[covariate]])), c( ObjDL.out$csv.IDName, 'pMineR.deltaDate', covariate)]
            }else{
              #LOGICA PER covariate.type="event" !
            }


            return(a)
          })



          mat_new <- as.data.frame(do.call('rbind', mat_cov))

          if(nrow(mat_new)>0){
            mat_new$nodo_IN <-arr.from[ind.from]
            mat_new$nodo_OUT <-arr.node.to[ind.to]
            to_ret<-mat_new
          }else{
            to_ret<-NULL
          }

          return(to_ret)

          # mat_new$nodo_IN <-arr.from[ind.from]
          # mat_new$nodo_OUT <-arr.node.to[ind.to]
          # return(mat_new)

        }
      })

      return(df_path_tot<-as.data.frame(do.call('rbind', df_path)))

    })

    #df_tot è il df che contine tutti i valori delle cov per ciascun paziente con relativo nodo in e nodo out
    df_tot <- do.call('rbind', df_tot)

    if(nrow(df_tot)>0){
      # df_tot <- df_tot[which(df_tot$time <= xlim),]
      if(class(df_tot[[covariate]])=="factor"){
        df_tot[[covariate]]<-as.numeric(levels(df_tot[[covariate]]))[df_tot[[covariate]]]
      }else{
        df_tot[[covariate]] <- as.numeric(df_tot[[covariate]])
      }
      df_tot[[covariate]] <- as.numeric(df_tot[[covariate]])
      colnames(df_tot) <- c('ID', 'time', 'covariate', 'nodo_IN', 'nodo_OUT')

      if( UM == "days") df_tot$time <- df_tot$time / 1440
      if( UM == "hours") df_tot$time <- df_tot$time / 60
      if( UM == "weeks") df_tot$time <- df_tot$time / (1440 * 7)
      if( UM == "months") df_tot$time <- df_tot$time / (43800)
    }else{
      df_tot<-NULL
    }

  }else{
   #CASO C) GRAFICI ANDAMENTALI PER VARIABILI CATEGORICHE:ERRORE
    df_tot<-NULL
  }

  ######################################### PLOT DEI DATI ####################################################################################################
  #distinguiamo 2 casi: 1) GRAFICO ANDAMENTALE
  #                     2) GRAFICO PUNUALE


  #Seconda condizione dell'if è provvisoria e serve per gestire casi con logica non ancora inplementata
  if(length(lst.to)!=0 & !is.null(df_tot)){
  #CASO 1:GRAFICO ANDAMENTALE
    mycolors<-c("#DF060A","#88df45","#041CB3","#F8961E","#55E6D7",
                "#FFFF48","#9B21DD")
    path<-unique(df_tot[,c("nodo_IN","nodo_OUT")])
    # colore<-rainbow(n=nrow(path))
    colore <- mycolors[1:nrow(path)]
    arr.colore <- data.frame(cbind(colore,path["nodo_IN"], path["nodo_OUT"]))
    tipo_punti <- ifelse(plot.points, 'p', 'n')
    tipo_linea <- ifelse(plot.RegressionLine, 1, 0)
    tipo_ic <- ifelse(plot.RegressionLine, 2, 0)


    for (i in 1:nrow(path)){

      dati <- df_tot[which(df_tot$nodo_IN == path[i,"nodo_IN"] & df_tot$nodo_OUT == path[i,"nodo_OUT"]),]
      if(plot.RegressionLine){
        model <- glm(covariate ~ time, data = dati, family = 'gaussian')
      }
      if(!plot.ci.mean){
        y.lim=c(min(df_tot$covariate),max(df_tot$covariate))
      }else{
        d_mean<-ci.mean.fun(alfa,delta,dati)
        y.lim=y.lim=c(min(min(d_mean$lower),min(df_tot$covariate)),max(max(df_tot$covariate),max(d_mean$upper)))
      }
      if(i == 1){
        plot(covariate ~ time,
             data = dati,
             type = tipo_punti,
             pch = points.symbols,
             cex = size.symbols,
             ylim=y.lim,

             xlim = c(0,max(df_tot$time)),
             # xlim = c(0,xlim),
             xlab = paste(UM,'from input node'),
             col = arr.colore[i,'colore'])
        if(plot.RegressionLine){
          if(length(which(is.na(model$coefficients)))==0){
            abline(model,
                   col = arr.colore[i,'colore'],
                   lwd = line.width,
                   lty = tipo_linea)
          }
        }
        if(plot.ci.mean){
          lines(x = d_mean$time,y = d_mean$mean,type = "l",xlab = "time",ylab = "mean", ylim = c(min(d_mean$lower),max(d_mean$upper)),col=arr.colore[i,'colore'])
          col<-col2rgb(arr.colore[i,'colore'], alpha = FALSE)/255
          polygon(x=c(d_mean$time,
                      rev(d_mean$time)),
                  y=c(d_mean$upper, rev(d_mean$lower)),
                  col=rgb(red =col[1,1],green =col[2,1] ,blue =col[3,1] ,alpha = 0.1
                  ),
                  density = 300, angle=90)
        }
      }else{
        points(covariate ~ time,
               data = dati,
               type = tipo_punti,
               pch = points.symbols,
               cex= size.symbols,
               col = arr.colore[i,'colore'] )
        if(plot.RegressionLine){
          if(length(which(is.na(model$coefficients)))==0){
            abline(model,
                   col = arr.colore[i,'colore'],
                   lwd = line.width,
                   lty = tipo_linea)
          }
        }
        if(plot.ci.mean){

          lines(x = d_mean$time,y = d_mean$mean,type = "l",xlab = "time",ylab = "mean", ylim = c(min(d_mean$lower),max(d_mean$upper)),col=arr.colore[i,'colore'])
          col<-col2rgb(arr.colore[i,'colore'], alpha = FALSE)/255
          polygon(x=c(d_mean$time,
                      rev(d_mean$time)),
                  y=c(d_mean$upper, rev(d_mean$lower)),
                  col=rgb(red =col[1,1],green =col[2,1] ,blue =col[3,1] ,alpha = 0.1
                  ),
                  density = 300, angle=90)
        }
      }


    }



    arr.colore$nomi_nodi_in <- unlist(lapply(arr.colore$nodo_IN, function(x){paste(x, CFMstructure$lst.nodi[[x]]$evento, sep = '-')} ))
    arr.colore$nomi_nodi_out <- unlist(lapply(arr.colore$nodo_OUT, function(x){paste(x, CFMstructure$lst.nodi[[x]]$evento, sep = '-')} ))
    arr.colore$Nome_coppie_nodi <- paste('From:', arr.colore$nomi_nodi_in, 'to:', arr.colore$nomi_nodi_out)
    legend(legend.position,
           legend <- arr.colore$Nome_coppie_nodi,
           col=arr.colore[1:nrow(arr.colore),'colore'],
           lty = 1,
           lwd = 3,
           y.intersp =  y.int.legend,
           cex=legend.text.size,
           bty = 'o'
    )

  }else{
    return(NULL)

  }

}










