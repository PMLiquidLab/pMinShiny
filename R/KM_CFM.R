#'@title KM function x CFM
#'
#'@import pMineR
#'@import survival
#'

# id.start<-0
# id.end<-c(47)
# autocens<-TRUE
# id.cens<-c()
# ObjDL.out<-ObjDL$getData()
# cens.leaf=FALSE
# UM="weeks"

#
#
# events.btw<-c("Covid")
# events.not.btw<-c("ICU")






#Param:
# 1. Struttura CFM (per ricavare la lista nodi)
# 2. id.node.start (id.start)
# 3. array id.node.end (id.end)
# 4. Flag x autocens default TRUE (cens.leaf) considero cens paz foglie
# 5. id.node x cens (id.cens)
# 6. ObjDL.out (per ricavare pat.process)
# 7. UM unità di misura tempo (utile quando applico il filtro sul tempo max di osservazione della coorte)
# 8. events.btw $ events.not.btw filtro sulla traccia dei pazienti considerati nella coorte
# 9. min_time & max_time (utili in caso di filtro dul delta t max di osservazione)
# 10. id_strat array di id della sottocoorte in caso di stratificazione

KM_CFM<-function(ObjCFM,
                 id.start,
                 id.end,
                 cens.leaf=TRUE,
                 id.cens=c(),
                 ObjDL,
                 UM="mins",
                 min_time=0,
                 max_time=Inf,
                 id_strat=c()){

  s<-ObjCFM$getDataStructure()
  out<-ObjDL$getData()

    if(identical(is.na(id.cens),logical(0))){
      #NON HO ID NODI CENS
      autocens=TRUE
    }else{
      autocens=FALSE
      #paz.list --> lista che per ogni nodo cens contiene una matrice con id e rispettivo nodo cens

      paz.list<-lapply(id.cens, function(nodo.cens){
        id.paz<-s$lst.nodi[[as.character(nodo.cens)]]$IPP
        paz<-cbind(id.paz, nodo.cens)
        return(paz)
      })

      names(paz.list)<-id.cens
      paz.mat<-c()
      for (i in c(1:length(paz.list))) {
        paz.mat<-rbind(paz.mat,paz.list[[i]])
      }
    }

    #seleziono coorte: tutti gli id che transitano in nodo start
    coorte<-s$lst.nodi[[as.character(id.start)]]$IPP


    #in caso di id_strat diverso da NULL filtro la corte per gli id specificati
    if(!is.null(id_strat)){
      coorte<-coorte[which(coorte %in% id_strat)]
    }


    #controllo che nodo end sia nel sottoalbero che inizia con nodo start
    check.flag<-c()


    for (i in c(1:length(id.end))) {
      check<-which(s$lst.nodi[[as.character(id.start)]]$IPP %in% s$lst.nodi[[as.character(id.end[i])]]$IPP)
      check.flag[i]<-identical(check, integer(0))
    }


    if(length(which(check.flag==TRUE))>0){
      to_ret<-NULL
    }else{



    # check<-which(s$lst.nodi[[as.character(id.start)]]$IPP %in% s$lst.nodi[[as.character(id.end)]]$IPP)
    # if(identical(check, integer(0))){
    #   to_ret<-NULL
    # }else{


      #ciclo su tutti gli id di coorte e calcolo deltaT come:
      #delta date al nodo evento - delta date al nodo start
      tmp<-lapply(coorte, function(id){
        time.start<-s$lst.nodi[[as.character(id.start)]]$pMineR.deltaDate[which(s$lst.nodi[[as.character(id.start)]]$IPP==id)]

        #possono esserci più nodi di end
        time.end<-array()
        for (i in c(1:length(id.end))) {
          dt<-s$lst.nodi[[as.character(id.end[i])]]$pMineR.deltaDate[which(s$lst.nodi[[as.character(id.end[i])]]$IPP==id)]
          if(identical(dt,numeric(0))){
            time.end[i]<-NA
          } else{
            time.end[i]<-dt
          }
        }

        if(length(time.end)==1){
          if(is.na(time.end)){
            deltaT<-NA
          }else{
            deltaT<-time.end-time.start
          }
          #può accadere che i due nodi di end siano sullo stesso ramo e che lo stesso paziente li sperimenti entrambi---> è giusto???
          #in questo caso prendo come data end quella relativa all'evento più lontano nel tempo????
        }else{
          if(identical(which(!is.na(time.end)),integer(0))){
            deltaT<-NA
          }else{
            #metto min se la regola diventa che per pazienti su stesso ramo prendo come delta quello relativo al primo evento
            #(forse min ha più senso...)
            deltaT<-max(time.end,na.rm = TRUE)-time.start
          }

        }



        # deltaT<-time.end-time.start
        return(list("id"=id,
                    "deltaT"= deltaT
        ))
      })

      matrice.KM <- c()

      for( i in c(1:length(tmp)) ) {
        if(is.na(tmp[[i]]$deltaT)){

          #SONO QUI SE tmp$ID[i] è un CENS-->se deltaT è Na vuol dire che l'id i-esimo che sto considerando non è transitato nel nodo di end quindi cens

          #bisogna controllare due flag: autocens e cens.leaf:

          #caso 1: F & F --> ho id nodi cens e voglio che solo quelli siano i cens
          #caso 2: F & T --> ho id nodi cens e voglio anche usare i paz ai leaf
          #caso 3: T & T --> non ho id nodi cens e uso come cens i paz ai leaf
          #caso 4: T & F --> non ho nodi cens e non uso come cens i paz ai leaf --> NON POSSO TORNO A CASO 1

          if(autocens== FALSE){
            if(tmp[[i]]$id %in% paz.mat[,1]){
              #sia per caso 1 che per caso 2 tratto i paz che passano nei nodi cens nello stesso modo:
              #devo calcolare il deltat come time.start - time in cui arrivano al nodo cens

              t.start<-s$lst.nodi[[as.character(id.start)]]$pMineR.deltaDate[which(s$lst.nodi[[as.character(id.start)]]$IPP==tmp[[i]]$id)]
              t.end<-s$lst.nodi[[paz.mat[which(paz.mat[,1]==tmp[[i]]$id),2]]]$pMineR.deltaDate[which(s$lst.nodi[[paz.mat[which(paz.mat[,1]==tmp[[i]]$id),2]]]$IPP==tmp[[i]]$id)]
              delta<-t.end-t.start
              matrice.KM <- rbind(matrice.KM, c(tmp[[i]]$id, delta,"0"))
            }else{
              if(cens.leaf== FALSE){
                #CASO 1:  uso come cens SOLO i pazi che transitano nei nodi inseriti come nodi cens
                matrice.KM <- matrice.KM
              }else{
                #CASO 2:  uso come cens sia i paz che transitano nei nodi cens che i paz che vanno nelle leaf
                #         per questi calcolo delta t come t.ultimo evento della traccia - t.start
                delta<-out$pat.process[[tmp[[i]]$id]]$pMineR.deltaDate[nrow(out$pat.process[[tmp[[i]]$id]])]
                matrice.KM <- rbind(matrice.KM, c(tmp[[i]]$id, delta,"0"))
              }
            }

          }else{
            # restano fuori caso 3 e 4 --> in entrambi questi casi rientro nella tecnica: non ho nodi cens uso come
            #paz cens quelli delle leaf

            delta<-out$pat.process[[tmp[[i]]$id]]$pMineR.deltaDate[nrow(out$pat.process[[tmp[[i]]$id]])]
            matrice.KM <- rbind(matrice.KM, c(tmp[[i]]$id, delta,"0"))
          }
        }else{
          matrice.KM <- rbind( matrice.KM, c(tmp[[i]]$id, tmp[[i]]$deltaT, "1"))
        }
      }

      colnames(matrice.KM)<-c("ID","time","outcome")
      matrice.KM <- as.data.frame(matrice.KM)


      # if(!is.null(events.not.btw)){
      #   to.delete<-c()
      #   #check su eventi between and not between.
      #   #scorro sul numero delle righe
      #   for (i in c(1:nrow(matrice.KM))) {
      #     #analizzo il pathprocess dell'id di mat.km alla riga i-esima:
      #     #cerco se nella traccia del paziente della riga i c'è un evento presente
      #     # nell'array event not between
      #     id.not<-which(out$pat.process[[as.character(matrice.KM$ID[i])]]$EVENT %in% events.not.btw)
      #
      #     if(!identical(id.not,integer(0)) & length(id.not)>0){
      #       #se c'è allora vorrà dire che quel paziente
      #       #non dovrà far parte della coorte e che quindi posso eliminare la riga che sto analizzando
      #       to.delete<-c(to.delete,i)
      #     }
      #   }
      #   matrice.KM<-matrice.KM[-to.delete,]
      # }
      #
      #
      # #faccio lo stesso check per gli eventi in event between
      # if(!is.null(events.btw)){
      #   to.keep<-c()
      #
      #   for (i in c(1:nrow(matrice.KM))) {
      #     id.keep<-which(out$pat.process[[as.character(matrice.KM$ID[i])]]$EVENT %in% events.btw)
      #     if(!identical(id.keep,integer(0)) & length(id.keep)>0){
      #       to.keep<-c(to.keep,i)
      #     }
      #   }
      #   matrice.KM<-matrice.KM[to.keep,]
      # }




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



      if( UM == "days") matrice.KM$time <- matrice.KM$time / 1440
      if( UM == "hours") matrice.KM$time <- matrice.KM$time / 60
      if( UM == "weeks") matrice.KM$time <- matrice.KM$time / (1440 * 7)
      if( UM == "months") matrice.KM$time <- matrice.KM$time / (43800)


      #filtro sul tempo massimo di osservazione
      deltamax<-max_time-min_time
      matrice.KM<-matrice.KM[matrice.KM$time<=deltamax,]
      if(nrow(matrice.KM)==0){
        to_ret<-NULL
      }else{
        km.model<-survfit(Surv(time, outcome)~1,   data=matrice.KM)
        to_ret<-list("KM"=km.model,
                     "data.surv"= matrice.KM)
      }






      # to_ret <- survfit(Surv(time, outcome)~1,   data=matrice.KM)
      # to_ret<-plot(KM0,
      #              main=paste0(s$lst.nodi[[as.character(id.start)]]$event, "->", unlist(lapply(id.end, function(nodo){ s$lst.nodi[[as.character(nodo)]]$event}))),
      #              xlab=UM,
      #              ylab="p",
      #              mark.time=TRUE)


  }






  return(to_ret)
}
#
# KM1<-KM_CFM(ObjCFM,
#             id.start,
#             id.end,
#             cens.leaf,
#             id.cens,
#             ObjDL,
#             UM)
#
# plot(c(KM0,KM1),
#      main=paste0(s$lst.nodi[[as.character(id.start)]]$event, "->", unlist(lapply(id.end, function(nodo){ s$lst.nodi[[as.character(nodo)]]$event}))),
#              xlab=UM,
#              ylab="p",
#              mark.time=TRUE)

