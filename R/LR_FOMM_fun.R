
#'@import pMineR



#
# arr.ID.train<-sample(x = unique(all.data[[1]]$ID),size = 700)
# arr.ID.test<-unique(all.data[[1]]$ID)[-which(unique(all.data[[1]]$ID) %in% arr.ID.train)]
# eventoDiPartenza<-"M_0000"
# obj.out<-ObjDL$getData()
# eventoGoal<-"Dead"
#
# arr.attributi<-c()
# for (i in c(1:length(colnames(all.data[[1]])))) {
#   name<-colnames(all.data[[1]])[i]
#   if(length(unique(all.data[[1]][,name]))>20){
#     arr.attributi[i]<-colnames(all.data[[1]])[i]
#   }else{
#     arr.attributi[i]<-NA
#   }
# }
# arr.attributi<-na.omit(arr.attributi)
# arr.attributi<-arr.attributi[4:length(arr.attributi)]
#
# k=1
# p.thr=0.05
# n.att=3
# feature.selection<-TRUE
# missing.tec<-"1"
# n.digit.out=4
#
#
#
# out<-LR_FOMM_fun(eventoDiPartenza = eventoDiPartenza,
#                  obj.out = obj.out,
#                  eventoGoal = eventoGoal,
#                  arr.attributi = arr.attributi,
#                  arr.ID.train = arr.ID.train,
#                  arr.ID.test = arr.ID.test,
#                  k=5,n.att = 3,min.time = 0, max.time = Inf
#                  )
# out1<-LR_FOMM_fun(eventoDiPartenza = eventoDiPartenza,
#                  obj.out = ObjDL$getData(),
#                  eventoGoal = eventoGoal,
#                  arr.attributi = arr.attributi,
#                  arr.ID.train = arr.ID.train,
#                  arr.ID.test = arr.ID.test,
#                  k=1,n.att = n.att,p.train = 0.7)
# out2<-LR_FOMM_fun(eventoDiPartenza = eventoDiPartenza,
#                   obj.out = obj.out,
#                   eventoGoal = eventoGoal,
#                   arr.attributi = c("Age_onset","Onset_Delta","Blood_Pressure_Systolic__mmHg"),
#                   arr.ID.train = arr.ID.train,
#                   arr.ID.test = arr.ID.test,
#                   feature.selection = FALSE)


LR_FOMM_fun<-function(eventoDiPartenza,
                      obj.out,
                      eventoGoal  ,
                      arr.attributi ,            #STO IPOTIZZANDO CHE TUTTE LE POSSIBILI CANDIDATE COVARIATE SIANO DI TIPO NUMERICO (è un controllo che devo fare fuori)
                      arr.ID.train,
                      arr.ID.test,
                      missing.tec="1",           #flag che mi dice come trattare missing val (default 1=na.omit, 2=na.omit+omit att>30%missing)
                      feature.selection=TRUE,
                      k=1,                       #numero di fold crossval (se k==1-> hold out)
                      p.train=0.7,               #in caso di k=1 uso questa percentuale di test/train
                      p.thr=0.05,                 #soglia sul p.value
                      n.att=2,                   #numero di attributi da usare come cov
                      n.digit.out=4,
                      passing=c(),
                      NOTpassing=c(),
                      max.time=Inf,
                      min.time=0,
                      UM="days",
                      for.gui=FALSE
                      ){

  ############################### CONTROLLI FORMALI #############################################
  #1) se non faccio feature selection setto tecnica di imputing ==1
  if(!feature.selection){
    missing.tec="1"
  }


  ###############################  CREATE DATA FRAME ##############################################
  obj.QOD<- pMineR::QOD(UM = UM )
  obj.QOD$loadDataset(dataList = obj.out)
  #SELEZIONO ID CLASSE 1: che sperimentano eventoGoal|eventoPartenza
  id.1<-obj.QOD$query(from = eventoDiPartenza,to = eventoGoal,arr.passingThrough = passing,arr.NOTpassingThrough = NOTpassing,time.range = c(min.time,max.time))
  ##SELEZIONO ID CLASSE 0: che NON sperimentano eventoGoal|eventoPartenza
  id.0<-obj.QOD$query(from = eventoDiPartenza,to = "END",arr.NOTpassingThrough = c(eventoGoal,NOTpassing),arr.passingThrough = passing, time.range = c(min.time,max.time))

  # CONTROLLO CHE CI SIANO PAZIENTI IN ENTRAMBE LE CLASSI (y=1 E y=0)
  if(length(id.1)==0){
    stop("non ho id.1")
  }

  if(length(id.0)==0){
    stop("non ho id.0")
  }

  #COSTRUISCO DF CHE CONTINE [ID,ATT_1,...,ATT_N, Y]
  ID<-c(id.1,id.0)

  tmp<-lapply(ID, function(id){
    att.val<-search_value(sub.path=obj.out$pat.process[[id]],eventoDiPartenza,arr.attributi)
    if(id %in% id.1){
      y.val<-1
    }else{
      y.val<-0
    }
    return(cbind(id,att.val,y.val))
  })

  df_tot<-as.data.frame(do.call("rbind",tmp))
  colnames(df_tot)<-c("ID",arr.attributi,"y")

  #SETTO TUTTI GLI ATTRIBUTI A TIPO NUMERICO
  for (i in c(1:length(arr.attributi))) {
    df_tot[,arr.attributi[i]]<-as.numeric(df_tot[,arr.attributi[i]])
  }

  #SETTO A TIPO FACTOR LA COLONNA CHE CONTIENE LA CLASSE (y)
  df_tot$y<-as.factor(df_tot$y)


  #################################### MISSING VALUES ########################################
  # STRATRGIE:  - 1 ---> na.omit di tutte le righe con almeno 1 na
  #             - 2 ---> elimino colonne con na > 30% + elimino righe in cui restano NA
  #             - 3 ---> elimino righe con NA >30% + elimino colonne con na > 30% + media sui restanti na

  switch (missing.tec,
    "1" = {
      df_tot<-na.omit(df_tot)},

    "2" = {
      p.check<-round(nrow(df_tot)*0.3)
      col.to.keep<-c()
      for (i in c(1:ncol(df_tot))) {
        if(length(which(is.na(df_tot[,i])))<=p.check){
          col.to.keep<-c(col.to.keep,colnames(df_tot)[i])
          }
        }
      df_tot<-subset(df_tot, select = col.to.keep )
      arr.attributi<-colnames(df_tot)[-which(colnames(df_tot) %in% c("ID","y"))]
      df_tot<-na.omit(df_tot)},

    #### 3: probabile stupidagine
    "3" = {
      p.check<-round(ncol(df_tot)*0.3)
      row.to.keep<-c()
      for (i in c(1:nrow(df_tot))) {
        if(length(which(is.na(df_tot[i,])))<=p.check){
          row.to.keep<-c(row.to.keep,i)
        }
      }
      df_tot<-df_tot[row.to.keep,]

      p.check<-round(nrow(df_tot)*0.3)
      col.to.keep<-c()
      for (i in c(1:ncol(df_tot))) {
        if(length(which(is.na(df_tot[,i])))<=p.check){
          col.to.keep<-c(col.to.keep,colnames(df_tot)[i])
          if(length(which(is.na(df_tot[,i])))>0){
            df_tot[which(is.na(df_tot[,i])),i]<-mean(df_tot[,i],na.rm = TRUE)
          }
        }
      }
      df_tot<-subset(df_tot, select = col.to.keep)
      arr.attributi<-colnames(df_tot)[-which(colnames(df_tot) %in% c("ID","y"))]}
    )

  ###################################   DIVISIONE TRAIN - TEST #########################################
  df_train <-df_tot[which(df_tot$ID %in% arr.ID.train),]
  df_test  <-df_tot[which(df_tot$ID %in% arr.ID.test),]



  if(feature.selection){
    #nel caso di feature selection devo operare un'ulteriore suddivisione tra test e train, SOLO A PARTIRE DAI DATI DI TRAIN
    lst.fold<-split(df_train$ID,sample(1:k))

    #ciclo sul numero di fold e per ogni iterata: - creo train set e test set
    lst.model<-lapply(1:k, function(ind.fold.test){
      print(paste("k fold:",ind.fold.test))

      if(for.gui){
        incProgress(
          amount = 1 / k)
      }

      if(k==1){
        #caso hold out: calcolo train set e test set per la feature selection a partire da p.train in input
        id.train_set<-sample(x = lst.fold[[1]],size = length(lst.fold[[1]])*p.train)
        train_set<-df_train[which(df_train$ID %in% id.train_set),]
        test_set <-df_train[-which(df_train$ID %in% train_set$ID),]
        # test_set<-lst.fold[[1]][-which(lst.fold[[1]] %in% train_set)]
      }else{
        #caso cross_fold:
        #creo il train e il test set
        id.train_set<-as.character(unlist(x = lst.fold[-ind.fold.test],use.names = FALSE))
        train_set<-df_train[which(df_train$ID %in% id.train_set),]
        test_set <-df_train[-which(df_train$ID %in% train_set$ID),]
      }

      #creo stringa per gli attributi scelti: all'inizio sarà vuota
      chosen.att<-c()
      # print(paste("chosen att:",chosen.att,"fold",ind.fold.test))

      #algoritmo stepwise: ciclo sul numero di attributi che disidero utilizzare come coviariate.
      # ad ogni iterazione calcolo il modello e guardo le performance: scelgo att con pval_train<=p.thr (soglia stabilita dall'utente) e AUC_test max
      # in OUTPUT ho lista lunga n_att (numero di covariate) in cui ogni elemento contiene: - attributo scelto i quell'iterata,
      #                                                                                     - pvalue di tutti gli attributi
      #                                                                                     - AUC di tutti gli attributi


      lst.stepwise<-lapply(1:n.att, function(att){
        print(paste("current n.att:",att))
        # print(paste("current arr.att",arr.att))


        if(is.null(chosen.att)){
          arr.att<-arr.attributi
        }else{
          arr.att<-arr.attributi[-which(arr.attributi %in% chosen.att)]
        }


        lst.perf<-list()
        pval<-list()
        AUC<-c()
        roc_test<-list()

        for (i in c(1:length(arr.att))) {
          df.train<-subset(train_set,select = c("ID",c(chosen.att,arr.att[i]),"y"))
          df.test<-subset(test_set, select = c("ID",c(chosen.att,arr.att[i]),"y"))
          lst.perf[[i]]<-train.FOMM.LR(df.train,
                                       df.test,
                                       plot_it=FALSE,
                                       chosen.digit = 4,
                                       perf.train = TRUE)

          pval[[i]]<-lst.perf[[i]]$pval_train
          AUC[i]<-lst.perf[[i]]$AUC_test
          roc_test[[i]]<-lst.perf[[i]]$roc_test[which(lst.perf[[i]]$roc_test$accuracy==max(lst.perf[[i]]$roc_test$accuracy)),]
          names(AUC)[i]<-paste0(i,"-",arr.att[i])
        }

        pval_tot<-do.call('rbind',pval)
        roc_test_all<-do.call('rbind',roc_test)

        if(ncol(pval_tot)==1){
          ind.pval.ok<-which(pval_tot<=p.thr)
        }else{
          mat_log<-pval_tot<=p.thr
          for(j in c(1:ncol(pval_tot)-1)){
            new_col<-"&"(mat_log[,1],mat_log[,2])
            mat_log<-mat_log[,-c(j,j+1)]
            mat_log<-cbind(mat_log,new_col)
          }
          ind.pval.ok<-which(new_col)
        }

        if(identical(ind.pval.ok,integer(0))){
          #caso in cui non ho trovato righe con pvalue su train <p.thr
          chosen.att<<-c(chosen.att,NULL)
          print("non ci sono p bassi")
          to_ret<-NULL


        }else{
          AUC.tmp<-AUC
          AUC.tmp[-ind.pval.ok]<-0
          ind.chosen<-which(AUC.tmp==max(AUC.tmp))[1]
          chosen.att<<-c(chosen.att,arr.att[ind.chosen])
          print(paste("currente chosen:",chosen.att))
          to_ret<-list("chosen_att"=chosen.att,
                       # "AUC.test"=AUC,
                       "AUC.best.test"=AUC[ind.chosen],
                       "roc.test.best"=roc_test_all[ind.chosen,],
                       "pval.train.best"=pval_tot[ind.chosen,],
                       "pvalue.train"=pval_tot)
        }

        return(to_ret)

      })


      names(lst.stepwise)<-paste0("att_",as.character(seq_along(1:n.att)))

      #restituisce l'ultima iterata della stepwise che sia diversa da NULL
      #es: se nell'ultima iterata non soddisfo le condizioni restituisco penultima


      if(length(which(lengths(lst.stepwise)==0))>0){
        lst.stepwise<-lst.stepwise[-which(lengths(lst.stepwise)==0)]
      }


      if(length(lst.stepwise)==0){
        lst.stepwise.final<-list()
      }else{
        lst.stepwise.final<-lst.stepwise[[length(lst.stepwise)]]
      }

      if(k!=1){
        #lst.stepwise.final->lista che contiene feature scelte + performance su train[all-ind.fold.test] e test:ind.fold.test
        #ora devo calcolare AUC media +std + accuracy media +std sui restanti test e train

        fold.to.compute<-c(1:k)[-which(c(1:k) %in% ind.fold.test)]
        AUC.all.fold<-lapply(fold.to.compute, function(fold.comp.test){
          id.train_set<-as.character(unlist(x = lst.fold[-fold.comp.test],use.names = FALSE))
          train_set<-df_train[which(df_train$ID %in% id.train_set),]
          test_set <-df_train[-which(df_train$ID %in% train_set$ID),]

          df.train<-subset(train_set,select = c("ID",lst.stepwise.final$chosen_att,"y"))
          df.test<-subset(test_set, select = c("ID",lst.stepwise.final$chosen_att,"y"))
          fold.to.check.mod <-train.FOMM.LR(df.train,
                                            df.test,
                                            plot_it=FALSE,
                                            chosen.digit = 4,
                                            perf.train = TRUE)
          count.y.1<-table(test_set$y)[2]
          count.y.0<-table(test_set$y)[1]
          return(list("best_accuracy"=fold.to.check.mod$roc_test[which(fold.to.check.mod$roc_test$accuracy==max(fold.to.check.mod$roc_test$accuracy)),"accuracy"][1],
                      "AUC_ith_test_fold"=fold.to.check.mod$AUC_test,
                      "count.y.1"=count.y.1,
                      "count.y.0"=count.y.0))
        })

        names(AUC.all.fold)<-paste0("test_fold",fold.to.compute)

        df_fold<-do.call('cbind',AUC.all.fold)

      }else{
        df_fold<-NULL
      }

      return(list("model_on_ithFold"=lst.stepwise.final,
                  "other_test_fold"=df_fold))
    })

    names(lst.model)<-paste0("test.fold_",as.character(seq_along(1:k)))

    ########################################################## COSTRUZIONE DEGLI OUTPUT ########################################

    if(k==1){
      best.att<-lst.model$test.fold_1[[1]]$chosen_att
      mat.att<-matrix(ncol = n.att+3)
      mat.att[1,]<-c(lst.model$test.fold_1[[1]]$chosen_att,
                     lst.model$test.fold_1[[1]]$AUC.best.test,
                     lst.model$test.fold_1[[1]]$roc.test.best$threshold,
                     lst.model$test.fold_1[[1]]$roc.test.best$accuracy)
      colnames(mat.att)<-c(paste0("covariate_",seq_along(1:n.att)),"AUC.on.test.fold","threshold","accuracy")
      arr.acc<-NULL
      arr.AUC<-NULL
      mat.att.total<-mat.att
    }else{
      best.att<-NULL
      arr.acc<-list()
      arr.AUC<-list()
      #costruisco matrice che contiene tutti gli attributi selezionati per ogni iterata della cross val
      add.col<-3                                          #numero di colonne contenenti informazioni aggiuntive: AUC.media+std, acc.media+std
      mat.att<-matrix("",nrow = k,ncol = n.att+add.col)
      for (i in c(1:length(lst.model))) {
        #per ogni item di lst.model (ogni fold) calcolo AUC media e std + acc media e std con i chosen di quell'iterata:
        arr.AUC[[i]]<-as.numeric(c(unlist(lst.model[[i]]$other_test_fold[1,]),lst.model[[i]]$model_on_ithFold$AUC.best.test))
        names(arr.AUC)[i]<-paste0("fold",i,": ",paste(lst.model[[i]][[1]]$chosen_att,collapse = "-"))
        arr.acc[[i]]<-as.numeric(c(unlist(lst.model[[i]]$other_test_fold[2,]),lst.model[[i]]$model_on_ithFold$roc.test.best$accuracy))
        names(arr.acc)[i]<-paste0("fold",i,": ",paste(lst.model[[i]][[1]]$chosen_att,collapse = "-"))

        if(length(lst.model[[i]][[1]]$chosen_att)<(ncol(mat.att)-add.col)){
          ind.diff<-(ncol(mat.att)-add.col)-length(lst.model[[i]][[1]]$chosen_att)
          riga<-c(lst.model[[i]][[1]]$chosen_att,
                  rep("",ind.diff),
                  lst.model[[i]][[1]]$AUC.best.test,
                  lst.model[[i]][[1]]$roc.test.best$threshold,
                  lst.model[[i]][[1]]$roc.test.best$accuracy)
        }else{
          riga<-c(lst.model[[i]][[1]]$chosen_att,
                  lst.model[[i]][[1]]$AUC.best.test,
                  lst.model[[i]][[1]]$roc.test.best$threshold,
                  lst.model[[i]][[1]]$roc.test.best$accuracy)
        }
        mat.att[i,]<-riga
      }


      colnames(mat.att)<-c(paste0("covariate_",seq_along(1:n.att)),"AUC.on.test.fold","threshold","accuracy")
      mat.att.total<-cbind(mat.att[,paste0("covariate_",seq_along(1:n.att))],
                           round(unlist(lapply(arr.AUC, mean),use.names = FALSE),digits = n.digit.out),
                           round(unlist(lapply(arr.AUC, sd  ),use.names = FALSE),digits = n.digit.out),
                           round(unlist(lapply(arr.acc, mean),use.names = FALSE),digits = n.digit.out),
                           round(unlist(lapply(arr.acc, sd  ),use.names = FALSE),digits = n.digit.out))
      colnames(mat.att.total)[(n.att+1):ncol(mat.att.total)]<-c("AUC.mean.folds","AUC.sd.folds","acc.mean.folds","acc.sd.folds")

    }

    final.models<-lapply(1:k, function(model){
      final<-train.FOMM.LR(df_train[,c("ID",lst.model[[model]]$model_on_ithFold$chosen_att,"y")],
                                       df_test[,c("ID",lst.model[[model]]$model_on_ithFold$chosen_att,"y")],
                                       plot_it=TRUE,
                                       chosen.digit = 4,
                                       perf.train = TRUE)
      return(final)
    })

    names(final.models)<-paste0("model",seq_along(1:k))



    to_ret<-list("final.model"=final.models,
                 "best.att"=best.att,
                 "mat.perf.featuresel"=mat.att,
                 "lst.arr.AUC.fold"=arr.AUC,
                 "lst.arr.acc.fold"=arr.acc,
                 "mat.perf.total"=mat.att.total)




  }else{
    final.model<-train.FOMM.LR(df_train[,c("ID",arr.attributi,"y")],
                               df_test[,c("ID",arr.attributi,"y")],
                               plot_it=TRUE,
                               chosen.digit = 4,
                               perf.train = TRUE)

    mat.att<-matrix(ncol = n.att+3)
    mat.att[1,]<-c(arr.attributi,
                   final.model$AUC_test,
                   final.model$roc_test[which(final.model$roc_test$accuracy==max(final.model$roc_test$accuracy)),"accuracy"][1],
                   final.model$roc_test[which(final.model$roc_test$accuracy==max(final.model$roc_test$accuracy)),"threshold"][1]
                   )
    colnames(mat.att)<-c(paste0("covariate_",seq_along(1:n.att)),"AUC.on.test.fold","threshold","accuracy")
    arr.acc<-NULL
    arr.AUC<-NULL
    mat.att.total<-mat.att





    to_ret<-list("final.model"=final.model,
                 "best.att"=arr.attributi,
                 "mat.perf.featuresel"=mat.att,
                 "lst.arr.AUC.fold"=arr.AUC,
                 "lst.arr.acc.fold"=arr.acc,
                 "mat.perf.total"=mat.att.total)
  }

  return(to_ret)

}
