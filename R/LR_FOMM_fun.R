
#'@import pMineR



# LR_FOMM_fun<-function(eventStart,obj.out,eventGoal,arr.attributes, arr.ID.train,arr.ID.test,feature.selection=TRUE,k=1,p.train=0.7,
#                       p.thr=0.05,n.att=2,n.digit.out=4,passing=c(),NOTpassing=c(),max.time=Inf,min.time=0,UM="days"){
#
#   check.flag<-TRUE
#
#   obj.QOD<- pMineR::QOD(UM = UM )
#   obj.QOD$loadDataset(dataList = obj.out)
#   id.1<-obj.QOD$query(from = eventStart,to = eventGoal,arr.passingThrough = passing,arr.NOTpassingThrough = NOTpassing,time.range = c(min.time,max.time))
#   id.0<-obj.QOD$query(from = eventStart,to = "END",arr.NOTpassingThrough = c(eventGoal,NOTpassing),arr.passingThrough = passing, time.range = c(min.time,max.time))
#
#   if(is.na(id.1) || is.na(id.0)){
#     check.flag<-FALSE
#     df_tot<-NULL
#     to_ret<-list("res"=NULL,"run.check"=check.flag)
#   }else{
#     ID<-c(id.1,id.0)
#     tmp<-lapply(ID, function(id){
#       att.val<-search_value(sub.path=obj.out$pat.process[[id]],eventStart,arr.attributes)
#       if(id %in% id.1){
#         y.val<-1
#       }else{
#         y.val<-0
#       }
#       return(cbind(id,att.val,y.val))
#     })
#
#     df_tot<-as.data.frame(do.call("rbind",tmp))
#     colnames(df_tot)<-c("ID",arr.attributes,"y")
#
#     for (i in c(1:length(arr.attributes))) {
#       df_tot[,arr.attributes[i]]<-as.numeric(df_tot[,arr.attributes[i]])
#     }
#
#     df_tot$y<-as.factor(df_tot$y)
#     df_tot<-na.omit(df_tot)
#   }
#
#
#   if(is.null(df_tot)){
#     check.flag<-FALSE
#     data_tot<-NULL
#     to_ret<-list("res"=NULL,"run.check"=check.flag)
#   }else{
#     df_train <-df_tot[which(df_tot$ID %in% arr.ID.train),]
#     df_test  <-df_tot[which(df_tot$ID %in% arr.ID.test),]
#
#     if(feature.selection){
#       lst.fold<-split(df_train$ID,sample(1:k))
#
#       lst.model<-lapply(1:k, function(ind.fold.test){
#         #DEBUG
#         print(paste("k fold:",ind.fold.test))
#
#         if(k==1){
#           #hold out
#           id.train_set<-sample(x = lst.fold[[1]],size = length(lst.fold[[1]])*p.train)
#           train_set<-df_train[which(df_train$ID %in% id.train_set),]
#           test_set <-df_train[-which(df_train$ID %in% train_set$ID),]
#         }else{
#           #cross fold
#           id.train_set<-as.character(unlist(x = lst.fold[-ind.fold.test],use.names = FALSE))
#           train_set<-df_train[which(df_train$ID %in% id.train_set),]
#           test_set <-df_train[-which(df_train$ID %in% train_set$ID),]
#         }
#
#         chosen.att<-c()
#
#
#         #algoritmo stepwise: ciclo sul numero di attributi che disidero utilizzare come coviariate.
#         # ad ogni iterazione calcolo il modello e guardo le performance: scelgo att con pval_train<=p.thr (soglia stabilita dall'utente) e AUC_test max
#         # in OUTPUT ho lista lunga n_att (numero di covariate) in cui ogni elemento contiene: - attributo scelto i quell'iterata,
#         #                                                                                     - pvalue di tutti gli attributi
#         #                                                                                     - AUC di tutti gli attributi
#
#
#         lst.stepwise<-lapply(1:n.att, function(att){
#           print(paste("current n.att:",att))
#
#           if(is.null(chosen.att)){
#             arr.att<-arr.attributes
#           }else{
#             arr.att<-arr.attributes[-which(arr.attributes %in% chosen.att)]
#           }
#
#
#           lst.perf<-list()
#           pval<-list()
#           AUC<-c()
#           roc_test<-list()
#
#           for (i in c(1:length(arr.att))) {
#             df.train<-subset(train_set,select = c("ID",c(chosen.att,arr.att[i]),"y"))
#             df.test<-subset(test_set, select = c("ID",c(chosen.att,arr.att[i]),"y"))
#             lst.perf[[i]]<-train.FOMM.LR(df.train,
#                                          df.test,
#                                          plot_it=FALSE,
#                                          chosen.digit = 4,
#                                          perf.train = TRUE)
#
#             pval[[i]]<-lst.perf[[i]]$pval_train
#             AUC[i]<-lst.perf[[i]]$AUC_test
#             roc_test[[i]]<-lst.perf[[i]]$roc_test[which(lst.perf[[i]]$roc_test$accuracy==max(lst.perf[[i]]$roc_test$accuracy)),]
#             names(AUC)[i]<-paste0(i,"-",arr.att[i])
#           }
#
#           pval_tot<-do.call('rbind',pval)
#           roc_test_all<-do.call('rbind',roc_test)
#
#           if(ncol(pval_tot)==1){
#             ind.pval.ok<-which(pval_tot<=p.thr)
#           }else{
#             mat_log<-pval_tot<=p.thr
#             for(j in c(1:ncol(pval_tot)-1)){
#               new_col<-"&"(mat_log[,1],mat_log[,2])
#               mat_log<-mat_log[,-c(j,j+1)]
#               mat_log<-cbind(mat_log,new_col)
#             }
#             ind.pval.ok<-which(new_col)
#           }
#
#           if(identical(ind.pval.ok,integer(0))){
#             #caso in cui non ho trovato righe con pvalue su train <p.thr
#             chosen.att<<-c(chosen.att,NULL)
#             print("non ci sono p bassi")
#             to_ret<-NULL
#
#
#           }else{
#             AUC.tmp<-AUC
#             AUC.tmp[-ind.pval.ok]<-0
#             ind.chosen<-which(AUC.tmp==max(AUC.tmp))[1]
#             chosen.att<<-c(chosen.att,arr.att[ind.chosen])
#             print(paste("currente chosen:",chosen.att))
#             to_ret<-list("model_perf"=lst.perf[[ind.chosen]],
#                          "chosen_att"=chosen.att)
#           }
#
#           return(to_ret)
#
#         })
#
#
#         names(lst.stepwise)<-paste0("att_",as.character(seq_along(1:n.att)))
#
#         #restituisce l'ultima iterata della stepwise che sia diversa da NULL
#         #es: se nell'ultima iterata non soddisfo le condizioni restituisco penultima
#
#
#         if(length(which(lengths(lst.stepwise)==0))>0){
#           lst.stepwise<-lst.stepwise[-which(lengths(lst.stepwise)==0)]
#         }
#
# #############DEBUG DA QUI: cosa fare quando lst.stepwise è tutta NULL
#         if(length(lst.stepwise)==0){
#           lst.stepwise.final<-list()
#           to_ret<-NULL
#         }else{
#           lst.stepwise.final<-lst.stepwise[[length(lst.stepwise)]]
#           if(k!=1){
#             #lst.stepwise.final->lista che contiene feature scelte + performance su train[all-ind.fold.test] e test:ind.fold.test
#             #ora devo calcolare AUC media +std + accuracy media +std sui restanti test e train
#
#             fold.to.compute<-c(1:k)[-which(c(1:k) %in% ind.fold.test)]
#             AUC.all.fold<-lapply(fold.to.compute, function(fold.comp.test){
#               id.train_set<-as.character(unlist(x = lst.fold[-fold.comp.test],use.names = FALSE))
#               train_set<-df_train[which(df_train$ID %in% id.train_set),]
#               test_set <-df_train[-which(df_train$ID %in% train_set$ID),]
#
#               df.train<-subset(train_set,select = c("ID",lst.stepwise.final$chosen_att,"y"))
#               df.test<-subset(test_set, select = c("ID",lst.stepwise.final$chosen_att,"y"))
#               fold.to.check.mod <-train.FOMM.LR(df.train,
#                                                 df.test,
#                                                 plot_it=FALSE,
#                                                 chosen.digit = 4,
#                                                 perf.train = TRUE)
#
#               count.y.1<-table(test_set$y)[2]
#               count.y.0<-table(test_set$y)[1]
#               cont<-c(count.y.0,count.y.1)
#               ret_list<-list("model_perf" =fold.to.check.mod,
#                              "class count"=cont)
#
#               return(ret_list)
#             })
#
#             names(AUC.all.fold)<-paste0("test_fold",fold.to.compute)
#             to_ret<-list()
#             comp_fold_name<-paste0("test_fold",ind.fold.test)
#             to_ret[[comp_fold_name]]<-lst.stepwise.final
#             for (i in c(1:length(names(AUC.all.fold)))) {
#               to_ret[[names(AUC.all.fold)[i]]]<-AUC.all.fold[[names(AUC.all.fold)[i]]]
#             }
#
#
#             # df_fold<-do.call('cbind',AUC.all.fold)
#
#           }else{
#             to_ret<-lst.stepwise.final
#             # df_fold<-NULL
#           }
#         }
#
#
#
#         # if(k!=1){
#         #   #lst.stepwise.final->lista che contiene feature scelte + performance su train[all-ind.fold.test] e test:ind.fold.test
#         #   #ora devo calcolare AUC media +std + accuracy media +std sui restanti test e train
#         #
#         #   fold.to.compute<-c(1:k)[-which(c(1:k) %in% ind.fold.test)]
#         #   AUC.all.fold<-lapply(fold.to.compute, function(fold.comp.test){
#         #     id.train_set<-as.character(unlist(x = lst.fold[-fold.comp.test],use.names = FALSE))
#         #     train_set<-df_train[which(df_train$ID %in% id.train_set),]
#         #     test_set <-df_train[-which(df_train$ID %in% train_set$ID),]
#         #
#         #     df.train<-subset(train_set,select = c("ID",lst.stepwise.final$chosen_att,"y"))
#         #     df.test<-subset(test_set, select = c("ID",lst.stepwise.final$chosen_att,"y"))
#         #     fold.to.check.mod <-train.FOMM.LR(df.train,
#         #                                       df.test,
#         #                                       plot_it=FALSE,
#         #                                       chosen.digit = 4,
#         #                                       perf.train = TRUE)
#         #
#         #     count.y.1<-table(test_set$y)[2]
#         #     count.y.0<-table(test_set$y)[1]
#         #     cont<-c(count.y.0,count.y.1)
#         #     ret_list<-list("model_perf" =fold.to.check.mod,
#         #                    "class count"=cont)
#         #
#         #     return(ret_list)
#         #   })
#         #
#         #   names(AUC.all.fold)<-paste0("test_fold",fold.to.compute)
#         #   to_ret<-list()
#         #   comp_fold_name<-paste0("test_fold",ind.fold.test)
#         #   to_ret[[comp_fold_name]]<-lst.stepwise.final
#         #   for (i in c(1:length(names(AUC.all.fold)))) {
#         #     to_ret[[names(AUC.all.fold)[i]]]<-AUC.all.fold[[names(AUC.all.fold)[i]]]
#         #   }
#         #
#         #
#         #   # df_fold<-do.call('cbind',AUC.all.fold)
#         #
#         # }else{
#         #   to_ret<-lst.stepwise.final
#         #   # df_fold<-NULL
#         # }
#
#         return(to_ret)
#         # return(list("model_on_ithFold"=lst.stepwise.final,
#         #             "other_test_fold"=AUC.all.fold))
#       })
#
#       names(lst.model)<-paste0("test.fold_",as.character(seq_along(1:k)))
#
#       if(length(which(lengths(lst.model)>0))>0){
#         lst.model<-lst.model[which(lengths(lst.model)>0)]
#         if(k==1){
#           best.att<-lst.model$test.fold_1$chosen_att
#           best_acc_ind<-which(lst.model$test.fold_1$model_perf$roc_test$accuracy==max(lst.model$test.fold_1$model_perf$roc_test$accuracy))
#           # best.att<-lst.model$test.fold_1[[1]]$chosen_att
#           mat.att<-matrix(ncol = length(best.att)+3)
#           mat.att[1,]<-c(best.att,
#                          lst.model$test.fold_1$model_perf$AUC_test,
#                          lst.model$test.fold_1$model_perf$roc_test$threshold[best_acc_ind][1],
#                          lst.model$test.fold_1$model_perf$roc_test$accuracy[best_acc_ind][1])
#           colnames(mat.att)<-c(paste0("covariate_",seq_along(1:length(best.att))),"AUC.on.test.fold","threshold","accuracy")
#           arr.acc<-NULL
#           arr.AUC<-NULL
#           mat.att.total<-mat.att
#           final.models<-train.FOMM.LR(df_train[,c("ID",best.att,"y")],
#                                       df_test[,c("ID",best.att,"y")],
#                                       plot_it=TRUE,
#                                       chosen.digit = 4,
#                                       perf.train = TRUE)
#         }else{
#           best.att<-NULL
#           add.col<-3
#           mat.att<-matrix("",nrow = length(lst.model),ncol = n.att+add.col)
#           mat.att.total<-matrix("",nrow = length(lst.model),ncol = n.att+4)
#           for (i in c(1:length(lst.model))) {
#             chosen.att<-lst.model[[i]][[1]][[2]]
#             #per ogni item di lst.model (ogni fold) calcolo AUC media e std + acc media e std con i chosen di quell'iterata:
#             arr.AUC<-lapply(lst.model[[i]],function(inner.fold){
#               return(inner.fold$model_perf$AUC_test)
#             })
#
#             arr.acc<-lapply(lst.model[[i]],function(inner.fold){
#               max.acc<-max(inner.fold$model_perf$roc_test$accuracy)
#               return(c(max.acc,inner.fold$model_perf$roc_test$threshold[which(inner.fold$model_perf$roc_test$accuracy==max.acc)]))
#             })
#
#
#             if(length(chosen.att)<(ncol(mat.att)-add.col)){
#               ind.diff<-(ncol(mat.att)-add.col)-length(chosen.att)
#               riga<-c(chosen.att,
#                       rep("",ind.diff),
#                       arr.AUC[[1]],
#                       arr.acc[[1]][2],
#                       arr.acc[[1]][1]
#               )
#               riga.total<-c(chosen.att,
#                             rep("",ind.diff),
#                             round(mean(unlist(arr.AUC)),digits = n.digit.out),
#                             round(sd(unlist(arr.AUC)),digits = n.digit.out),
#                             round(mean(unlist(lapply(arr.acc, function(i){return(i[[1]])}))),digits = n.digit.out),
#                             round(sd( unlist(lapply(arr.acc, function(i){return(i[[1]])}))),digits = n.digit.out))
#             }else{
#               riga<-c(chosen.att,
#                       arr.AUC[[1]],
#                       arr.acc[[1]][2],
#                       arr.acc[[1]][1])
#               riga.total<-c(chosen.att,
#                             round(mean(unlist(arr.AUC)),digits = n.digit.out),
#                             round(sd(unlist(arr.AUC)),digits = n.digit.out),
#                             round(mean(unlist(lapply(arr.acc, function(i){return(i[[1]])}))),digits = n.digit.out),
#                             round(sd( unlist(lapply(arr.acc, function(i){return(i[[1]])}))) ,digits = n.digit.out))
#             }
#             mat.att[i,]<-riga
#             mat.att.total[i,]<-riga.total
#
#
#
#           }
#
#
#           colnames(mat.att)<-c(paste0("covariate_",seq_along(1:n.att)),"AUC.on.test.fold","threshold","accuracy")
#           colnames(mat.att.total)<-c(paste0("covariate_",seq_along(1:n.att)),"AUC.mean.folds","AUC.sd.folds","acc.mean.folds","acc.sd.folds")
#           final.models<-lapply(1:length(lst.model), function(i){
#             final<-train.FOMM.LR(df_train[,c("ID",lst.model[[i]][[1]][[2]],"y")],
#                                  df_test[,c("ID",lst.model[[i]][[1]][[2]],"y")],
#                                  plot_it=TRUE,
#                                  chosen.digit = 4,
#                                  perf.train = TRUE)
#             # return(final)
#             return(list("final.model"=final,
#                         "count_train"=nrow(df_train),
#                         "count_test"=nrow(df_test)))
#           })
#           names(final.models)<-paste0("model",seq_along(1:length(lst.model)))
#
#         }
#       }else{
#         check.flag<-FALSE
#         return(NULL)
#       }
#
#
#       ########################################################## COSTRUZIONE DEGLI OUTPUT ########################################
#
#
#       lst_to_ret<-list("final.model"=final.models,
#                    "lst.models.fold"=lst.model,
#                    # "best.att"=best.att,
#                    # "mat.perf.featuresel"=mat.att,
#                    # "lst.arr.AUC.fold"=arr.AUC,
#                    # "lst.arr.acc.fold"=arr.acc,
#                    "mat.perf.total"=mat.att.total)
#       to_ret<-list("res"=lst_to_ret,"run.check"=check.flag)
#
#
#
#     }else{
#       final.model<-train.FOMM.LR(df_train[,c("ID",arr.attributes,"y")],
#                                  df_test[,c("ID",arr.attributes,"y")],
#                                  plot_it=TRUE,
#                                  chosen.digit = 4,
#                                  perf.train = TRUE)
#
#       mat.att<-matrix(ncol = length(arr.attributes)+3)
#       mat.att[1,]<-c(arr.attributes,
#                      final.model$AUC_test,
#                      final.model$roc_test[which(final.model$roc_test$accuracy==max(final.model$roc_test$accuracy)),"accuracy"][1],
#                      final.model$roc_test[which(final.model$roc_test$accuracy==max(final.model$roc_test$accuracy)),"threshold"][1]
#       )
#       colnames(mat.att)<-c(paste0("covariate_",seq_along(1:length(arr.attributes))),"AUC.on.test.fold","threshold","accuracy")
#       arr.acc<-NULL
#       arr.AUC<-NULL
#       mat.att.total<-mat.att
#
#       lst_to_ret<-list("final.model"=final.model,
#                    "lst.models.fold"=NULL,
#                    # "best.att"=best.att,
#                    # "mat.perf.featuresel"=mat.att,
#                    # "lst.arr.AUC.fold"=arr.AUC,
#                    # "lst.arr.acc.fold"=arr.acc,
#                    "mat.perf.total"=mat.att.total)
#       to_ret<-list("res"=lst_to_ret,"run.check"=check.flag)
#
#
#
#
#
#       # to_ret<-list("final.model"=final.model,
#       #              "best.att"=arr.attributi,
#       #              "mat.perf.featuresel"=mat.att,
#       #              "lst.arr.AUC.fold"=arr.AUC,
#       #              "lst.arr.acc.fold"=arr.acc,
#       #              "mat.perf.total"=mat.att.total)
#     }
#   }
#
#   return(to_ret)
#
# }


LR_FOMM_fun<-function(eventStart, eventGoal, arr.attributes, obj.out, arr.ID.train, arr.ID.test=c(), feature.selection=TRUE, k=1, p.train=0.7,
                           p.thr=0.05, n.att=2, n.digit.out=4, passing=c(), NOTpassing=c(), max.time=Inf, min.time=0, UM="days", pred.disc=FALSE,
                           arr.cand=c(), oversamp.tec="bootstrap", perf.ind="acc",acc.ind="accuracy",omit.missing=F,ev.param="acc",thr.acc=0.5){

  check.flag<-TRUE

  #creo oggetto QOD che mi aiuterà a ricostuire le classi di appartenenza dei pazienti
  obj.QOD<- pMineR::QOD(UM = UM )
  obj.QOD$loadDataset(dataList = obj.out)

  if(pred.disc){
    #caso predictive pd: id.1=paz evGoal, id0=tutti gli altri paz che transitano in evStart
    id.1<-obj.QOD$query(from = eventStart,to = eventGoal,time.range = c(min.time,max.time),step.range = c(1,1))
    arr.cand<-arr.cand[-which(arr.cand==eventGoal)]

    id.0<-unlist(lapply(arr.cand, function(ev.to){
      if(param.verbose) print(paste("query ev.to:",ev.to))
      return(obj.QOD$query(from = eventStart,to = ev.to, time.range = c(min.time,max.time),step.range = c(1,1)))}))
  }else{
    #caso predictive model
    id.1<-obj.QOD$query(from = eventStart,to = eventGoal,arr.passingThrough = passing,arr.NOTpassingThrough = NOTpassing,time.range = c(min.time,max.time))
    id.0<-obj.QOD$query(from = eventStart,to = "END",arr.NOTpassingThrough = c(eventGoal,NOTpassing),arr.passingThrough = passing, time.range = c(min.time,max.time))
  }

  #check output query fun:
  #controllo che gli id1 e id0 non siano nulli e che id1 siano almeno un TOT (quanto deve valere questo tot? probabilmente almeno quanto il K dello smote)

  if(length(which(is.na(id.1)))>1 || length(which(is.na(id.0)))>1 || length(id.1)<3){
    check.flag<-FALSE
    df_tot<-NULL

  }else{

    #query ha restituito degli id
    ID<-c(id.1,id.0)

    #creo il data set utile per il calcolo del modello:
    #dunque un data set che associa ad ogni ID il valore delle covariate (registrate all'istante temporale in cui avviene nodeStart)
    tmp<-lapply(ID, function(id){
      att.val<-search_value(sub.path=obj.out$pat.process[[id]],eventStart,arr.attributes)
      if(id %in% id.1){
        y.val<-1
      }else{
        y.val<-0
      }
      return(cbind(id,att.val,y.val))
    })

    df_tot<-as.data.frame(do.call("rbind",tmp))
    colnames(df_tot)<-c("ID",arr.attributes,"y")


    #solo covariate numeriche
    for (i in c(1:length(arr.attributes))) {
      df_tot[,arr.attributes[i]]<-as.numeric(df_tot[,arr.attributes[i]])
    }

    df_tot$y<-as.factor(df_tot$y)

    #richiamo fun per gestione missing values
    if(omit.missing){
      df_tot<-na.omit(df_tot)
    }else{
      df_tot<-remove.miss.col(df_tot)$df.clean.col
    }

    arr.attributes<-colnames(df_tot)[-which(colnames(df_tot) %in% c("ID","y"))]

    if(n.att==Inf | n.att>length(arr.attributes)) n.att<-length(arr.attributes)
  }


  #check righe di df_toto (post na.omit): se OK allora passo alla suddivisione in train e test
  if(is.null(df_tot) || nrow(df_tot[which(df_tot$ID %in% id.1),])<=3 || is.na(arr.attributes)){
    check.flag<-FALSE
    data_tot<-NULL
    to_ret<-list("res"=NULL,"run.check"=check.flag)
  }else{
    #TRAIN
    df_train <-df_tot[which(df_tot$ID %in% arr.ID.train),]

    #check sullo sblinaciamento delle classi:
    #prima controllo che il numero di pazienti sia superiore almeno a 3 (PARAMATRIZZARE!!!)
    #poi controllo che il numero di pazienti y=0 sia superiore al 20% dei paz in classe Y=1:
    #se questo non succede, applico tecnica di oversampling
    if(table(df_train$y)[2]>3 & table(df_train$y)[1]>3){
      if(table(df_train$y)[2]<round(nrow(df_train)*0.2)){
        #calcolo di quanto deve aumentare la classe di minoranza affichè numero di 1 sia >20% tot
        switch (oversamp.tec,
                "bootstrap" = {
                  # unders_size<-round(sum(table(df_train$y))*0.2)-table(df_train$y)[2]
                  unders_size<-round(0.25*table(df_train$y)[1]-table(df_train$y)[2])
                  df1<-df_train[sample(x =  which(df_train$y==1), size = unders_size,replace = T ),]
                  # df1<-df_train[which(df_train$y ==1),]
                  df_train<-rbind(df_train,df1)
                  df_train<-df_train[,-1]
                },
                "smote"={
                  d_size<-round(0.25*(table(df_train$y)[1]/table(df_train$y)[2]))
                  smote <- SMOTE(df_train[,-c(1,ncol(df_train))], df_train$y,dup_size = d_size,K = 3)
                  df_train<-smote$data
                  colnames(df_train)[length(colnames(df_train))]<-"y"
                  df_train$y<-as.factor(df_train$y)
                },
                "none"={
                  df_train<-df_train[,-1]
                }
        )
      }else{
        df_train<-df_train[,-1]
      }

      #TRAIN:
      #potrebbe essere che non venga esplicitato un set di test
      if(is.null(arr.ID.test)){
        df_test<-data.frame()
      }else{

        df_test<-df_tot[which(df_tot$ID %in% arr.ID.test),]
        df_test<-df_test[,-1]
        # if(table(df_test$y)[2]<nrow(df_test)*0.1){
        #   d_size<-round(0.25*(table(df_test$y)[1]/table(df_test$y)[2]))
        #   smote <- SMOTE(df_test[,-c(1,ncol(df_test))], df_test$y,dup_size = d_size,K = 3)
        #   df_test<-smote$data
        #   colnames(df_test)[length(colnames(df_test))]<-"y"
        #   df_test$y<-as.factor(df_test$y)
        # }else{
        #   df_test<-df_test[,-1]
        # }
      }


      if(feature.selection){

        lst.model<-lapply(1:k, function(ind.fold.test){
          if(param.verbose){
            print(paste("k fold:",ind.fold.test))
          }

          #suddivido il TRAINNG in TRAIN e VALIDATION
          p.strat<-round(table(df_train$y)[2]/sum(table(df_train$y)),digits = 2)
          continue<-TRUE
          while (continue) {
            ind.train_set<-sample(x=rownames(df_train),size = nrow(df_train)*p.train)
            train_set<-df_train[which(rownames(df_train) %in% ind.train_set),]
            test_set <-df_train[-which(rownames(df_train) %in% ind.train_set),]
            # if(param.verbose){print(table(train_set$y))}

            perc.train<-table(train_set$y)[2]/sum(table(train_set$y))
            perc.test<-table(test_set$y)[2]/sum(table(test_set$y))

            # fatto check sulla stratificazione (((perc.test>=(p.strat-0.1)) & (perc.test<=(p.strat+0.05))))
            if( ((perc.train>=(p.strat-0.1)) & (perc.train<=(p.strat+0.1))) & (perc.test>0 & perc.test<1) ) continue<-FALSE
          }

          if(param.verbose){
            print(paste("------>",table(train_set$y),"<------------"))
            print(ind.train_set)
            print("*******START STEPWISE**************")
          }


          #applico stepwise per la scelta delle covariate:
          #ciclo un numero di volte pari ad n.att sulle covariate che si vogliono inserire nel modello.
          #alla prima iterata calcolo un modello con una sola covariata e dopo averle provate tutte, scelgo la covariata con p value più basso
          #alla seconda itarata calcolo un nuovo modello composto da covariata scelta all'iterata precedente e ciascuna delle altre rimaste.
          #scelgo come seconda covariata quella che garantisca che il pvalue della prima cov non scenda sotto il valore soglia (es.0.05) e con il pval più basso
          #... vado vanti un numero di volte pari ad n.att
          chosen.att<-c()
          lst.stepwise<-lapply(1:n.att, function(att){
            if(param.verbose){
              print(paste("n att:",att, "for fold test:", ind.fold.test))
            }


            if(is.null(chosen.att) || is.na(chosen.att)){
              arr.att<-arr.attributes
            }else{
              arr.att<-arr.attributes[-which(arr.attributes %in% chosen.att)]
            }


            lst.perf<-list()
            pval<-list()
            AUC<-c()
            roc_test<-list()

            for (i in c(1:length(arr.att))) {
              df.train<-subset(train_set,select = c(c(chosen.att,arr.att[i]),"y"))
              df.test<-subset(test_set, select = c(c(chosen.att,arr.att[i]),"y"))
              lst.perf[[i]]<-train.FOMM.LR.new(df_train = df.train,df_test = df.test, chosen.digit = 4,perf.train = TRUE,ev.param = ev.param,thr.acc)

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

              for(j in c(1:(ncol(pval_tot)-1))){
                new_col<-"&"(mat_log[,1],mat_log[,2])
                mat_log<-mat_log[,-c(j,j+1)]
                mat_log<-cbind(mat_log,new_col)
              }
              ind.pval.ok<-which(new_col)
            }

            if(identical(ind.pval.ok,integer(0))){
              #caso in cui non ho trovato righe con pvalue su train <p.thr
              chosen.att<<-c(chosen.att,NULL)
              if(param.verbose){
                print("non ci sono p bassi")
              }
              to_ret<-NULL
            }else{
              AUC.tmp<-AUC
              AUC.tmp[-ind.pval.ok]<-0
              ind.chosen<-which(AUC.tmp==max(AUC.tmp))[1]
              chosen.att<<-c(chosen.att,arr.att[ind.chosen])
              if(param.verbose){
                print(paste("currente chosen:",chosen.att))
              }
              to_ret<-list("model_perf"=lst.perf[[ind.chosen]],
                           "chosen_att"=chosen.att)
            }

            return(to_ret)

          })

          print("***********END STEPWISE***************")


          names(lst.stepwise)<-paste0("att_",as.character(seq_along(1:n.att)))

          if(length(which(lengths(lst.stepwise)==0))>0){
            lst.stepwise<-lst.stepwise[-which(lengths(lst.stepwise)==0)]
          }

          if(length(lst.stepwise)==0){
            lst.stepwise.final<-list()
            to_ret<-NULL
          }else{
            lst.stepwise.final<-lst.stepwise[[length(lst.stepwise)]]

            if(k!=1){
              # fold.to.compute<-c(1:k)[-which(c(1:k) %in% ind.fold.test)]
              AUC.all.fold<-lapply(1:(k-1), function(fold.comp.test){
                continue<-TRUE
                while (continue) {
                  ind.train_set<-sample(x=rownames(df_train),size = nrow(df_train)*p.train)
                  train_set<-df_train[which(rownames(df_train) %in% ind.train_set),]
                  test_set <-df_train[-which(rownames(df_train) %in% ind.train_set),]
                  # if(param.verbose){print(table(train_set$y))}

                  perc.train<-table(train_set$y)[2]/sum(table(train_set$y))
                  perc.test<-table(test_set$y)[2]/sum(table(test_set$y))

                  # fattto check sulla stratificazione
                  if( ((perc.train>=(p.strat-0.1)) & (perc.train<=(p.strat+0.1))) & (perc.test>0 & perc.test<1)  ) continue<-FALSE

                }

                df.train<-subset(train_set,select = c(lst.stepwise.final$chosen_att,"y"))
                df.test<-subset(test_set, select = c(lst.stepwise.final$chosen_att,"y"))
                fold.to.check.mod <-train.FOMM.LR.new(df.train,
                                                  df.test,
                                                  chosen.digit = 4,
                                                  perf.train = TRUE,ev.param = ev.param,thr.acc = thr.acc)

                ret_list<-list("model_perf" =fold.to.check.mod,
                               "class count"=table(test_set$y))



                return(ret_list)
              })

              names(AUC.all.fold)<-paste0("test_fold",seq_along(1:k)[-ind.fold.test])
              to_ret<-list()
              comp_fold_name<-paste0("test_fold",ind.fold.test)
              to_ret[[comp_fold_name]]<-lst.stepwise.final
              for (i in c(1:length(names(AUC.all.fold)))) {
                to_ret[[names(AUC.all.fold)[i]]]<-AUC.all.fold[[names(AUC.all.fold)[i]]]
              }

            }else{
              # final.perf <-train.FOMM.LR(df_train = rbind(df.train,df.test),
              #                            chosen.digit = 4, perf.train = TRUE,ev.param = ev.param,thr.acc = thr.acc)

              to_ret<-lst.stepwise.final
            }
          }

          return(to_ret)
        })

        names(lst.model)<-paste0("test.fold_",as.character(seq_along(1:k)))

        # addestramento final model (modello costruito su tutto il training set con le covariate selzionate)

        if(length(which(lengths(lst.model)>0))>0){

          lst.model<-lst.model[which(lengths(lst.model)>0)]

          #CASO HOLD OUT:
          if(k==1){
            best.att<-lst.model$test.fold_1$chosen_att
            best_acc_ind<-which(lst.model$test.fold_1$model_perf$roc_test$accuracy==max(lst.model$test.fold_1$model_perf$roc_test$accuracy))
            mat.att<-matrix(ncol = length(best.att)+3)
            mat.att[1,]<-c(best.att,
                           lst.model$test.fold_1$model_perf$AUC_test,
                           lst.model$test.fold_1$model_perf$roc_test$threshold[best_acc_ind][1],
                           lst.model$test.fold_1$model_perf$roc_test$accuracy[best_acc_ind][1])
            colnames(mat.att)<-c(paste0("covariate_",seq_along(1:length(best.att))),"AUC.on.test.fold","threshold","accuracy")
            arr.acc<-NULL
            arr.AUC<-NULL
            mat.att.total<-mat.att

            if(nrow(df_test)==0){
              final.model<-train.FOMM.LR.new(df_train[,c(best.att,"y")],
                                         chosen.digit = 4,
                                         perf.train = TRUE,ev.param = ev.param,thr.acc = thr.acc)
            }else{
              final.model<-train.FOMM.LR.new(df_train[,c(best.att,"y")],
                                         df_test[,c(best.att,"y")],
                                         chosen.digit = 4,
                                         perf.train = TRUE,ev.param = ev.param,thr.acc = thr.acc)
            }

            final.models<-list("final.model"=final.model,
                               "count_train"=table(df_train$y),
                               "count_test"=nrow(df_test)
            )

            #CASO CROSS FOLD
          }else{

            best.att<-NULL
            add.col<-3
            mat.att<-matrix("",nrow = length(lst.model),ncol = n.att+add.col)
            mat.att.total<-matrix("",nrow = length(lst.model),ncol = n.att+4)


            for (i in c(1:length(lst.model))) {
              chosen.att<-lst.model[[i]][[1]][[2]]
              arr.AUC<-lapply(lst.model[[i]],function(inner.fold) {
                #QUI METTO CONDIZIONI SE VOGLIO UN ALTRO SCORE DIVERSO DA AUC.MEAN
                return(inner.fold$model_perf$AUC_test)
              })

              arr.acc<-lapply(lst.model[[i]],function(inner.fold){
                #accuratezza cambiare qui
                if(!is.null(inner.fold$model_perf)){

                  switch (acc.ind,
                          "accuracy" = {
                            max.acc<-max(inner.fold$model_perf$roc_test$accuracy,na.rm = T)
                            ret<-c(max.acc,inner.fold$model_perf$roc_test$threshold[which(inner.fold$model_perf$roc_test$accuracy==max.acc)])},

                          "b.accuracy"={
                            max.acc.test<-max(inner.fold$model_perf$roc_test$accuracy,na.rm = T)
                            max.acc.train<-max(inner.fold$model_perf$roc_train$accuracy,na.rm = T)

                            ret<-c(max.acc.test*0.632+max.acc.train*0.368,inner.fold$model_perf$roc_test$threshold[which(inner.fold$model_perf$roc_test$accuracy==max.acc)])
                          }
                  )
                  # max.acc<-max(inner.fold$model_perf$roc_test$accuracy,na.rm = T)
                  # ret<-c(max.acc,inner.fold$model_perf$roc_test$threshold[which(inner.fold$model_perf$roc_test$accuracy==max.acc)])
                }else{
                  ret<-NULL
                }
                return(ret)
              })


              if(length(chosen.att)<(ncol(mat.att)-add.col)){
                ind.diff<-(ncol(mat.att)-add.col)-length(chosen.att)
                riga<-c(chosen.att,
                        rep("",ind.diff),
                        arr.AUC[[1]],
                        arr.acc[[1]][2],
                        arr.acc[[1]][1]
                )
                riga.total<-c(chosen.att,
                              rep("",ind.diff),
                              round(mean(unlist(arr.AUC)),digits = n.digit.out),
                              round(sd(unlist(arr.AUC)),digits = n.digit.out),
                              round(mean(unlist(lapply(arr.acc, function(i){return(i[[1]])}))),digits = n.digit.out),
                              round(sd( unlist(lapply(arr.acc, function(i){return(i[[1]])}))),digits = n.digit.out))
              }else{
                riga<-c(chosen.att,
                        arr.AUC[[1]],
                        arr.acc[[1]][2],
                        arr.acc[[1]][1])
                riga.total<-c(chosen.att,
                              round(mean(unlist(arr.AUC)),digits = n.digit.out),
                              round(sd(unlist(arr.AUC)),digits = n.digit.out),
                              round(mean(unlist(lapply(arr.acc, function(i){return(i[[1]])}))),digits = n.digit.out),
                              round(sd( unlist(lapply(arr.acc, function(i){return(i[[1]])}))) ,digits = n.digit.out))
              }
              mat.att[i,]<-riga
              mat.att.total[i,]<-riga.total
            }

            colnames(mat.att)<-c(paste0("covariate_",seq_along(1:n.att)),"AUC.on.test.fold","threshold","accuracy")
            colnames(mat.att.total)<-c(paste0("covariate_",seq_along(1:n.att)),"AUC.mean.folds","AUC.sd.folds","acc.mean.folds","acc.sd.folds")
            col_to_del<-lapply(1:(ncol(mat.att.total)-add.col-1), function(colonna){
              if(length(which(mat.att.total[,colonna]==""))==nrow(mat.att.total)) to_del<-colonna
            })


            if(!is.null(unlist(col_to_del))) mat.att.total<-mat.att.total[,-unlist(col_to_del)]

            final.models<-lapply(1:length(lst.model), function(i){
              if(nrow(df_test)==0){
                final<-train.FOMM.LR.new(df_train[,c(lst.model[[i]][[1]][[2]],"y")],
                                     chosen.digit = 4,
                                     perf.train = TRUE,ev.param = ev.param,thr.acc = thr.acc)
              }else{
                final<-train.FOMM.LR.new(df_train[,c(lst.model[[i]][[1]][[2]],"y")],
                                     df_test[,c(lst.model[[i]][[1]][[2]],"y")],
                                     chosen.digit = 4,
                                     perf.train = TRUE,ev.param = ev.param,thr.acc = thr.acc)}

              return(list("final.model"=final,
                          "count_train"=table(df_train$y),
                          "count_test"=nrow(df_test)))
            })

            names(final.models)<-paste0("model",seq_along(1:length(lst.model)))
          }

        }else{
          check.flag<-FALSE
          return(list("res"=NULL,
                      "run.check"=check.flag))
        }

        lst_to_ret<-list("final.model"=final.models,
                         "lst.models.fold"=lst.model,
                         "mat.perf.total"=mat.att.total)
        to_ret<-list("res"=lst_to_ret,"run.check"=check.flag)

      }else{
        if(length(arr.ID.test)>1){
          dfTest<-df_test[,c(arr.attributes,"y")]
        }else{
          dfTest<-data.frame()
        }
        final.model<-train.FOMM.LR.new(df_train = df_train[,c(arr.attributes,"y")],
                                   df_test = dfTest,
                                   chosen.digit = 4,
                                   perf.train = TRUE,ev.param = ev.param,thr.acc = thr.acc)
        mat.att<-matrix(ncol = length(arr.attributes)+3)
        if(length(arr.ID.test)>1){

          mat.att[1,]<-c(arr.attributes,
                         final.model$AUC_test,
                         final.model$roc_test[which(final.model$roc_test$accuracy==max(final.model$roc_test$accuracy)),"accuracy"][1],
                         final.model$roc_test[which(final.model$roc_test$accuracy==max(final.model$roc_test$accuracy)),"threshold"][1]
          )
          colnames(mat.att)<-c(paste0("covariate_",seq_along(1:length(arr.attributes))),"AUC.on.test.fold","threshold","accuracy")

        }else{
          mat.att[1,]<-c(arr.attributes,
                         final.model$AUC_train,
                         final.model$roc_train[which(final.model$roc_train$accuracy==max(final.model$roc_train$accuracy)),"accuracy"][1],
                         final.model$roc_train[which(final.model$roc_train$accuracy==max(final.model$roc_train$accuracy)),"threshold"][1]
          )
          colnames(mat.att)<-c(paste0("covariate_",seq_along(1:length(arr.attributes))),"AUC.on.train.fold","accuracy","threshold")
        }
        arr.acc<-NULL
        arr.AUC<-NULL
        mat.att.total<-mat.att




        lst_to_ret<-list("final.model"=final.model,
                         "lst.models.fold"=NULL,
                         "mat.perf.total"=mat.att.total)
        to_ret<-list("res"=lst_to_ret,"run.check"=check.flag)
      }

    }else{
      to_ret<-list("res"=NULL,"run.check"=FALSE)
    }
  }

  return(to_ret)

}





train.FOMM.LR.new <- function( df_train,df_test=data.frame(),chosen.digit = 4,perf.train = TRUE,perf.test=TRUE,ev.param="acc",thr.acc=0.5) {
  if(nrow(df_test)==0){
    perf.train=TRUE
    perf.test=FALSE
  }
  df_train_tmp<-df_train
  # df_train_tmp<-df_train[,2:ncol(df_train)]
  glm.fit<-glm( y ~ ., data=df_train_tmp, family= binomial(link = "logit"))
  if(perf.test & perf.train){
    lst.perf.test<-compute.perf.fun.new(glm.fit,df_test,ev.param)
    lst.perf.train<-compute.perf.fun.new(glm.fit,df_train,ev.param)
    to_ret<-list("model"=glm.fit,"roc_test"= lst.perf.test$df_roc,"pval_test"= lst.perf.test$pval,"AUC_test" = lst.perf.test$AUC,
                 "roc_train"= lst.perf.train$df_roc,"pval_train"=lst.perf.train$pval,"AUC_train"= lst.perf.train$AUC)
  }else if(perf.train & !perf.test){
    lst.perf.train<-compute.perf.fun.new(model = glm.fit,df = df_train,ev.param)
    to_ret<-list("model"=glm.fit,"roc_train"= lst.perf.train$df_roc,"pval_train"= lst.perf.train$pval,"AUC_train"= round(lst.perf.train$AUC,digits = chosen.digit))
  }else{
    lst.perf.test<-compute.perf.fun.new(glm.fit,df_test,ev.param)
    to_ret<-list("model"=glm.fit,"roc_test"= lst.perf.test$df_roc,"pval_test"= lst.perf.test$pval,"AUC_test"= round(lst.perf.test$AUC,digits = chosen.digit))
  }

  return(to_ret)
}


compute.perf.fun.new<-function(model, df, ev.param= "acc",thr.acc=0.5){
  pred<-predict(model,newdata= df, type="response")
  df$y_prob<-pred
  df<-df[order(df$y_prob),]
  df$y_pred<-"0"
  arr.thr<-df$y_prob
  list_roc<-lapply(arr.thr, function(thr){

    df$y_pred[which(df$y_prob>thr)]<-"1"

    TP<-length(which(df$y=="1" & df$y_pred=="1"))
    TN<-length(which(df$y=="0" & df$y_pred=="0"))
    FP<-length(which(df$y=="0" & df$y_pred=="1"))
    FN<-length(which(df$y=="1" & df$y_pred=="0"))

    x_FPR<-FP/(FP+TN)
    y_TPR<- TP/(TP+FN)
    acc<-(TP+TN)/(TP+TN+FP+FN)
    return(c(thr,x_FPR,y_TPR,acc))

  })

  df_roc<-as.data.frame(do.call('rbind',list_roc))
  colnames(df_roc)<-c("threshold","FPR","TPR","accuracy")

  #calcolo AUC
  FPR<-df_roc$FPR[order(df_roc$FPR)]
  TPR<-df_roc$TPR[order(df_roc$TPR)]
  dFPR <- c(0,diff(FPR))
  dTPR <- c(0,diff(TPR))
  AUC <- sum(TPR * dFPR) + sum(dTPR * dFPR)/2



  #calcolo pval x att
  tab<-summary(model)
  pval<-tab$coefficients[,4][2:nrow(tab$coefficients)]

  if(ev.param=="acc"){
    df$y_pred[which(df$y_prob>thr.acc)]<-"1"

    TP<-length(which(df$y=="1" & df$y_pred=="1"))
    TN<-length(which(df$y=="0" & df$y_pred=="0"))
    FP<-length(which(df$y=="0" & df$y_pred=="1"))
    FN<-length(which(df$y=="1" & df$y_pred=="0"))
    df_roc$accuracy<-(TP+TN)/(TP+TN+FP+FN)
  }

  # switch (ev.param,
  #   "acc" = {
  #     ev.value=acc
  #   },
  #   "AUC"= {
  #     ev.value= AUC
  #   }
  # )

  return(list("df_roc" = df_roc,"AUC" = AUC,"pval"= pval))
}
