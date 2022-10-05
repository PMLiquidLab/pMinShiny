
#'@import pMineR



#
# arr.ID.train<-sample(x = unique(all.data[[1]]$ID),size = 700)
# arr.ID.test<-unique(all.data[[1]]$ID)[-which(unique(all.data[[1]]$ID) %in% arr.ID.train)]
# eventStart<-"M_0000"
# obj.out<-ObjDL$getData()
# eventGoal<-"Dead"
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
# k=5
# p.thr=0.05
# n.att=3
# feature.selection<-TRUE
# missing.tec<-"1"
# n.digit.out=4
#
#
#

# out<-LR_FOMM_fun(eventStart = eventStart,
#                  obj.out = obj.out,
#                  eventGoal = eventGoal,
#                  arr.attributes = arr.attributi,
#                  arr.ID.train = arr.ID.train,
#                  arr.ID.test = arr.ID.test,
#                  k=5,n.att = 3,min.time = 0, max.time = Inf,passing = "Dead",NOTpassing = "Dead"
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
#                   arr.attributi =arr.attributi,
#                   arr.ID.train = arr.ID.train,
#                   arr.ID.test = arr.ID.test,
#                   feature.selection = FALSE)

#FOMM.R


LR_FOMM_fun<-function(eventStart,obj.out,eventGoal,arr.attributes, arr.ID.train,arr.ID.test,feature.selection=TRUE,k=1,p.train=0.7,
                      p.thr=0.05,n.att=2,n.digit.out=4,passing=c(),NOTpassing=c(),max.time=Inf,min.time=0,UM="days"){

  check.flag<-TRUE

  obj.QOD<- pMineR::QOD(UM = UM )
  obj.QOD$loadDataset(dataList = obj.out)
  id.1<-obj.QOD$query(from = eventStart,to = eventGoal,arr.passingThrough = passing,arr.NOTpassingThrough = NOTpassing,time.range = c(min.time,max.time))
  id.0<-obj.QOD$query(from = eventStart,to = "END",arr.NOTpassingThrough = c(eventGoal,NOTpassing),arr.passingThrough = passing, time.range = c(min.time,max.time))

  if(is.na(id.1) || is.na(id.0)){
    check.flag<-FALSE
    df_tot<-NULL
    to_ret<-list("res"=NULL,"run.check"=check.flag)
  }else{
    ID<-c(id.1,id.0)
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

    for (i in c(1:length(arr.attributes))) {
      df_tot[,arr.attributes[i]]<-as.numeric(df_tot[,arr.attributes[i]])
    }

    df_tot$y<-as.factor(df_tot$y)
    df_tot<-na.omit(df_tot)
  }


  if(is.null(df_tot)){
    check.flag<-FALSE
    data_tot<-NULL
    to_ret<-list("res"=NULL,"run.check"=check.flag)
  }else{
    df_train <-df_tot[which(df_tot$ID %in% arr.ID.train),]
    df_test  <-df_tot[which(df_tot$ID %in% arr.ID.test),]

    if(feature.selection){
      lst.fold<-split(df_train$ID,sample(1:k))

      lst.model<-lapply(1:k, function(ind.fold.test){
        #DEBUG
        print(paste("k fold:",ind.fold.test))

        if(k==1){
          #hold out
          id.train_set<-sample(x = lst.fold[[1]],size = length(lst.fold[[1]])*p.train)
          train_set<-df_train[which(df_train$ID %in% id.train_set),]
          test_set <-df_train[-which(df_train$ID %in% train_set$ID),]
        }else{
          #cross fold
          id.train_set<-as.character(unlist(x = lst.fold[-ind.fold.test],use.names = FALSE))
          train_set<-df_train[which(df_train$ID %in% id.train_set),]
          test_set <-df_train[-which(df_train$ID %in% train_set$ID),]
        }

        chosen.att<-c()


        #algoritmo stepwise: ciclo sul numero di attributi che disidero utilizzare come coviariate.
        # ad ogni iterazione calcolo il modello e guardo le performance: scelgo att con pval_train<=p.thr (soglia stabilita dall'utente) e AUC_test max
        # in OUTPUT ho lista lunga n_att (numero di covariate) in cui ogni elemento contiene: - attributo scelto i quell'iterata,
        #                                                                                     - pvalue di tutti gli attributi
        #                                                                                     - AUC di tutti gli attributi


        lst.stepwise<-lapply(1:n.att, function(att){
          print(paste("current n.att:",att))

          if(is.null(chosen.att)){
            arr.att<-arr.attributes
          }else{
            arr.att<-arr.attributes[-which(arr.attributes %in% chosen.att)]
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
            to_ret<-list("model_perf"=lst.perf[[ind.chosen]],
                         "chosen_att"=chosen.att)
          }

          return(to_ret)

        })


        names(lst.stepwise)<-paste0("att_",as.character(seq_along(1:n.att)))

        #restituisce l'ultima iterata della stepwise che sia diversa da NULL
        #es: se nell'ultima iterata non soddisfo le condizioni restituisco penultima


        if(length(which(lengths(lst.stepwise)==0))>0){
          lst.stepwise<-lst.stepwise[-which(lengths(lst.stepwise)==0)]
        }

#############DEBUG DA QUI: cosa fare quando lst.stepwise è tutta NULL
        if(length(lst.stepwise)==0){
          lst.stepwise.final<-list()
          to_ret<-NULL
        }else{
          lst.stepwise.final<-lst.stepwise[[length(lst.stepwise)]]
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
              cont<-c(count.y.0,count.y.1)
              ret_list<-list("model_perf" =fold.to.check.mod,
                             "class count"=cont)

              return(ret_list)
            })

            names(AUC.all.fold)<-paste0("test_fold",fold.to.compute)
            to_ret<-list()
            comp_fold_name<-paste0("test_fold",ind.fold.test)
            to_ret[[comp_fold_name]]<-lst.stepwise.final
            for (i in c(1:length(names(AUC.all.fold)))) {
              to_ret[[names(AUC.all.fold)[i]]]<-AUC.all.fold[[names(AUC.all.fold)[i]]]
            }


            # df_fold<-do.call('cbind',AUC.all.fold)

          }else{
            to_ret<-lst.stepwise.final
            # df_fold<-NULL
          }
        }



        # if(k!=1){
        #   #lst.stepwise.final->lista che contiene feature scelte + performance su train[all-ind.fold.test] e test:ind.fold.test
        #   #ora devo calcolare AUC media +std + accuracy media +std sui restanti test e train
        #
        #   fold.to.compute<-c(1:k)[-which(c(1:k) %in% ind.fold.test)]
        #   AUC.all.fold<-lapply(fold.to.compute, function(fold.comp.test){
        #     id.train_set<-as.character(unlist(x = lst.fold[-fold.comp.test],use.names = FALSE))
        #     train_set<-df_train[which(df_train$ID %in% id.train_set),]
        #     test_set <-df_train[-which(df_train$ID %in% train_set$ID),]
        #
        #     df.train<-subset(train_set,select = c("ID",lst.stepwise.final$chosen_att,"y"))
        #     df.test<-subset(test_set, select = c("ID",lst.stepwise.final$chosen_att,"y"))
        #     fold.to.check.mod <-train.FOMM.LR(df.train,
        #                                       df.test,
        #                                       plot_it=FALSE,
        #                                       chosen.digit = 4,
        #                                       perf.train = TRUE)
        #
        #     count.y.1<-table(test_set$y)[2]
        #     count.y.0<-table(test_set$y)[1]
        #     cont<-c(count.y.0,count.y.1)
        #     ret_list<-list("model_perf" =fold.to.check.mod,
        #                    "class count"=cont)
        #
        #     return(ret_list)
        #   })
        #
        #   names(AUC.all.fold)<-paste0("test_fold",fold.to.compute)
        #   to_ret<-list()
        #   comp_fold_name<-paste0("test_fold",ind.fold.test)
        #   to_ret[[comp_fold_name]]<-lst.stepwise.final
        #   for (i in c(1:length(names(AUC.all.fold)))) {
        #     to_ret[[names(AUC.all.fold)[i]]]<-AUC.all.fold[[names(AUC.all.fold)[i]]]
        #   }
        #
        #
        #   # df_fold<-do.call('cbind',AUC.all.fold)
        #
        # }else{
        #   to_ret<-lst.stepwise.final
        #   # df_fold<-NULL
        # }

        return(to_ret)
        # return(list("model_on_ithFold"=lst.stepwise.final,
        #             "other_test_fold"=AUC.all.fold))
      })

      names(lst.model)<-paste0("test.fold_",as.character(seq_along(1:k)))

      if(length(which(lengths(lst.model)>0))>0){
        lst.model<-lst.model[which(lengths(lst.model)>0)]
        if(k==1){
          best.att<-lst.model$test.fold_1$chosen_att
          best_acc_ind<-which(lst.model$test.fold_1$model_perf$roc_test$accuracy==max(lst.model$test.fold_1$model_perf$roc_test$accuracy))
          # best.att<-lst.model$test.fold_1[[1]]$chosen_att
          mat.att<-matrix(ncol = length(best.att)+3)
          mat.att[1,]<-c(best.att,
                         lst.model$test.fold_1$model_perf$AUC_test,
                         lst.model$test.fold_1$model_perf$roc_test$threshold[best_acc_ind][1],
                         lst.model$test.fold_1$model_perf$roc_test$accuracy[best_acc_ind][1])
          colnames(mat.att)<-c(paste0("covariate_",seq_along(1:length(best.att))),"AUC.on.test.fold","threshold","accuracy")
          arr.acc<-NULL
          arr.AUC<-NULL
          mat.att.total<-mat.att
          final.models<-train.FOMM.LR(df_train[,c("ID",best.att,"y")],
                                      df_test[,c("ID",best.att,"y")],
                                      plot_it=TRUE,
                                      chosen.digit = 4,
                                      perf.train = TRUE)
        }else{
          best.att<-NULL
          add.col<-3
          mat.att<-matrix("",nrow = length(lst.model),ncol = n.att+add.col)
          mat.att.total<-matrix("",nrow = length(lst.model),ncol = n.att+4)
          for (i in c(1:length(lst.model))) {
            chosen.att<-lst.model[[i]][[1]][[2]]
            #per ogni item di lst.model (ogni fold) calcolo AUC media e std + acc media e std con i chosen di quell'iterata:
            arr.AUC<-lapply(lst.model[[i]],function(inner.fold){
              return(inner.fold$model_perf$AUC_test)
            })

            arr.acc<-lapply(lst.model[[i]],function(inner.fold){
              max.acc<-max(inner.fold$model_perf$roc_test$accuracy)
              return(c(max.acc,inner.fold$model_perf$roc_test$threshold[which(inner.fold$model_perf$roc_test$accuracy==max.acc)]))
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
          final.models<-lapply(1:length(lst.model), function(i){
            final<-train.FOMM.LR(df_train[,c("ID",lst.model[[i]][[1]][[2]],"y")],
                                 df_test[,c("ID",lst.model[[i]][[1]][[2]],"y")],
                                 plot_it=TRUE,
                                 chosen.digit = 4,
                                 perf.train = TRUE)
            # return(final)
            return(list("final.model"=final,
                        "count_train"=nrow(df_train),
                        "count_test"=nrow(df_test)))
          })
          names(final.models)<-paste0("model",seq_along(1:length(lst.model)))

        }
      }else{
        check.flag<-FALSE
        return(NULL)
      }


      ########################################################## COSTRUZIONE DEGLI OUTPUT ########################################

      # if(k==1){
      #   best.att<-lst.model$test.fold_1$chosen_att
      #   best_acc_ind<-which(lst.model$test.fold_1$model_perf$roc_test$accuracy==max(lst.model$test.fold_1$model_perf$roc_test$accuracy))
      #   # best.att<-lst.model$test.fold_1[[1]]$chosen_att
      #   mat.att<-matrix(ncol = length(best.att)+3)
      #   mat.att[1,]<-c(best.att,
      #                  lst.model$test.fold_1$model_perf$AUC_test,
      #                  lst.model$test.fold_1$model_perf$roc_test$threshold[best_acc_ind][1],
      #                  lst.model$test.fold_1$model_perf$roc_test$accuracy[best_acc_ind][1])
      #   colnames(mat.att)<-c(paste0("covariate_",seq_along(1:length(best.att))),"AUC.on.test.fold","threshold","accuracy")
      #   arr.acc<-NULL
      #   arr.AUC<-NULL
      #   mat.att.total<-mat.att
      #   final.models<-train.FOMM.LR(df_train[,c("ID",best.att,"y")],
      #                               df_test[,c("ID",best.att,"y")],
      #                               plot_it=TRUE,
      #                               chosen.digit = 4,
      #                               perf.train = TRUE)
      # }else{
      #   best.att<-NULL
      #   # arr.acc<-list()
      #   # arr.AUC<-list()
      #   #costruisco matrice che contiene tutti gli attributi selezionati per ogni iterata della cross val
      #   add.col<-3                                          #numero di colonne contenenti informazioni aggiuntive: AUC.media+std, acc.media+std
      #   mat.att<-matrix("",nrow = k,ncol = n.att+add.col)
      #   mat.att.total<-matrix("",nrow = k,ncol = n.att+4)
      #   for (i in c(1:length(lst.model))) {
      #     chosen.att<-lst.model[[i]][[1]][[2]]
      #     #per ogni item di lst.model (ogni fold) calcolo AUC media e std + acc media e std con i chosen di quell'iterata:
      #     arr.AUC<-lapply(lst.model[[i]],function(inner.fold){
      #       return(inner.fold$model_perf$AUC_test)
      #     })
      #
      #     arr.acc<-lapply(lst.model[[i]],function(inner.fold){
      #       max.acc<-max(inner.fold$model_perf$roc_test$accuracy)
      #       return(c(max.acc,inner.fold$model_perf$roc_test$threshold[which(inner.fold$model_perf$roc_test$accuracy==max.acc)]))
      #     })
      #
      #
      #     # arr.AUC[[i]]<-as.numeric(c(unlist(lst.model[[i]]$other_test_fold[1,]),lst.model[[i]]$model_on_ithFold$AUC.best.test))
      #     # names(arr.AUC)[i]<-paste0("fold",i,": ",paste(lst.model[[i]][[1]]$chosen_att,collapse = "-"))
      #     # arr.acc[[i]]<-as.numeric(c(unlist(lst.model[[i]]$other_test_fold[2,]),lst.model[[i]]$model_on_ithFold$roc.test.best$accuracy))
      #     # names(arr.acc)[i]<-paste0("fold",i,": ",paste(lst.model[[i]][[1]]$chosen_att,collapse = "-"))
      #
      #     if(length(chosen.att)<(ncol(mat.att)-add.col)){
      #       ind.diff<-(ncol(mat.att)-add.col)-length(chosen.att)
      #       riga<-c(chosen.att,
      #               rep("",ind.diff),
      #               arr.AUC[[1]],
      #               arr.acc[[1]][2],
      #               arr.acc[[1]][1]
      #       )
      #       riga.total<-c(chosen.att,
      #                     rep("",ind.diff),
      #                     round(mean(unlist(arr.AUC)),digits = n.digit.out),
      #                     round(sd(unlist(arr.AUC)),digits = n.digit.out),
      #                     round(mean(unlist(lapply(arr.acc, function(i){return(i[[1]])}))),digits = n.digit.out),
      #                     round(sd( unlist(lapply(arr.acc, function(i){return(i[[1]])}))),digits = n.digit.out))
      #     }else{
      #       riga<-c(chosen.att,
      #               arr.AUC[[1]],
      #               arr.acc[[1]][2],
      #               arr.acc[[1]][1])
      #       riga.total<-c(chosen.att,
      #                     round(mean(unlist(arr.AUC)),digits = n.digit.out),
      #                     round(sd(unlist(arr.AUC)),digits = n.digit.out),
      #                     round(mean(unlist(lapply(arr.acc, function(i){return(i[[1]])}))),digits = n.digit.out),
      #                     round(sd( unlist(lapply(arr.acc, function(i){return(i[[1]])}))) ,digits = n.digit.out))
      #     }
      #     mat.att[i,]<-riga
      #     mat.att.total[i,]<-riga.total
      #
      #
      #
      #   }
      #
      #
      #   colnames(mat.att)<-c(paste0("covariate_",seq_along(1:n.att)),"AUC.on.test.fold","threshold","accuracy")
      #   colnames(mat.att.total)<-c(paste0("covariate_",seq_along(1:n.att)),"AUC.mean.folds","AUC.sd.folds","acc.mean.folds","acc.sd.folds")
      #   final.models<-lapply(1:k, function(i){
      #     final<-train.FOMM.LR(df_train[,c("ID",lst.model[[i]][[1]][[2]],"y")],
      #                          df_test[,c("ID",lst.model[[i]][[1]][[2]],"y")],
      #                          plot_it=TRUE,
      #                          chosen.digit = 4,
      #                          perf.train = TRUE)
      #     return(final)
      #   })
      #   names(final.models)<-paste0("model",seq_along(1:k))
      #
      # }

      # final.models<-lapply(1:k, function(i){
      #   final<-train.FOMM.LR(df_train[,c("ID",lst.model[[i]][[1]][[2]],"y")],
      #                                    df_test[,c("ID",lst.model[[i]][[1]][[2]],"y")],
      #                                    plot_it=TRUE,
      #                                    chosen.digit = 4,
      #                                    perf.train = TRUE)
      #   return(final)
      # })





      lst_to_ret<-list("final.model"=final.models,
                   "lst.models.fold"=lst.model,
                   # "best.att"=best.att,
                   # "mat.perf.featuresel"=mat.att,
                   # "lst.arr.AUC.fold"=arr.AUC,
                   # "lst.arr.acc.fold"=arr.acc,
                   "mat.perf.total"=mat.att.total)
      to_ret<-list("res"=lst_to_ret,"run.check"=check.flag)



    }else{
      final.model<-train.FOMM.LR(df_train[,c("ID",arr.attributes,"y")],
                                 df_test[,c("ID",arr.attributes,"y")],
                                 plot_it=TRUE,
                                 chosen.digit = 4,
                                 perf.train = TRUE)

      mat.att<-matrix(ncol = length(arr.attributes)+3)
      mat.att[1,]<-c(arr.attributes,
                     final.model$AUC_test,
                     final.model$roc_test[which(final.model$roc_test$accuracy==max(final.model$roc_test$accuracy)),"accuracy"][1],
                     final.model$roc_test[which(final.model$roc_test$accuracy==max(final.model$roc_test$accuracy)),"threshold"][1]
      )
      colnames(mat.att)<-c(paste0("covariate_",seq_along(1:length(arr.attributes))),"AUC.on.test.fold","threshold","accuracy")
      arr.acc<-NULL
      arr.AUC<-NULL
      mat.att.total<-mat.att

      lst_to_ret<-list("final.model"=final.model,
                   "lst.models.fold"=NULL,
                   # "best.att"=best.att,
                   # "mat.perf.featuresel"=mat.att,
                   # "lst.arr.AUC.fold"=arr.AUC,
                   # "lst.arr.acc.fold"=arr.acc,
                   "mat.perf.total"=mat.att.total)
      to_ret<-list("res"=lst_to_ret,"run.check"=check.flag)





      # to_ret<-list("final.model"=final.model,
      #              "best.att"=arr.attributi,
      #              "mat.perf.featuresel"=mat.att,
      #              "lst.arr.AUC.fold"=arr.AUC,
      #              "lst.arr.acc.fold"=arr.acc,
      #              "mat.perf.total"=mat.att.total)
    }
  }

  # if(is.na(id.0)){
  #   check.flag<-FALSE
  # }


  # ID<-c(id.1,id.0)
  #
  # tmp<-lapply(ID, function(id){
  #   att.val<-search_value(sub.path=obj.out$pat.process[[id]],eventoDiPartenza,arr.attributi)
  #   if(id %in% id.1){
  #     y.val<-1
  #   }else{
  #     y.val<-0
  #   }
  #   return(cbind(id,att.val,y.val))
  # })
  #
  # df_tot<-as.data.frame(do.call("rbind",tmp))
  # colnames(df_tot)<-c("ID",arr.attributi,"y")
  #
  # for (i in c(1:length(arr.attributi))) {
  #   df_tot[,arr.attributi[i]]<-as.numeric(df_tot[,arr.attributi[i]])
  # }
  #
  # df_tot$y<-as.factor(df_tot$y)



  #################################### MISSING VALUES ########################################
  # STRATRGIE:  - 1 ---> na.omit di tutte le righe con almeno 1 na
  #             - 2 ---> elimino colonne con na > 30% + elimino righe in cui restano NA
  #             - 3 ---> elimino righe con NA >30% + elimino colonne con na > 30% + media sui restanti na

  # switch (missing.tec,
  #   "1" = {
  #     df_tot<-na.omit(df_tot)},
  #
  #   "2" = {
  #     p.check<-round(nrow(df_tot)*0.3)
  #     col.to.keep<-c()
  #     for (i in c(1:ncol(df_tot))) {
  #       if(length(which(is.na(df_tot[,i])))<=p.check){
  #         col.to.keep<-c(col.to.keep,colnames(df_tot)[i])
  #         }
  #       }
  #     df_tot<-subset(df_tot, select = col.to.keep )
  #     arr.attributi<-colnames(df_tot)[-which(colnames(df_tot) %in% c("ID","y"))]
  #     df_tot<-na.omit(df_tot)},
  #
  #   #### 3: probabile stupidagine
  #   "3" = {
  #     p.check<-round(ncol(df_tot)*0.3)
  #     row.to.keep<-c()
  #     for (i in c(1:nrow(df_tot))) {
  #       if(length(which(is.na(df_tot[i,])))<=p.check){
  #         row.to.keep<-c(row.to.keep,i)
  #       }
  #     }
  #     df_tot<-df_tot[row.to.keep,]
  #
  #     p.check<-round(nrow(df_tot)*0.3)
  #     col.to.keep<-c()
  #     for (i in c(1:ncol(df_tot))) {
  #       if(length(which(is.na(df_tot[,i])))<=p.check){
  #         col.to.keep<-c(col.to.keep,colnames(df_tot)[i])
  #         if(length(which(is.na(df_tot[,i])))>0){
  #           df_tot[which(is.na(df_tot[,i])),i]<-mean(df_tot[,i],na.rm = TRUE)
  #         }
  #       }
  #     }
  #     df_tot<-subset(df_tot, select = col.to.keep)
  #     arr.attributi<-colnames(df_tot)[-which(colnames(df_tot) %in% c("ID","y"))]}
  #   )

  ###################################   DIVISIONE TRAIN - TEST #########################################


  # df_train <-df_tot[which(df_tot$ID %in% arr.ID.train),]
  # df_test  <-df_tot[which(df_tot$ID %in% arr.ID.test),]

  # if(nrow(df_train)==0){
  #   stop("No istances in train set")
  #   #OUT LIST, CON FLAG TRUE= BUON FINE RUNNING
  # }
  #
  # if(nrow(df_test)==0){
  #   stop("No istances in test set")
  # }




  # if(feature.selection){
  #   lst.fold<-split(df_train$ID,sample(1:k))
  #
  #   lst.model<-lapply(1:k, function(ind.fold.test){
  #     #DEBUG
  #     print(paste("k fold:",ind.fold.test))
  #
  #     if(k==1){
  #       #hold out
  #       id.train_set<-sample(x = lst.fold[[1]],size = length(lst.fold[[1]])*p.train)
  #       train_set<-df_train[which(df_train$ID %in% id.train_set),]
  #       test_set <-df_train[-which(df_train$ID %in% train_set$ID),]
  #     }else{
  #       #cross fold
  #       id.train_set<-as.character(unlist(x = lst.fold[-ind.fold.test],use.names = FALSE))
  #       train_set<-df_train[which(df_train$ID %in% id.train_set),]
  #       test_set <-df_train[-which(df_train$ID %in% train_set$ID),]
  #     }
  #
  #     chosen.att<-c()
  #
  #
  #     #algoritmo stepwise: ciclo sul numero di attributi che disidero utilizzare come coviariate.
  #     # ad ogni iterazione calcolo il modello e guardo le performance: scelgo att con pval_train<=p.thr (soglia stabilita dall'utente) e AUC_test max
  #     # in OUTPUT ho lista lunga n_att (numero di covariate) in cui ogni elemento contiene: - attributo scelto i quell'iterata,
  #     #                                                                                     - pvalue di tutti gli attributi
  #     #                                                                                     - AUC di tutti gli attributi
  #
  #
  #     lst.stepwise<-lapply(1:n.att, function(att){
  #       print(paste("current n.att:",att))
  #
  #       if(is.null(chosen.att)){
  #         arr.att<-arr.attributi
  #       }else{
  #         arr.att<-arr.attributi[-which(arr.attributi %in% chosen.att)]
  #       }
  #
  #
  #       lst.perf<-list()
  #       pval<-list()
  #       AUC<-c()
  #       roc_test<-list()
  #
  #       for (i in c(1:length(arr.att))) {
  #         df.train<-subset(train_set,select = c("ID",c(chosen.att,arr.att[i]),"y"))
  #         df.test<-subset(test_set, select = c("ID",c(chosen.att,arr.att[i]),"y"))
  #         lst.perf[[i]]<-train.FOMM.LR(df.train,
  #                                      df.test,
  #                                      plot_it=FALSE,
  #                                      chosen.digit = 4,
  #                                      perf.train = TRUE)
  #
  #         pval[[i]]<-lst.perf[[i]]$pval_train
  #         AUC[i]<-lst.perf[[i]]$AUC_test
  #         roc_test[[i]]<-lst.perf[[i]]$roc_test[which(lst.perf[[i]]$roc_test$accuracy==max(lst.perf[[i]]$roc_test$accuracy)),]
  #         names(AUC)[i]<-paste0(i,"-",arr.att[i])
  #       }
  #
  #       pval_tot<-do.call('rbind',pval)
  #       roc_test_all<-do.call('rbind',roc_test)
  #
  #       if(ncol(pval_tot)==1){
  #         ind.pval.ok<-which(pval_tot<=p.thr)
  #       }else{
  #         mat_log<-pval_tot<=p.thr
  #         for(j in c(1:ncol(pval_tot)-1)){
  #           new_col<-"&"(mat_log[,1],mat_log[,2])
  #           mat_log<-mat_log[,-c(j,j+1)]
  #           mat_log<-cbind(mat_log,new_col)
  #         }
  #         ind.pval.ok<-which(new_col)
  #       }
  #
  #       if(identical(ind.pval.ok,integer(0))){
  #         #caso in cui non ho trovato righe con pvalue su train <p.thr
  #         chosen.att<<-c(chosen.att,NULL)
  #         print("non ci sono p bassi")
  #         to_ret<-NULL
  #
  #
  #       }else{
  #         AUC.tmp<-AUC
  #         AUC.tmp[-ind.pval.ok]<-0
  #         ind.chosen<-which(AUC.tmp==max(AUC.tmp))[1]
  #         chosen.att<<-c(chosen.att,arr.att[ind.chosen])
  #         print(paste("currente chosen:",chosen.att))
  #         to_ret<-list("model_perf"=lst.perf[[ind.chosen]],
  #                      "chosen_att"=chosen.att)
  #       }
  #
  #       return(to_ret)
  #
  #     })
  #
  #
  #     names(lst.stepwise)<-paste0("att_",as.character(seq_along(1:n.att)))
  #
  #     #restituisce l'ultima iterata della stepwise che sia diversa da NULL
  #     #es: se nell'ultima iterata non soddisfo le condizioni restituisco penultima
  #
  #
  #     if(length(which(lengths(lst.stepwise)==0))>0){
  #       lst.stepwise<-lst.stepwise[-which(lengths(lst.stepwise)==0)]
  #     }
  #
  #
  #     if(length(lst.stepwise)==0){
  #       lst.stepwise.final<-list()
  #     }else{
  #       lst.stepwise.final<-lst.stepwise[[length(lst.stepwise)]]
  #     }
  #
  #     if(k!=1){
  #       #lst.stepwise.final->lista che contiene feature scelte + performance su train[all-ind.fold.test] e test:ind.fold.test
  #       #ora devo calcolare AUC media +std + accuracy media +std sui restanti test e train
  #
  #       fold.to.compute<-c(1:k)[-which(c(1:k) %in% ind.fold.test)]
  #       AUC.all.fold<-lapply(fold.to.compute, function(fold.comp.test){
  #         id.train_set<-as.character(unlist(x = lst.fold[-fold.comp.test],use.names = FALSE))
  #         train_set<-df_train[which(df_train$ID %in% id.train_set),]
  #         test_set <-df_train[-which(df_train$ID %in% train_set$ID),]
  #
  #         df.train<-subset(train_set,select = c("ID",lst.stepwise.final$chosen_att,"y"))
  #         df.test<-subset(test_set, select = c("ID",lst.stepwise.final$chosen_att,"y"))
  #         fold.to.check.mod <-train.FOMM.LR(df.train,
  #                                           df.test,
  #                                           plot_it=FALSE,
  #                                           chosen.digit = 4,
  #                                           perf.train = TRUE)
  #
  #         count.y.1<-table(test_set$y)[2]
  #         count.y.0<-table(test_set$y)[1]
  #         cont<-c(count.y.0,count.y.1)
  #         ret_list<-list("model_perf" =fold.to.check.mod,
  #                        "class count"=cont)
  #
  #         return(ret_list)
  #       })
  #
  #       names(AUC.all.fold)<-paste0("test_fold",fold.to.compute)
  #       to_ret<-list()
  #       comp_fold_name<-paste0("test_fold",ind.fold.test)
  #       to_ret[[comp_fold_name]]<-lst.stepwise.final
  #       for (i in c(1:length(names(AUC.all.fold)))) {
  #         to_ret[[names(AUC.all.fold)[i]]]<-AUC.all.fold[[names(AUC.all.fold)[i]]]
  #       }
  #
  #
  #       # df_fold<-do.call('cbind',AUC.all.fold)
  #
  #     }else{
  #       to_ret<-lst.stepwise.final
  #       # df_fold<-NULL
  #     }
  #
  #     return(to_ret)
  #     # return(list("model_on_ithFold"=lst.stepwise.final,
  #     #             "other_test_fold"=AUC.all.fold))
  #   })
  #
  #   names(lst.model)<-paste0("test.fold_",as.character(seq_along(1:k)))
  #
  #   ########################################################## COSTRUZIONE DEGLI OUTPUT ########################################
  #
  #   if(k==1){
  #     best.att<-lst.model$test.fold_1$chosen_att
  #     best_acc_ind<-which(lst.model$test.fold_1$model_perf$roc_test$accuracy==max(lst.model$test.fold_1$model_perf$roc_test$accuracy))
  #     # best.att<-lst.model$test.fold_1[[1]]$chosen_att
  #     mat.att<-matrix(ncol = length(best.att)+3)
  #     mat.att[1,]<-c(best.att,
  #                    lst.model$test.fold_1$model_perf$AUC_test,
  #                    lst.model$test.fold_1$model_perf$roc_test$threshold[best_acc_ind][1],
  #                    lst.model$test.fold_1$model_perf$roc_test$accuracy[best_acc_ind][1])
  #     colnames(mat.att)<-c(paste0("covariate_",seq_along(1:length(best.att))),"AUC.on.test.fold","threshold","accuracy")
  #     arr.acc<-NULL
  #     arr.AUC<-NULL
  #     mat.att.total<-mat.att
  #     final.models<-train.FOMM.LR(df_train[,c("ID",best.att,"y")],
  #                          df_test[,c("ID",best.att,"y")],
  #                          plot_it=TRUE,
  #                          chosen.digit = 4,
  #                          perf.train = TRUE)
  #   }else{
  #     best.att<-NULL
  #     # arr.acc<-list()
  #     # arr.AUC<-list()
  #     #costruisco matrice che contiene tutti gli attributi selezionati per ogni iterata della cross val
  #     add.col<-3                                          #numero di colonne contenenti informazioni aggiuntive: AUC.media+std, acc.media+std
  #     mat.att<-matrix("",nrow = k,ncol = n.att+add.col)
  #     mat.att.total<-matrix("",nrow = k,ncol = n.att+4)
  #     for (i in c(1:length(lst.model))) {
  #       chosen.att<-lst.model[[i]][[1]][[2]]
  #       #per ogni item di lst.model (ogni fold) calcolo AUC media e std + acc media e std con i chosen di quell'iterata:
  #       arr.AUC<-lapply(lst.model[[i]],function(inner.fold){
  #         return(inner.fold$model_perf$AUC_test)
  #       })
  #
  #       arr.acc<-lapply(lst.model[[i]],function(inner.fold){
  #         max.acc<-max(inner.fold$model_perf$roc_test$accuracy)
  #         return(c(max.acc,inner.fold$model_perf$roc_test$threshold[which(inner.fold$model_perf$roc_test$accuracy==max.acc)]))
  #       })
  #
  #
  #       # arr.AUC[[i]]<-as.numeric(c(unlist(lst.model[[i]]$other_test_fold[1,]),lst.model[[i]]$model_on_ithFold$AUC.best.test))
  #       # names(arr.AUC)[i]<-paste0("fold",i,": ",paste(lst.model[[i]][[1]]$chosen_att,collapse = "-"))
  #       # arr.acc[[i]]<-as.numeric(c(unlist(lst.model[[i]]$other_test_fold[2,]),lst.model[[i]]$model_on_ithFold$roc.test.best$accuracy))
  #       # names(arr.acc)[i]<-paste0("fold",i,": ",paste(lst.model[[i]][[1]]$chosen_att,collapse = "-"))
  #
  #       if(length(chosen.att)<(ncol(mat.att)-add.col)){
  #         ind.diff<-(ncol(mat.att)-add.col)-length(chosen.att)
  #         riga<-c(chosen.att,
  #                 rep("",ind.diff),
  #                 arr.AUC[[1]],
  #                 arr.acc[[1]][2],
  #                 arr.acc[[1]][1]
  #                 )
  #         riga.total<-c(chosen.att,
  #                       rep("",ind.diff),
  #                           round(mean(unlist(arr.AUC)),digits = n.digit.out),
  #                           round(sd(unlist(arr.AUC)),digits = n.digit.out),
  #                           round(mean(unlist(lapply(arr.acc, function(i){return(i[[1]])}))),digits = n.digit.out),
  #                           round(sd( unlist(lapply(arr.acc, function(i){return(i[[1]])}))),digits = n.digit.out))
  #       }else{
  #         riga<-c(chosen.att,
  #                 arr.AUC[[1]],
  #                 arr.acc[[1]][2],
  #                 arr.acc[[1]][1])
  #         riga.total<-c(chosen.att,
  #                       round(mean(unlist(arr.AUC)),digits = n.digit.out),
  #                       round(sd(unlist(arr.AUC)),digits = n.digit.out),
  #                       round(mean(unlist(lapply(arr.acc, function(i){return(i[[1]])}))),digits = n.digit.out),
  #                       round(sd( unlist(lapply(arr.acc, function(i){return(i[[1]])}))) ,digits = n.digit.out))
  #       }
  #       mat.att[i,]<-riga
  #       mat.att.total[i,]<-riga.total
  #
  #
  #
  #     }
  #
  #
  #     colnames(mat.att)<-c(paste0("covariate_",seq_along(1:n.att)),"AUC.on.test.fold","threshold","accuracy")
  #     colnames(mat.att.total)<-c(paste0("covariate_",seq_along(1:n.att)),"AUC.mean.folds","AUC.sd.folds","acc.mean.folds","acc.sd.folds")
  #     final.models<-lapply(1:k, function(i){
  #       final<-train.FOMM.LR(df_train[,c("ID",lst.model[[i]][[1]][[2]],"y")],
  #                            df_test[,c("ID",lst.model[[i]][[1]][[2]],"y")],
  #                            plot_it=TRUE,
  #                            chosen.digit = 4,
  #                            perf.train = TRUE)
  #       return(final)
  #     })
  #     names(final.models)<-paste0("model",seq_along(1:k))
  #
  #   }
  #
  #   # final.models<-lapply(1:k, function(i){
  #   #   final<-train.FOMM.LR(df_train[,c("ID",lst.model[[i]][[1]][[2]],"y")],
  #   #                                    df_test[,c("ID",lst.model[[i]][[1]][[2]],"y")],
  #   #                                    plot_it=TRUE,
  #   #                                    chosen.digit = 4,
  #   #                                    perf.train = TRUE)
  #   #   return(final)
  #   # })
  #
  #
  #
  #
  #
  #   to_ret<-list("final.model"=final.models,
  #                "lst.models.fold"=lst.model,
  #                # "best.att"=best.att,
  #                # "mat.perf.featuresel"=mat.att,
  #                # "lst.arr.AUC.fold"=arr.AUC,
  #                # "lst.arr.acc.fold"=arr.acc,
  #                "mat.perf.total"=mat.att.total)
  #
  #
  #
  #
  # }else{
  #   final.model<-train.FOMM.LR(df_train[,c("ID",arr.attributi,"y")],
  #                              df_test[,c("ID",arr.attributi,"y")],
  #                              plot_it=TRUE,
  #                              chosen.digit = 4,
  #                              perf.train = TRUE)
  #
  #   mat.att<-matrix(ncol = length(arr.attributi)+3)
  #   mat.att[1,]<-c(arr.attributi,
  #                  final.model$AUC_test,
  #                  final.model$roc_test[which(final.model$roc_test$accuracy==max(final.model$roc_test$accuracy)),"accuracy"][1],
  #                  final.model$roc_test[which(final.model$roc_test$accuracy==max(final.model$roc_test$accuracy)),"threshold"][1]
  #                  )
  #   colnames(mat.att)<-c(paste0("covariate_",seq_along(1:length(arr.attributi))),"AUC.on.test.fold","threshold","accuracy")
  #   arr.acc<-NULL
  #   arr.AUC<-NULL
  #   mat.att.total<-mat.att
  #
  #   to_ret<-list("final.model"=final.model,
  #                "lst.models.fold"=NULL,
  #                # "best.att"=best.att,
  #                # "mat.perf.featuresel"=mat.att,
  #                # "lst.arr.AUC.fold"=arr.AUC,
  #                # "lst.arr.acc.fold"=arr.acc,
  #                "mat.perf.total"=mat.att.total)
  #
  #
  #
  #
  #
  #   # to_ret<-list("final.model"=final.model,
  #   #              "best.att"=arr.attributi,
  #   #              "mat.perf.featuresel"=mat.att,
  #   #              "lst.arr.AUC.fold"=arr.AUC,
  #   #              "lst.arr.acc.fold"=arr.acc,
  #   #              "mat.perf.total"=mat.att.total)
  # }

  return(to_ret)

}
