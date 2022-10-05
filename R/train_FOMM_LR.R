
#'@import stats
#'@import graphics



#Modello di regressione Logistica su nodo FOMM
#input: eventStart      --> evento di partenza
#       eventGoal       --> outcome
#       arr.attributi   --> variabili dipendenti
#       ID.test         --> ID paz test  set
#       ID.train        --> ID paz train set
# AGGIUNGERE PARAMETRO PER SCELTA ALGORITMO

train.FOMM.LR <- function( df_train,
                           df_test,
                           plot_it=FALSE,
                           chosen.digit = 4,
                           perf.train = TRUE) {


  ######################################   TRAIN DEL MODELLO   #########################################################
  df_train_tmp<-df_train[,2:ncol(df_train)]
  # switch (cheosen.alg,
  #   "LR" = {glm.fit<-glm( y ~ ., data=df_train_tmp, family= binomial(link = "logit"))}
  #
  # )
  glm.fit<-glm( y ~ ., data=df_train_tmp, family= binomial(link = "logit"))


  #####################################  CALCOLO DELLE PRESTAZIONI #####################################################
  lst.perf.test<-compute.perf.fun(glm.fit,df_test)
  lst.perf.train<-compute.perf.fun(glm.fit,df_train)

  ###############################################  CREAZIONE PLOT #####################################################


  if(plot_it){
    df_roc<-lst.perf.test$df_roc
    plot.roc <- plot(df_roc$FPR, df_roc$TPR, type="b", xlim=c(0,1), ylim=c(0,1), lwd=2,
         xlab= "FRP",ylab="TPR")+
      lines(df_roc$FPR, df_roc$TPR)+
      abline(0,1, col="red", lty=2)+
      text(x = 0.8,y = 0.12, paste("AUC =",round(lst.perf.test$AUC,digits = chosen.digit)))
  }else{
    plot.roc<-NULL
  }


  ############################################# CREO OUTPUT ###########################################################

  if(perf.train){
    to_ret<-list("model"      = glm.fit,
                 "plot"       = plot.roc,
                 "roc_test"   = lst.perf.test$df_roc,
                 "pval_test"  = lst.perf.test$pval,
                 "AUC_test"   = lst.perf.test$AUC,
                 "roc_train"  = lst.perf.train$df_roc,
                 "pval_train" = lst.perf.train$pval,
                 "AUC_train"  = lst.perf.train$AUC )

  }else{
    to_ret<-list("model"      = glm.fit,
                 "plot"       = plot.roc,
                 "roc_test"   = lst.perf.test$df_roc,
                 "pval_test"  = lst.perf.test$pval,
                 "AUC_test"   = round(lst.perf.test$AUC,digits = chosen.digit))
  }

  return(to_ret)
}



