#'@importFrom stats predict

compute.perf.fun<-function(model, df){
  pred<-predict(model,newdata= df, type="response")
  df$y_prob<-pred


  #######################################    CALCOLO ROC        ###################################################
  #1. calcolo array possibili soglie: th : se p<=th --> y_pred=0
  #                                           p> th --> y_pred=1
  #2. per ogni th costruisco matrice di confusione:               y_pred=1   y_pred=0
  #
  #                                                   y=1             TP       FN
  #                                                   y=0             FP       TN
  #
  #3. calcolo punti x costruire la ROC: x(sens)   = TP/(TP + FN)
  #                                     y(1-spec) = 1- (TN/(TN+FP))

  df<-df[order(df$y_prob),]
  arr.thr<-df$y_prob

  list_roc<-lapply(arr.thr, function(thr){

    df$y_pred<-"0"
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
  pval<-tab$coefficients[2:nrow(tab$coefficients),4]

  return(list("df_roc" = df_roc,
              "AUC"    = AUC,
              "pval"   = pval))
}
