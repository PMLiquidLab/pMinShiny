#'@title function for mean and ci
#'

#'@importFrom stats density


ci.mean.fun<-function(alfa,delta,df_tot,covariate,tech="1"){


  ci<-1-alfa
  time_sub<- seq (from=0, to=max(df_tot$time), by=1)
  df_tot<-df_tot[order(df_tot$time),]

  #calcolo meadia e int conf
  list_out<-lapply(time_sub, function(cent){
    lower.wind<-cent-delta/2
    upper.wind<-cent+delta/2
    sub.group<-df_tot[which(df_tot$time<=upper.wind & df_tot$time>=lower.wind),"covariate"]
    if(!is.null(sub.group)){
      if(length(sub.group)>1){
        switch (tech,
          "1" = {
            mean.out<-mean(sub.group)
            marg.err <- qt(ci + (1 - ci)/2, df = length(sub.group) - 1) * sd(sub.group)/sqrt(length(sub.group))
            ci.lower<-mean.out-marg.err
            ci.upper<-mean.out+marg.err
            to.ret<-c(mean.out,ci.lower,ci.upper,cent)
          },

          "2" = {
            dx<-stats::density(sub.group)

          },
          "3" = {

          }

        )

      }else{
        to.ret<-c(NA,NA,NA,NA)
      }


    }else{
      to.ret<-c(NA,NA,NA,NA)
    }

    return(to.ret)
  })

  df_mean <- do.call('rbind', list_out)
  df_mean<-as.data.frame(df_mean)

  colnames(df_mean)<-c("mean","lower","upper","time")
  df_mean<-na.omit(df_mean)
  return(df_mean)
}




