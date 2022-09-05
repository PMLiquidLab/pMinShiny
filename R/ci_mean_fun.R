#'@title function for mean and ci
#'

#'@import stats




# alfa<-0.05 #alfa
# delta<-50 #ampiezza finestra temporale per il campionamento
#covariate: grandezza di interesse
#tech: 1<- calcolo degli intervalli media+t*sqrt(var/n)
#      2<- density function
#      3<- bootstrapping

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
            dx<-density(sub.group)

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


# plot(x = d_mean$time,y = d_mean$mean,type = "l",xlab = "time",ylab = "mean", ylim = c(min(d_mean$lower),max(d_mean$upper)))
# col<-col2rgb("red", alpha = FALSE)/255
# polygon(x=c(d_mean$time,
#             rev(d_mean$time)),
#         y=c(d_mean$upper, rev(d_mean$lower)),
#         col=rgb(red =col[1,1],green =col[2,1] ,blue =col[3,1] ,alpha = 0.1
#         ),
#         density = 100, angle=90)
#
#
# # lines(d_mean$time,d_mean$lower )
# # lines(d_mean$time,d_mean$upper )
# lines(d_mean$time, d_mean$mean,col="red")
# #
# #
# #   eb <- aes(ymax = df_mean$upper, ymin = df_mean$lower)
# # ggplot(data = df_mean, aes(x = time, y = mean)) +
# #   geom_line(size = 1,col="red") +
# #   geom_ribbon(eb, alpha = 0.1,fill="red")
# #
# #
# # ggplot(data=df_tot, aes(x=time, y=covariate))
# #
# #   stat_summary(fun.data ="mean_sdl", mult=1, ) + theme_bw()
# p<-array()
# for (i in c(1:length(dx_norm$x))) {
#   x<-dx_norm$x[i]
#   p[i]<-integrate(f,min(dx_norm$x),x)$value
#
#
# }


