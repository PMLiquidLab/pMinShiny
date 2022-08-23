#'@title tab function
#'
#'




evt.tab<-function(gruppi,eventi){
  #gruppi<-list
  #eventi<-array
  gru<-rep("",length(eventi))
  df<-data.frame(eventi,gru)
  names(df)<-c("EVENT", "GROUP")

  tmp<-lapply(1:length(eventi), function(evento){
    for(i in c(2:length(gruppi))){
      if(eventi[evento] %in% gruppi[[i]]){
        gr<-names(gruppi)[i]

        break
      }else{
        gr<-eventi[evento]
      }
    }
    return(gr)
  })
  tmp<-lapply(tmp, function(x) if(is.null(x)) NA else x)
  df[,2]<-unlist(tmp)
  df<-subset(df,!is.na(df[,2]))
  return(df)
}
