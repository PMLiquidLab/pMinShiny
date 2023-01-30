#'@import pMineR
#'@importFrom timevis timevis



timeLine.data<-function(arr.ID,
                        objDL.out,
                        single= TRUE){
  if(single){
    df<-as.data.frame(objDL.out$pat.process[[as.character(arr.ID)]])
  }else{
    pat_list<-list()
    for (i in c(1:length(arr.ID))) {
      i<-arr.ID[i]
      pat_list[[i]]<-objDL.out$pat.process[[as.character(i)]]
    }
    df <- do.call("rbind", pat_list)
  }



  df$DATE_INI<-format(as.Date(df$DATE_INI,"%d/%m/%Y %H:%M:%S"),"%Y-%m-%d %H:%M:%S")
  df$DATE_END<-format(as.Date(df$DATE_END),"%Y-%m-%d %H:%M:%S")
  df<-na.omit(df)
  df$DATE_END[which(df$DATE_INI==df$DATE_END)]<-NA


 graph<-timevis(data = data.frame(
   id      = df$pMineR.internal.ID.Evt,
   content = df$EVENT,
   start   = df$DATE_INI,
   end     = df$DATE_END,
   group   = df$ID),
   groups = data.frame(id= unique(df$ID),content = as.character(unique(df$ID)) )
   )
 return(graph)
}



