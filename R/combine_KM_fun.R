#'@title creo struttura per combinare tutti i plot delle km
#'
#'@import survival
#'
#'
#'



combine_km_fun<-function(list.path,path.plot){

  data_tot<-data.frame()
  for (i in c(1:length(list.path))) {
    name<-paste0("path",i)
    pat<-all.path[[name]]


    out.fun<-KM_CFM(ObjCFM = ObjCFM,id.start = pat$id.start,
                    id.end = pat$id.end,
                    cens.leaf = pat$cens.leaf,
                    id.cens = pat$id.cens,
                    ObjDL = ObjDL,
                    UM = pat$um.time,
                    min_time = pat$min_time,
                    max_time = pat$max_time)


    if(!is.null(out.fun) & paste("Path",i) %in% path.plot){
      out.fun<-cbind(out.fun$data, path= paste("path",i))
      data_tot<-rbind(data_tot,out.fun)
    }
  }

  if(!is.null(data_tot)){
    final.surv<-survfit(Surv(time, outcome) ~ path, data = data_tot)

    # output$km.curves<-renderPlot(ggsurvplot(final.surv))
  }else{
    final.surv<-NULL
  }

  return(final.surv)

}
