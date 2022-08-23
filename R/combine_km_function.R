#'@title creo struttura per combinare tutti i plot delle km
#'
#'@import survival



render.km.graph<-function(list.path,path.plot){
  df.surv<-data.frame()
  id.not.valid<-c()

  for (i in c(1:length(list.path))) {
    name<-names(list.path)[i]
    # name<-paste0("path",i)
    # if(!is.null(all.path[[name]])){
      pat<-all.path[[name]]

      out.fun<-KM_CFM(ObjCFM = ObjCFM,id.start = pat$id.start,
                      id.end = pat$id.end,
                      cens.leaf = pat$cens.leaf,
                      id.cens = pat$id.cens,
                      ObjDL = ObjDL,
                      UM = pat$um.time,
                      min_time = pat$min_time,
                      max_time = pat$max_time)

      if(!is.null(out.fun)){
        out.fun<-cbind(out.fun$data.surv, path= name)
        df.surv<-rbind(df.surv,out.fun)
      }else{
        id.not.valid<-c(id.not.valid,i)
      }




    # out.fun<-KM_CFM(ObjCFM = ObjCFM,id.start = pat$id.start,
    #                 id.end = pat$id.end,
    #                 cens.leaf = pat$cens.leaf,
    #                 id.cens = pat$id.cens,
    #                 ObjDL = ObjDL,
    #                 UM = pat$um.time,
    #                 min_time = pat$min_time,
    #                 max_time = pat$max_time)
    #
    # if(!is.null(out.fun)){
    #   out.fun<-cbind(out.fun$data.surv, path= paste0("path",i))
    #   df.surv<-rbind(df.surv,out.fun)
    # }else{
    #   id.not.valid<-c(id.not.valid,i)
    # }
  }



  for (i in c(1:length(path.plot))) {
    path.plot[i]<- gsub(" ","",path.plot[i])
    path.plot[i]<-tolower(path.plot[i])
  }



  if(!length(df.surv)==0){
    final.surv<-survfit(Surv(time, outcome) ~ path, data = df.surv[df.surv$path %in% path.plot,])
    to_ret<-list("final.surv"=final.surv,
                 "final.data"=df.surv,
                 "id.not.valid"= id.not.valid)
    }else{
      to_ret<-id.not.valid
      }

  return(to_ret)
}
