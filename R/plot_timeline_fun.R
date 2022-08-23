
#'@import pMineR


plot.timeline.fun<-function(objQOD, arr.id,um.time,mas.time,time.flag,id.legend){
  if(time.flag){
    mas.time= Inf
  }
  objQOD$plotTimeline(objDL.obj = c(), arr.ID = arr.id, UM = um.time, max.time=mas.time, ID.on.y.label = id.legend, Time.on.x.label = TRUE)

}
