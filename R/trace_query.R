
#'@title trace id
#'
#'@import pMineR
#'




trace.id<-function(Obj.QOD, event.start,event.end, time.b, time.a, inf.flag,um.time,event.between,event.NOT.between, comp.mat=TRUE){
  if(inf.flag){
    time.r<-c(time.b,Inf)
  }else{
    time.r<-c(time.b,time.a)
  }


  arr.ID <- Obj.QOD$query(from = event.start,
                          to = event.end,
                          UM= um.time,
                          arr.passingThrough =event.between,
                          time.range= time.r,
                          arr.NOTpassingThrough = event.NOT.between,
                          returnCompleteMatrix =comp.mat)

  return(arr.ID)
}






