#'@title trace function
#'
#'@import pMineR
#'





#param: chosen-> sel event
#       objDL.new-> dataLoader obj
#       time_start,time_end -> time window ()--> Da settare nella main page in modo da non ricaricare l'event log ad ogni chiamata
#       cum.flag -> cumulative flag
#       temp.scale -> temporal scale (asse x)



trace.evolution<-function(chosen,objDL.new,cum.flag,temp.scale, max.time){
  # objDL.new$applyFilter(array.events.to.keep = chosen)
  objDL.out <-objDL.new$getData()
  objQOD <- QOD()
  objQOD$loadDataset(dataList = objDL.out)
  objQOD$plotTraceEvolution(objDL.out = objDL.out,UM = temp.scale,cumulative = cum.flag,arr.events = chosen, max.t= max.time)
  # objQOD$plotTraceEvolution(objDL.out = objDL.out,holdEvts = cum.flag, UM = temp.scale)
}
