#'@import pMineR

# # # depth= 2 ,
# # starting.ID = "root",
# # currentLevel = 0,
# # total.hits = 0,
# # # kindOfGraph = "twopi",
# # GraphFontsize = "9" ,
# # # withPercentages = TRUE,
# # # relative.percentages = FALSE,
# # # proportionalPenwidth=TRUE ,
# # default.arcColor = "Black",
# # proportionalPenwidth.k.thickness = 5,
# # # arr.States.color=c(),
# # arr.States.color.shades = FALSE,
# # arr.States.color.shades.thresholds = c(25,50,75),
# # # predictive.model = FALSE,
# # predictive.model.outcome = "",
# # predictive.model.skipNodeLabel = c(),
# # # predictive.model.engine.type = "default",
# # predictive.model.engine.parameter = list(),
# # # preserve.topology = FALSE,
# # set.to.gray = FALSE,
# # set.to.gray.color= "WhiteSmoke" ,
# # debug.it = FALSE,
# # # show.far.leaf = FALSE,
# # # show.median.time.from.root = FALSE,
# # heatmap.based.on.median.time = FALSE ,
# # # heatmap.base.color = "Khaki",
# # abs.threshold = NA , nodeShape = "oval"
#
#
#
#
#
#
# # #HITS
# # script <- ObjCFM$plotCFGraphComparison(stratifyFor = "Sex",
# #                                        stratificationValues = c("F","M"),
# #                                        depth = Inf,
# #                                        abs.threshold = 12,
# #                                        kindOfGraph = "dot",
# #                                        nodeShape = "square")$script
# # grViz(script)
# #
# #
# # #TIME
# # script <- ObjCFM$plotCFGraphComparison(stratifyFor = "Sex",
# #                                        stratificationValues = c("F","M"),
# #                                        depth = Inf,
# #                                        checkDurationFromRoot = TRUE,
# #                                        abs.threshold = 12,
# #                                        kindOfGraph = "dot",
# #                                        nodeShape = "square")$script
# # grViz(script)
# #
# # #%TODIE
# # script <- ObjCFM$plotCFGraphComparison(stratifyFor = "Sex",
# #                                        stratificationValues = c("F","M"),
# #                                        depth = Inf,
# #                                        hitsMeansReachAGivenFinalState = TRUE,
# #                                        finalStateForHits = "Death" ,
# #                                        abs.threshold = 12,
# #                                        kindOfGraph = "dot",
# #                                        nodeShape = "square" )$script
# # grViz(script)
# #
#
# #PARAM: ObjCFM--> CFM
# #       strat.var--> nome var stratificazione
# #       strat.value--> possible value 4 strat (max 2)
# #       cat.flag-->è una var categorica?
# #       hit.flag--> hiit
# #       time.flag-->show duration time
# #       perc.end --> % hits all'ultimo evento
# #       finalState-->evento finale
#
#
#
# cf_pred<-function(ObjCFM,
#                   max_depth= Inf,
#                   support,
#                   outcome,
#                   col.pred
#                   ){
#
#  if(is.na(col.pred)){
#    sub.shades<-c()
#
#  }else{
#    shades<-c("Red","Coral","DarkOrange","Orange","LightSalmon","SandyBrown","Chocolate","Gold")
#    len<-length(col.pred)+1
#    sub.shades<-shades[1:len]
#    names<-c(outcome,col.pred)
#    names(sub.shades)<-names
#  }
#
#
#  script<- ObjCFM$plotCFGraph(depth = max_depth,
#                              predictive.model = TRUE,
#                              predictive.model.outcome = outcome,
#                              arr.States.color = sub.shades,
#                              abs.threshold = 12,
#                              kindOfGraph = "dot",
#                              nodeShape = "square")$script
#
#   grViz(script)
# }
#
#
# lst.nodi<-ObjCFM$getDataStructure()$lst.nodi
# loadedDataset<-ObjDL$getData()
# prova<-array()
#
#
# for (i in c(1:length(id.nodi))) {
#    son<-as.character(id.nodi[i])
#    tmp.tempi<-unlist(lapply(lst.nodi[[son]]$IPP, function(tmpIPP)
#    { loadedDataset$pat.process[[tmpIPP]][lst.nodi[[son]]$depth,"pMineR.deltaDate"] }))
#    if( length(tmp.tempi) > 0) {
#       tmp.tempi <- as.numeric(unlist(lapply(tmp.tempi,function(x){  format((x/(24*60)),digits=3) })))
#       med<-median(tmp.tempi)
#    }else{
#       tmp.tempi<-NA
#       med<-NA
#    }
#    prova[i]<-med
# }
#
# cf.graph<-ObjCFM$plotCFGraph(depth = Inf,heatmap.based.on.median.time = col,kindOfGraph = "dot",
#                              nodeShape = "square",show.median.time.from.root = TRUE, abs.threshold = 20)
# len<-length(cf.graph$arr.nodi)
# id.nodi<-array()
# for (i in c(1:len)) {
#    id.nodi[i]<-strsplit(cf.graph$arr.nodi[i],"'")[[1]][2]
# }
#
# id.nodi<-as.numeric(id.nodi[2:length(id.nodi)])
#
# lapply(id.nodi, function(id){
#    substring(id,2,length(id-2))
#
# })
#
#
# son<-"200"
# lst.nodi<-ObjCFM$getDataStructure()$lst.nodi
# loadedDataset<-ObjDL$getData()
# tmp.tempi <- unlist(lapply(lst.nodi[[son]]$IPP, function(tmpIPP)
#    { loadedDataset$pat.process[[tmpIPP]][lst.nodi[[son]]$depth,"pMineR.deltaDate"] }))
# if( length(tmp.tempi) > 0) {
#    tmp.tempi <- as.numeric(unlist(lapply(tmp.tempi,function(x){  format((x/(24*60)),digits=3) })))
#    stringa.tempi <- paste( c( "\n",min(tmp.tempi)," - ",median(tmp.tempi)," - ",max(tmp.tempi)  ), collapse = '')
#    if( length(heatmap.based.on.median.time) > 0 ) {
#       arr.numeri.colore <- heatmap.based.on.median.time
#       paletteColorNumber <- which(c(arr.numeri.colore,Inf) - median(tmp.tempi) >= 0)[1]
#       fillColor <-  paste(c(heatmap.base.color,paletteColorNumber),collapse = '')
#    }
# }
#



