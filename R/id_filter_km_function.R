#'@title KM function x CFM: creo una lista di id filter per stratificare i path
#'
#'@import survival
#'

# list.path<-data_reactive$paths
# path.plot<- data_reactive$paths.to.plot
#
# filter_id_km<-function(list.path,ObjDL){
#   id.list<-list()
#   ObjDL.out<-ObjDL$getData()
#   pat.process<-ObjDL.out$pat.process
#   for (i in c(1:length(list.path))) {
#     name<-paste0("path",i)
#     pat<-all.path[[name]]
#
#     if(pat$strat.var!=""){
#       if(pat$strat.var.type=="Categorical"){
#        tmp<-lapply(names(pat.process), function(id.paz){
#          if(pat.process[[id.paz]][,pat$strat.var][1]==pat$strat.value1){
#            to.keep<-id.paz
#          }
#        })
#       }else{
#         mediana<-median(as.numeric(all.data[[1]][,pat$strat.var]),na.rm = T)
#         tmp<-lapply(names(pat.process), function(id.paz){
#           if(pat.process[[id.paz]][,pat$strat.var][1]<=mediana){
#             to.keep<-id.paz
#           }
#         })
#
#       }
#     }
#
#     id.list[[name]]<-unlist(tmp)
#
#   }
#   return(id.list)
#
# }
