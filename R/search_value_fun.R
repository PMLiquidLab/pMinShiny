
#'@title Searchvalue fun

search_value<-function(sub.path,eventoDiPartenza,arr.attributi,type="att"){
  if(type=="att"){
    riga<-which(sub.path$EVENT==eventoDiPartenza)
    values<-sub.path[riga,arr.attributi]
  }else{
    values<-rep.int(NA,times = length(arr.attributi))
  }
  return(values)
}
