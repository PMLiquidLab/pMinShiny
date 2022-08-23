#'@title apply dict function
#'
#'


applyDict<-function(  column.name="GROUP" ,
                      dict.name = 'main',
                      column.event.name= "EVENT",
                      pat.process,
                      param.EVENTName="EVENT",
                      df.groups
                      ) {

  new.myData<-c()
  list.dict.column.event.name<-list()
  list.dict.column.event.name[[dict.name]]<-column.event.name

  list.dictionary<-list()
  list.dictionary[[ dict.name ]]<-df.groups

  #NEED PAT.PROCESS
  for(idPaz in names(pat.process)) {
    matrice<-pat.process[[idPaz]]
    names(matrice)<-names(pat.process[[idPaz]])

    aaa<-as.character(pat.process[[idPaz]][[param.EVENTName]])

    bbb<-unlist(lapply(aaa, function(x) {
      # prendi la voce corrispondente al nome dell'evento
      column.event.name<-list.dict.column.event.name[[ dict.name ]]
      arrPosizioniTMP<-which(list.dictionary[[ dict.name ]][[ column.event.name ]]==x )
      if(length(arrPosizioniTMP)>1) stop("Error! an Event is associated to more possible new Event names!")
      # e sostituisci

      if(length(arrPosizioniTMP)==0) return( "" )
      else return(as.character( list.dictionary[[ dict.name ]][[ column.name ]][arrPosizioniTMP])  )
    }  ))

    matrice[[param.EVENTName]] <- bbb
    matrice <- matrice[  which(matrice[[param.EVENTName]]!="") ,   ]

    new.myData <- rbind(new.myData,matrice)
  }

  return(new.myData)
}



