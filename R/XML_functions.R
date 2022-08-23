#'@title Fuction for write and read XML
#'
#'
#'@import XML



XML.write.fun<-function(id,key,cross,rule,crossEL,flag){
  prefix.xml <- "<file><key><var/></key><crossvars/><longvar/></file>"

  # BUILD XML TREE
  doc = xmlTreeParse(prefix.xml, useInternalNodes = T)     # PARSE STRING

  #ROOT
  root = xmlRoot(doc)                                    # <file></file>

  #aDD KEY
  node1<-root[["key"]][["var"]]
  newN<-newXMLNode("name", key, parent=node1)


  #aDD CROSS VARS
  node2<-root[["crossvars"]]


  if(is.null(cross)){
    newN<-newXMLNode("var", parent=node2)
    newN<-newXMLNode("name","none",parent=newN)
  }else{
    for (i in seq_along(cross)) {
      newN<-newXMLNode("var", parent=node2)
      newN<-newXMLNode("name",cross[i],parent=newN)
    }
  }


  #ADD LONGVAR
  node3<-root[["longvar"]]

  #case: BY ATTRIBUTE
  if(flag){
    if(is.na(names(rule)[1])){
      newN<-newXMLNode("var",parent = node3)
      intN<-newXMLNode("name_long","NA",parent = newN)
      intN<-newXMLNode("time_ind","NA",parent = newN)
      intN<-newXMLNode("name","NA",parent = newN)
      intN<-newXMLNode("delta_b","NA",parent = newN)
      intN<-newXMLNode("delta_a","NA",parent = newN)
      intN<-newXMLNode("rule","NA",parent = newN)
      intN<-newXMLNode("value","NA",parent = newN)
    }else{
      for (i in seq_along(rule)) {
        newN<-newXMLNode("var",parent = node3)
        nome<-names(rule)[i]
        intN<-newXMLNode("name_long",rule[[nome]][1],parent = newN)
        intN<-newXMLNode("time_ind",rule[[nome]][2],parent = newN)
        intN<-newXMLNode("name",nome,parent = newN)
        intN<-newXMLNode("delta_b",rule[[nome]][3],parent = newN)
        intN<-newXMLNode("delta_a",rule[[nome]][4],parent = newN)
        intN<-newXMLNode("rule",rule[[nome]][5],parent = newN)
        intN<-newXMLNode("value",rule[[nome]][6],parent = newN)
      }
    }


  }else{
    #case: BY EVENT
    for (i in seq_along(rule)) {
      newN<-newXMLNode("var",parent = node3)
      intN<-newXMLNode("data_ini",rule[[i]][1],parent = newN)
      intN<-newXMLNode("data_end",rule[[i]][2],parent = newN)
      intN<-newXMLNode("event",attrs = c(name=rule[[i]][4]),rule[[i]][3],parent = newN) #aggiunta att x sett event name-->name="NA": non c'è nuovo nome
    }

    #aggiungo le CROSSVAREL
    newNCROSS<-newXMLNode("crossvars",parent = node3)
    for (i in seq_along(crossEL)) {
      newN<-newXMLNode("var", parent=newNCROSS)
      newN<-newXMLNode("name",crossEL[i],parent=newN)
    }


  }

  # filename<-paste0("MergeFile",as.character(id),".xml")
  # fileOUT<-saveXML(doc,file.path(path,filename))
  all.xml[[id]]<<-doc
}





XML.read.fun<-function(xml.el){
  # res <- xmlParse(file = filePath)
  res<-xml.el
  #lettura
  rootnode <- xmlRoot(res)
  #dal nodo "key" ho bisogno del nome:
  key_name<-xmlValue(rootnode[["key"]])

  #cross
  cross_vars<-array()
  for (i in c(1:xmlSize(rootnode[["crossvars"]]))){
    cross_vars[i]<-xmlValue(rootnode[["crossvars"]][i][["var"]])
  }

  #long
  rules<-list()
  nomi<-array()
  n_nodi<-xmlSize(rootnode[["longvar"]])

  if(xmlSize(rootnode[["longvar"]][1][["var"]])>3){
    if(xmlValue(rootnode[["longvar"]][1][["var"]])=="NA"){
      rules[[1]]<-"NA"
    }else{
      for (i in c(1:(n_nodi))) {
        rules[[i]]<-c(xmlValue(rootnode[["longvar"]][i][["var"]][["time_ind"]]),
                      xmlValue(rootnode[["longvar"]][i][["var"]][["name_long"]]),
                      xmlValue(rootnode[["longvar"]][i][["var"]][["delta_b"]]),
                      xmlValue(rootnode[["longvar"]][i][["var"]][["delta_a"]]),
                      xmlValue(rootnode[["longvar"]][i][["var"]][["rule"]]),
                      xmlValue(rootnode[["longvar"]][i][["var"]][["value"]])
        )
      }
      for(i in c(1:(n_nodi))){
        nomi[i]<-xmlValue(rootnode[["longvar"]][i][["var"]][["name"]])
      }
      names(rules)<-nomi
    }
  }else{
    for (i in c(1:(n_nodi-1))) {
      rules[[i]]<-c(xmlValue(rootnode[["longvar"]][i][["var"]][["data_ini"]]),
                    xmlValue(rootnode[["longvar"]][i][["var"]][["data_end"]]),
                    xmlValue(rootnode[["longvar"]][i][["var"]][["event"]])
      )
      names(rules[[i]])<-c("DATA_INI","DATA_END",xmlAttrs(rootnode[["longvar"]][["var"]][["event"]]))
    }
  }





  #crossEL
  crossELnode<-rootnode[["longvar"]][n_nodi]
  cross_varsEL<-array()
  for (i in c(1:xmlSize(crossELnode[["crossvars"]]))){
    cross_varsEL[i]<-xmlValue(crossELnode[["crossvars"]][i][["var"]])
  }

  outvar<-list(key_name,cross_vars,rules,cross_varsEL)
  names(outvar)<-c("key","crossvar","rules","crossvarEL")
  return(outvar)
}



