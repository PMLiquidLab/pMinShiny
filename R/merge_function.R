#'@title Merging core function
#'
#'
#'@import rlang
#'@import dplyr


#library(dbplyr)
#library(rlang)


merge_fun<-function(){

    EventLog<-all.data[["EventLog"]]
  data<-all.data[!names(all.data)=="EventLog"]
  rules<-lapply(all.xml, XML.read.fun)


  newE<-EventLog
  newE<-newE %>% select("ID","DATE_INI","DATE_END","EVENT")

  #primo step: aggiungo tutte le var crossectional
  for (i in c(1:length(all.xml))) {
    if(!rules[[i]]$crossvar[1]=="none" & !is.na(rules[[i]]$crossvar[1]) ){
      newE<-merge(newE,unique(subset(data[[i]],select = c(rules[[i]]$key,rules[[i]]$crossvar))),
                  by.x = "ID",by.y = rules[[i]]$key, all.x = TRUE)
    }
  }

  #secondo step:aggiungo variabili temporali
  #2.1 controllo se ho delle variabili temporali da aggiungere:
  #per verificare se ho var temporali controllo: "none" %in% rules[[i]]$rules[[1]]-->per merge by event      in questi casi
  #                                               "NA" %in% rules[[i]]$rules[[1]]--->per merge by att        mi fermo (avr? solo aggiunto le crossectional)
  #
  #2.2 capisco se fare merging by event o by attribute: lo faccio sulla base della lunghezza di rules[[i]]$rules[1]:-se =3 --> BY EVENT
  #                                                                                                                 -se =4 --> BY ATTRIBUTE
  event<-array()
  att<-array()
  for (i in c(1:length(all.xml))) {
    if(length(rules[[i]]$rules[[1]])>3 & !("NA" %in% rules[[i]]$rules[[1]]) ){
      att<-as.numeric(na.omit(append(att,i)))
    }else if(length(rules[[i]]$rules[[1]])<=3
             & !("none" %in% rules[[i]]$rules[[1]])
             & !("" %in% rules[[i]]$rules[[1]])
    ){
      event<-as.numeric(na.omit(append(event,i)))
    }
  }

  if(!is.na(event)){
    for (i in event) {
      if("none" %in% rules[[i]]$crossvarEL){
        #CASO IN CUI NON DEVO AGGIUNGERE VARIABILI CROSS SPECIFICHE AL NUOVO EVENTO
        # sel.col<-append(rules[[i]]$key,rules[[i]]$rules[[1]])[!append(rules[[i]]$key,rules[[i]]$rules[[1]]) %in% c("")]
        df<-subset(data[[i]], select = append(rules[[i]]$key,rules[[i]]$rules[[1]]))

        colnames(df)<-colnames(newE)[1:4]
        df<-df[which(!is.na(df$DATE_INI)),]

      }else{
        #CASO IN CUI AGGIUNGO VARIABILI SPECIFICHE
        df<-subset(data[[i]], select = c(rules[[i]]$key,rules[[i]]$rules[[1]], rules[[i]]$crossvarEL))
        colnames(df)<-colnames(newE)[1:4]
        df<-df[which(!is.na(df$DATE_INI)),]
        colnames(df)[5:length(colnames(df))]<-rules[[i]]$crossvarEL
        newE[rules[[i]]$crossvarEL]<-array(dim = c(nrow(newE),1))
      }
      if(!names(rules[[i]]$rules[[1]])[3]=="NA"){
        df[,4]<-names(rules[[i]]$rules[[1]])[3]
      }

      for (i in c(1:length(all.xml))) {
        if(!rules[[i]]$crossvar[1]=="none"){
          df<-merge(df,unique(subset(data[[i]],select = c(rules[[i]]$key,rules[[i]]$crossvar))),
                    by.x = "ID",by.y = rules[[i]]$key, all.x = TRUE)
        }
      }
      if(length(colnames(newE))==length(colnames(df))){
        newE<-rbind(newE,df)
      }else{
        df[colnames(newE)[!(colnames(newE) %in% colnames(df))]]<-array(dim =  c(nrow(df),1))
        newE<-rbind(newE,df)
      }
    }
  }

  if(!is.na(att)){
    for (i in att) {
      #fare a monte check value_ind=null
      #fissi per tutte le variabili temp
      value_ind<-rules[[i]]$rules[[1]][6]
      time_ind<-rules[[i]]$rules[[1]][1]
      var<-rules[[i]]$rules[[1]][2]
      tmpEL<-newE %>% mutate("DATE_INI"=as.Date(newE$DATE_INI),
                             "DATE_END"=as.Date(newE$DATE_END)
      )
      tmpEL<-tmpEL[order(tmpEL[,"ID"],tmpEL[,"DATE_INI"]),]
      tmp.data<-data[[i]]
      tmp.data[,time_ind]<-as.Date(tmp.data[,time_ind])


      #ciclo sulle variabili temporali
      new.column<-sapply(names(rules[[i]]$rules), function(var.name){
        incProgress(
          amount = 1 / length(names(rules[[i]]$rules))
        )

        if(rules[[i]]$rules[[var.name]][4]=="to the next event"){
          for (k in c(1:nrow(tmpEL)-1)) {
            tmpEL[k,"DATE_END"]<-tmpEL[k+1,"DATE_INI"]
          }
        }else{
          tmpEL<- tmpEL %>% mutate("DATE_INI"=tmpEL$DATE_INI-as.numeric(rules[[i]]$rules[[var.name]][3]),
                                   "DATE_END"=tmpEL$DATE_END-as.numeric(rules[[i]]$rules[[var.name]][4]),
          )
        }

        rule<-rules[[i]]$rules[[var.name]][5]

        #ciclo sugli id
        tmp.add.var<-sapply(unique(newE$ID), function(id){

          add.var<-array()
          paz_path.data<-tmp.data[tmp.data[rules[[i]]$key] == id,]
          paz_path.data<-paz_path.data[order(paz_path.data[,time_ind]),]
          paz_path.el<-tmpEL[tmpEL$ID==id,]
          paz_path.el<-paz_path.el[order(paz_path.el[,"DATE_INI"]),]

          #ciclo sugli eventi dell'paziente con il dato id

          new.val<-sapply(1:nrow(paz_path.el),function(riga){
            sub_path<-paz_path.data %>%
              filter(paz_path.data[,var]==var.name &
                       (paz_path.data[,time_ind]>=paz_path.el[riga,"DATE_INI"] & paz_path.data[,time_ind]<=paz_path.el[riga,"DATE_END"])
              )
            #prelevo valori
            if(value_ind=="none"){
              val<-array()
            }else{
              val<-sub_path[,value_ind]
            }
            if(is_empty(val)){
              val<-NULL
            }else{
              # c("mean","min value","max value","last measure","first measure","count","trend")
              switch (rule,
                      "mean" = {
                        val<-mean(val)
                      },
                      "max value"={
                        val<-max(val)
                      },
                      "min value"={
                        val<-min(val)
                      },
                      "last measure"={
                        val<-val[length(val)]
                      },
                      "first measure"={
                        val<-val[1]
                      },
                      "count"={
                        val<-nrow(sub_path)
                      },
                      "trend"={
                        if(nrow(sub_path>2)){
                          sub_path$day.numeric = as.numeric(sub_path$day)/(24*60*60)
                          q <- lm(day.numeric ~ value, data=sub_path)
                          val<-q$coefficients [2]
                        }else{
                          val<-NULL
                        }
                      }
              )

            }

            return(val)
          })
          #
          # time_end<-Sys.time()
          # print(time_end-time_ini)
          new.val1<-lapply(new.val, function(x) if(is.null(x)) NA else x)
          return(unlist(new.val1,use.names = FALSE))

        })

        tmp.add.var1<-lapply(tmp.add.var, function(x) if(is.null(x)) NA else x)
        return(unlist(tmp.add.var1,use.names = FALSE))


      })

      newE<-cbind(newE,new.column)

    }


  }

  return(newE)
}
