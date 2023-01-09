#'@title LOGRANK TEST X KM
#'
#'@import survival
#'@import utils
#'
#'

#out.fun<-output of render.km.graph

logrank_fun<-function(out.fun,lst.path){
  df.out<-data.frame()
  if(!is.null(out.fun$id.not.valid)){

    not.to.check<-which(names(lst.path) %in% paste0("path",out.fun$id.not.valid)==TRUE)
    to.combine<-names(lst.path)[-not.to.check]
  }else{
    to.combine<-names(lst.path)
  }

  not.to.check<-which(names(lst.path) %in% paste0("path",out.fun$id.not.valid)==TRUE)


  if(length(out.fun)==3 & length(to.combine)>1){
    coppie<-t(combn(to.combine,2))
    for (i in c(1:nrow(coppie))) {
      res<-survdiff(Surv(time, outcome) ~ path,
                    data=out.fun$final.data[out.fun$final.data$path %in% coppie[i,],],
                    rho = 0)
      #res$chisq= valore della statistica del chi2
      #calcolo p value come 1-(area sotto la curva di distribuzione chi2 con n-1 gdl)

      p.val <- 1 - pchisq(res$chisq, length(res$n) - 1)
      res.to.save<-c(paste0(coppie[i,],collapse = "-"),p.val)
      df.out<-rbind(df.out,res.to.save)
    }

    colnames(df.out)<-c("Groups","p-value")

  }
  return(df.out)
}
