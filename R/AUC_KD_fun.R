#'@importFrom stats density
#'@rawNamespace import(graphics, except = box)

AUC_KD_fun<-function(lst.arr.AUC,ind.fold=c(),all.fold=FALSE){
  if(all.fold){
    dens <-lapply(lst.arr.AUC, function(lst.AUC){
      arr.AUC<-unlist(lst.AUC)
      d<-density(arr.AUC)
      fun.app<-approxfun(d)
      x.val<-sort(seq(0,1,0.01))
      # x.val<-sort(runif(50,0,1))
      y.val<-fun.app(x.val)

      return(list("x.val"=x.val,
                  "y.val"=y.val))
      # return(d)
    })

    y.bound<-lapply(dens,function(dens.i){return(c(min(dens.i$y.val,na.rm = TRUE),max(dens.i$y.val,na.rm = TRUE)))})
    x.bound<-lapply(dens,function(dens.i){return(c(min(dens.i$x.val[which(!is.na(dens.i$y.val))],na.rm = TRUE),max(dens.i$x.val[which(!is.na(dens.i$y.val))],na.rm = TRUE)))})
    #aggiungi controllo con y != NA
    #abline su 0.5
    lower.y<-min(unlist(lapply(y.bound, function(y){return(y[1])})))
    upper.y<-max(unlist(lapply(y.bound, function(y){return(y[2])})))
    lower.x<-min(unlist(lapply(x.bound, function(x){return(x[1])})))
    upper.x<-max(unlist(lapply(x.bound, function(x){return(x[2])})))


    # plot(NA, xlim=range(sapply(dens, "[", "x")), ylim=range(sapply(dens, "[", "y")))
    plot(NA, xlim=c(lower.x,upper.x), ylim=c(lower.y,upper.y),xlab="sampled AUC from uniform dist", ylab="density")
    for(i in c(1:length(dens))){
      lines(dens[[i]]$x.val,dens[[i]]$y.val,col=i, lty=1,lwd=2)
    }


    legend("topright", legend=paste("model cov row", seq_along(1:length(dens))), fill=1:length(dens))
  }else{
    lst.AUC<-lst.arr.AUC[[ind.fold]]
    arr.AUC<-unlist(lst.AUC)
    d<-density(arr.AUC)
    plot(d)
  }

  # lst.AUC<-lst.arr.AUC[[ind.fold]]
  # arr.AUC<-unlist(lst.AUC)
  # d<-density(arr.AUC)
  # plot(d)

#
#   dens <-lapply(lst.arr.AUC, function(lst.AUC){
#     arr.AUC<-unlist(lst.AUC)
#     d<-density(arr.AUC)
#
#     return(d)
#   })
#
#   plot(NA, xlim=range(sapply(dens, "[", "x")), ylim=range(sapply(dens, "[", "y")))
#   mapply(lines, dens, col=1:length(dens),lty=1, lwd=2)
#
#   legend("topright", legend=names(dens), fill=1:length(dens))

}



