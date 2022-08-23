#'@title Co-occurrence matrix
#'@description this function creates a matrix that shows the co-coccurrence of the selected events,
#'calculated as the number of times (among the total) each event occurs in the presence of another,
#'within the clinical path of the same patient
#'
#'@import pMineR
#'@import dplyr
#'

# library(pMineR)
# library(dplyr)

heatmap<-function(data,chosen,objDL.new){
  objDL.new$applyFilter(array.events.to.keep = chosen)
  objDL.new.export <- objDL.new$getData()
  arr.eventi <- objDL.new.export$arrayAssociativo[!(objDL.new.export$arrayAssociativo %in% c("BEGIN","END"))]
  MM.Cross <- matrix( 0,nrow = length(arr.eventi), ncol = length(arr.eventi) )
  colnames(MM.Cross) <- arr.eventi; rownames(MM.Cross) <- arr.eventi;
  tmp.1 <- lapply( rownames(MM.Cross) , function(event.C) {
    tmp.2 <- lapply(colnames(MM.Cross), function(event.R) {
      tmp.3 <- lapply( names(objDL.new.export$pat.process) , function(patID) {
        arr.evt.to.chech <- objDL.new.export$pat.process[[patID]][[objDL.new.export$csv.EVENTName]]
        if( event.C %in% arr.evt.to.chech & event.R %in% arr.evt.to.chech) {
          MM.Cross[ event.R , event.C ] <<- MM.Cross[ event.R , event.C ] + 1
        }
      })
    } )
  })

  tmp.1 <- lapply( 1:nrow(MM.Cross) , function(riga) {
    MM.Cross[riga,] <<- MM.Cross[riga,] / MM.Cross[riga,riga]
  })

  cex <- 1.3
  threshold.low <- 0
  threshold.hi <- 1
  show.diagonal <- TRUE
  par(mar = c(5, 10, 10, 2))
  image(t(MM.Cross[nrow(MM.Cross):1,]),col=heat.colors(256)[256:1] , axes=FALSE )
  arr.posizioni <- (0.1:ncol(MM.Cross)/(ncol(MM.Cross)-1))
  axis(2, arr.posizioni, labels=rownames(MM.Cross)[length(rownames(MM.Cross)):1],las=2)
  axis(3, arr.posizioni, labels=rownames(MM.Cross),las=2)
  for( riga in 1:nrow(MM.Cross)) {
    for( colonna in 1:ncol(MM.Cross)) {
      valore <- t(MM.Cross[ncol(MM.Cross)-colonna+1,riga])
      if( valore >= threshold.low & valore <= threshold.hi ) {
        text((riga-1)/(nrow(MM.Cross)-1),(colonna-1)/(ncol(MM.Cross)-1),format(valore,digits = 2) , cex=cex )
      }
    }
  }

}
