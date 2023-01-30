#'@title function for cov analysis
#'


#'@rawNamespace import(graphics, except = box)
#'@importFrom stats glm




plot_cov_graph<-function(df_tot,
                         UM="days",
                         plot.points=TRUE,
                         plot.RegressionLine=FALSE,
                         plot.ci.mean=FALSE,
                         alfa=0.05,
                         delta=10,
                         points.symbols=20,
                         size.symbols=1.5,
                         line.width=2,
                         y.int.legend=0.6,
                         legend.text.size=0.6,
                         legend.position='topleft'){

  ######################################### PLOT DEI DATI ####################################################################################################
  if( UM == "days") df_tot$time <- df_tot$time / 1440
  if( UM == "hours") df_tot$time <- df_tot$time / 60
  if( UM == "weeks") df_tot$time <- df_tot$time / (1440 * 7)
  if( UM == "months") df_tot$time <- df_tot$time / (43800)
  if( UM == "years") df_tot$time <- df_tot$time / (43800*12)



  #distinguiamo 2 casi: 1) GRAFICO ANDAMENTALE
  #                     2) GRAFICO PUNUALE


  #Seconda condizione dell'if è provvisoria e serve per gestire casi con logica non ancora inplementata

  if(!is.null(df_tot)){
    #CASO 1:GRAFICO ANDAMENTALE
    mycolors<-c("#DF060A","#88df45","#041CB3","#F8961E","#55E6D7",
                "#FFFF48","#9B21DD")
    path<-unique(df_tot[,c("nodo_IN","nodo_OUT")])
    # colore<-rainbow(n=nrow(path))
    colore <- mycolors[1:nrow(path)]
    arr.colore <- data.frame(cbind(colore,path["nodo_IN"], path["nodo_OUT"]))
    tipo_punti <- ifelse(plot.points, 'p', 'n')
    tipo_linea <- ifelse(plot.RegressionLine, 1, 0)
    tipo_ic <- ifelse(plot.RegressionLine, 2, 0)


    for (i in 1:nrow(path)){

      dati <- df_tot[which(df_tot$nodo_IN == path[i,"nodo_IN"] & df_tot$nodo_OUT == path[i,"nodo_OUT"]),]
      if(plot.RegressionLine){
        model <- glm(covariate ~ time, data = dati, family = 'gaussian')
      }
      if(!plot.ci.mean){
        y.lim=c(min(df_tot$covariate),max(df_tot$covariate))
      }else{
        d_mean<-ci.mean.fun(alfa,delta,dati)
        y.lim=c(min(min(d_mean$lower),min(df_tot$covariate)),max(max(df_tot$covariate),max(d_mean$upper)))
      }
      if(i == 1){
        plot(covariate ~ time,
             data = dati,
             type = tipo_punti,
             pch = points.symbols,
             cex = size.symbols,
             ylim=y.lim,

             xlim = c(0,max(df_tot$time)),
             # xlim = c(0,xlim),
             xlab = paste(UM,'from input node'),
             col = arr.colore[i,'colore'])
        if(plot.RegressionLine){
          if(length(which(is.na(model$coefficients)))==0){
            abline(model,
                   col = arr.colore[i,'colore'],
                   lwd = line.width,
                   lty = tipo_linea)
          }
        }
        if(plot.ci.mean){
          lines(x = d_mean$time,y = d_mean$mean,type = "l",xlab = "time",ylab = "mean", ylim = c(min(d_mean$lower),max(d_mean$upper)),col=arr.colore[i,'colore'])
          col<-col2rgb(arr.colore[i,'colore'], alpha = FALSE)/255
          graphics::polygon(x=c(d_mean$time,
                      rev(d_mean$time)),
                  y=c(d_mean$upper, rev(d_mean$lower)),
                  col=rgb(red =col[1,1],green =col[2,1] ,blue =col[3,1] ,alpha = 0.1
                  ),
                  density = 300, angle=90)
        }
      }else{
        points(covariate ~ time,
               data = dati,
               type = tipo_punti,
               pch = points.symbols,
               cex= size.symbols,
               col = arr.colore[i,'colore'] )
        if(plot.RegressionLine){
          if(length(which(is.na(model$coefficients)))==0){
            abline(model,
                   col = arr.colore[i,'colore'],
                   lwd = line.width,
                   lty = tipo_linea)
          }
        }
        if(plot.ci.mean){

          lines(x = d_mean$time,y = d_mean$mean,type = "l",xlab = "time",ylab = "mean", ylim = c(min(d_mean$lower),max(d_mean$upper)),col=arr.colore[i,'colore'])
          col<-col2rgb(arr.colore[i,'colore'], alpha = FALSE)/255
          polygon(x=c(d_mean$time,
                      rev(d_mean$time)),
                  y=c(d_mean$upper, rev(d_mean$lower)),
                  col=rgb(red =col[1,1],green =col[2,1] ,blue =col[3,1] ,alpha = 0.1
                  ),
                  density = 300, angle=90)
        }
      }


    }



    # arr.colore$nomi_nodi_in <- unlist(lapply(arr.colore$nodo_IN, function(x){paste(x, CFMstructure$lst.nodi[[x]]$evento, sep = '-')} ))
    # arr.colore$nomi_nodi_out <- unlist(lapply(arr.colore$nodo_OUT, function(x){paste(x, CFMstructure$lst.nodi[[x]]$evento, sep = '-')} ))
    arr.colore$Nome_coppie_nodi <- paste('From:', arr.colore$nodo_IN, 'to:', arr.colore$nodo_OUT)
    legend(legend.position,
           legend <- arr.colore$Nome_coppie_nodi,
           col=arr.colore[1:nrow(arr.colore),'colore'],
           lty = 1,
           lwd = 3,
           y.intersp =  y.int.legend,
           cex=legend.text.size,
           bty = 'o'
    )

  }else{
    return(NULL)

  }
}
