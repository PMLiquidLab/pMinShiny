#'@title Event distribution
#'@description this function creates a bar plot that shows the distribution of events.
#'
#'@import dplyr
#'@import ggplot2
#'


#function for event distribution plot
event_plot<-function(data,chosen,flag){
  # nb.cols <- length(chosen)                                     #num colori necessari
  mycolors<-c("#E41A1C","#C72A35", "#AB3A4E",
              "#8F4A68", "#735B81" ,"#566B9B",
              "#3A7BB4", "#3A85A8" ,"#3D8D96",
              "#419584", "#449D72", "#48A460",
              "#4CAD4E", "#56A354" ,"#629363",
              "#6E8371" ,"#7A7380", "#87638F",
              "#93539D" ,"#A25392" ,"#B35A77",
              "#C4625D" ,"#D46A42", "#E57227",
              "#F67A0D", "#FF8904","#FF9E0C",
              "#FFB314" ,"#FFC81D", "#FFDD25",
              "#FFF12D", "#F9F432", "#EBD930",
              "#DCBD2E", "#CDA12C" ,"#BF862B",
              "#B06A29" ,"#A9572E", "#B65E46",
              "#C3655F" ,"#D06C78", "#DE7390",
              "#EB7AA9", "#F581BE" ,"#E585B8",
              "#D689B1", "#C78DAB", "#B791A5",
              "#A8959F", "#999999")

  if(is.null(chosen)){
    chosen=""
  }
  data_plot<-select(data,4)
  data_plot[,1]<-as.factor(data_plot[,1])

  data_plot<-subset(data_plot,data_plot[,1] %in% chosen)
  name<-unique(data_plot[,])
  tot<-length(data_plot[,1])
  x1<-data_plot[,1]
  perc=vector(length = length(name))
  con=c(1:length(name))

  for (i in con) {
    perc[i]=(length(which(x1==name[i]))/tot)*100
  }

  #if flag==TRUE yaxis with count
  if(flag){
    groups<-data_plot %>% group_by(data_plot[,1])
    sum<- groups %>% summarise(n=n())
    finalplot<-ggplot(sum, aes(x=`data_plot[, 1]`, y=n, fill=`data_plot[, 1]`)) + geom_bar(stat = "identity", show.legend = FALSE)+
      scale_fill_manual(values = mycolors)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),
            legend.title =element_blank()) +
      xlab("")+
      ylab("event count")
  }else{
    #flag==FALSE yaxis %
    datatoplot=data.frame(name,perc)
    finalplot<-ggplot(datatoplot, aes(x=name, y=perc, fill=name)) + geom_bar(stat = "identity",show.legend = FALSE)+
      scale_fill_manual(values = mycolors)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),
            legend.title =element_blank()) +
      xlab("")+
      ylab("%")
  }
  return(finalplot)
}




