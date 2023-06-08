#'@title Event distribution over patients
#'@description this function creates a bar plot that shows the distribution of events by patient,
#'showing how many patients shared a specific type of event.
#'
#'@import dplyr
#'@import ggplot2
#'


event_pat<-function(data,chosen,flag){
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


  ID<-data[,1]
  EVENT<-data[,4]
  data<-data.frame(ID,EVENT)
  gr<-data %>% group_by(data$ID,data$EVENT)
  sum<-gr %>% summarise(n=n())
  data_frame<-as.data.frame(sum)
  colnames(data_frame)<-c("ID","EVENT","n")
  count<-as.matrix(table(data_frame$EVENT))
  perc<-(count[,1]/length(unique(data$ID)))*100


  # count<-array()
  # perc<-array()
  #
  #
  # for (i in c(1:length(chosen)) ) {
  #   data_frame[data_frame$EVENT==chosen[i] & data_frame$n<2,]
  #   # count[i]<-dim(data_frame[data_frame$EVENT==chosen[i] & data_frame$n<2,])[1]
  #   perc[i]<-(count[i]/length(unique(data$ID)))*100
  # }
  # data_frame<-data.frame(chosen,count,perc)
  data_frame<-data.frame(rownames(count),count,perc)
  colnames(data_frame)<-c("events","count","perc")
  #flag==FALSE --> perc
  if(!flag){
    finalplot<-ggplot(data_frame, aes(x=events, y=perc, fill=events)) + geom_bar(stat = "identity",position="dodge",show.legend = FALSE)+
      scale_fill_manual(values = mycolors)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),
            legend.title =element_blank()) +
      xlab("")+
      ylab("%")
  }else{
    finalplot<-ggplot(data_frame, aes(x=events, y=count, fill=events)) + geom_bar(stat = "identity",position="dodge",show.legend = FALSE)+
      scale_fill_manual(values = mycolors)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),
            legend.title =element_blank()) +
      xlab("")+
      ylab("count")
  }

  return(finalplot)

}
