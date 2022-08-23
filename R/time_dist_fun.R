#'@title Event distribution over time
#'@description this function creates a bar plot The bar plot that shows the distribution
#'of the  events in different time.the time axis has been discretized using quarters,
#'in order to highlight how the distribution of each event changes over time. it is also possible to
#'set a time window in order to analize event distribution in a smaller temporal range.
#'
#'@import lubridate
#'@import dplyr
#'@import ggplot2
#'




# library(RColorBrewer)
# library(dplyr)
# library(lubridate)

time_dist<-function(data,chosen,date_range){

  mycolors<-c("#E41A1C",


              "#AB3A4E",
              "#8F4A68",
              "#419584",
              "#566B9B",
              "#FF8904",
              "#D06C78",
              "#3D8D96",

              "#449D72",
              "#48A460",
              "#4CAD4E",
              "#CDA12C",
              "#C4625D",
              "#629363",
              "#FFB314" ,
              "#C72A35",
              "#87638F",
              "#93539D",
              "#A25392",
              "#F9F432",
              "#3A7BB4",
              "#B35A77",
              "#EBD930",
              "#D46A42",
              "#E57227",
              "#F67A0D",

              "#FF9E0C",
              "#6E8371",
              "#FFC81D",

              "#FFF12D",
              "#7A7380",
              "#735B81",

              "#DCBD2E",
              "#56A354",
              "#BF862B",
              "#B06A29",
              "#A9572E",
              "#B65E46",
              "#C3655F",
              "#3A85A8",
              "#DE7390",
              "#EB7AA9",
              "#F581BE",

              "#D689B1",
              "#C78DAB",
              "#B791A5",
              "#E585B8",
              "#A8959F",
              "#FFDD25",
              "#999999")


  date_var<-as.Date(select(data,2)[,1])
  # date_var<-date_var[date_var<=date_range[2] & date_var>date_range[1]]
  # date_var<-as.factor(quarter(date_var,with_year = TRUE, fiscal_start = 1))
  events<-as.factor(select(data,4)[,1])
  new_data<-data.frame(date_var,events)
  # new_data<-subset(new_data, events %in% chosen)
  final_data<-filter(new_data, (date_var<=date_range[2] & date_var>date_range[1]) & events %in% chosen)
  final_data$date_var<-as.factor(quarter(final_data$date_var,with_year = TRUE, fiscal_start = 1))

  groups<-final_data %>% group_by(date_var,events)
  sum<- groups %>% summarise(n=n())
  finalplot<-ggplot(sum, aes(x=date_var, y=n, fill=events)) + geom_bar(stat = "identity",position="dodge")+
    scale_fill_manual(values = mycolors)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),
          legend.title =element_blank()) +
    xlab("Year Quarters")+
    ylab("count")

  return(finalplot)
}
