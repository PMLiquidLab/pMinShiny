#'@title dict_visual_mod: first grouping mod
#'
#'
#'@import shiny
#'@import shinythemes
#'@import dplyr
#'@import shinyWidgets
#'@import DT
#'@import shinydashboard




group_visual_mod<- function(id){
  #flag=TRUE-->Merge Module, it is necessary to select a key for the uploaded data set
  #flag=FALSE-->Visualization module
  ns<-NS(id)
  fluidPage(
    fluidRow(
      uiOutput(ns("visual"))
      # sidebarLayout(
      #   sidebarPanel(
      #     selectInput(ns("show.visual"),label = " ",
      #                 choices = c("Group Table","Bar Plot","New EventLog"),
      #                 selected = "Group Table")),
      #   mainPanel(
      #     uiOutput(ns("visual"))
      #   )
      # )
    )
  )

}

group_visual_server<-function(input,
                          output,
                          session,
                          data,
                          id,
                          pat,
                          type.of.visual){
  ns <- session$ns


  #TAB OUTPUT
  tab<-reactive({
    dict<-all.dict[[id]]
    df<-evt.tab(dict,unique(data[,4]))
    return(df)
  })


  #DIST OUTPUT
  dist<-reactive({
    # pat.process<-pat
    # dict<-all.dict[[id]]
    # df1<-evt.tab(dict,unique(data[,4]))
    # df<-applyDict(column.name="GROUP" ,
    #               dict.name = 'main',
    #               column.event.name= "EVENT",
    #               pat.process,
    #               param.EVENTName="EVENT",
    #               df1)[,2:6]
    plot_fin<-event_plot(data,unique(data[,4]),FALSE)
    return(plot_fin)
  })

  dist.new<-reactive({
    data_plot<-select(data,4)
    data_plot[,1]<-as.factor(data_plot[,1])
    data_plot<-subset(data_plot,data_plot[,1] %in% unique(data[,4]))
    name<-unique(data_plot[,])
    tot<-length(data_plot[,1])
    x1<-data_plot[,1]
    perc=vector(length = length(name))
    con=c(1:length(name))

    for (i in con) {
      perc[i]=(length(which(x1==name[i]))/tot)*100
    }


    datatoplot=data.frame(name,perc)
    # finalplot<-ggplot(datatoplot, aes(x=name, y=perc, fill=name)) + geom_bar(stat = "identity",show.legend = FALSE)+
    #   scale_fill_manual(values = mycolors)+
    #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),
    #         legend.title =element_blank()) +
    #   xlab("")+
    #   ylab("%")





    df<-tab()
    df[,3]<-datatoplot[datatoplot$name==df[,1],2]
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

    finalplot<-ggplot(df, aes(x=GROUP, y=perc, fill=EVENT)) + geom_bar(stat = "identity", show.legend = TRUE)+
      scale_fill_manual(values = mycolors)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=12),
            legend.title =element_blank()) +
      xlab("")+
      ylab("%")
    # plot_fin<-event_plot(data,unique(data[,4]),FALSE)
    return(finalplot)
  })


  #EL OUTPUT
  EL<-reactive({
    pat.process<-pat
    df1<-evt.tab(all.dict[[id]],unique(data[,4]))
    df<-applyDict(column.name="GROUP" ,
                  dict.name = 'main',
                  column.event.name= "EVENT",
                  pat.process,
                  param.EVENTName="EVENT",
                  df1)[,2:6]
    return(df)
  })


  output$visual<-renderUI({
    fluidPage(
      fluidRow(
        if(type.of.visual=="Group Table"){
          DT::dataTableOutput(ns("group.tab"))
        }else {
          fluidRow(
            shinydashboard::box(title = "Event distribution in EL",
                                background = "blue",
                                width = 5,
                                plotOutput(ns("bar.plot"))),

            shinydashboard::box(title = "New Event distribution",
                                background = "blue",
                                width = 7,
                                plotOutput(ns("bar.plot.new")))
          )


          # plotOutput(ns("bar.plot"))
        }
      ),
      fluidRow(
        DT::dataTableOutput(ns("EL.tab"))
      ),
      fluidRow(
        downloadButton(ns("downloadData"), "Download")
      )
    )
  })

  output$group.tab<- DT::renderDataTable(tab(),rownames = FALSE)
  output$bar.plot<- renderPlot(dist())
  output$bar.plot.new<- renderPlot(dist.new())
  output$EL.tab<- DT::renderDataTable(EL(),rownames = FALSE)
  output$downloadData <- downloadHandler(

    filename = function() {
      paste("groupedEL", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(EL(), file, row.names = FALSE)
    }
  )




}


