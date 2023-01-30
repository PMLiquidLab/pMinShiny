#'@title dict_mod: first grouping mod
#'
#'
#'@rawNamespace import(shiny, except = c(dataTableOutput,renderDataTable))
#'@import shinythemes
#'@import dplyr
#'@import shinyWidgets
#'@importFrom DT renderDataTable dataTableOutput
#'@impot  stringr
#'@import sortable


dict_mod_ui<- function(id,data){
  #flag=TRUE-->Merge Module, it is necessary to select a key for the uploaded data set
  #flag=FALSE-->Visualization module
  ns<-NS(id)
  sidebarLayout(
    sidebarPanel(
      textInput(ns("dict.name"), label = "Insert the name of the Dictionary"),
      numericInput(ns("n.group"),"Enter the number of new groups you want to create",
                   value=1,
                   min = 1,
                   max = length(unique(data[,4]))),

        uiOutput(ns("nomi")),

      fluidRow(
        column(6,
               actionButton(ns("save"), label = "Save the dictionary")
               ),
        column(6,
               # conditionalPanel("output.showpan=='yes'", ns=ns,
               #                  # actionButton(ns("show.graph"), "show CFM graph")
               #                  CFM_group_ui(ns("cfm.ex"))
               #
               # )
               # actionButton(ns("show.graph"), "show CFM graph")
               # CFM_group_ui(ns("cfm.ex"))
               )
      )
      # actionButton(ns("save"), label = "Save the dictionary"),
      #
      # CFM_group_ui(ns("cfm.ex"))
    ),
    mainPanel(
      uiOutput(ns("groups")),

    )
  )
}


dict_mod_server<-function(input,
                          output,
                          session,
                          data,
                          id,
                          pat,
                          ObjDL){
  ns <- session$ns

  data_reactive<-reactiveValues(
    n.gr=array()
  )

  observeEvent(input$n.group,{
    data_reactive$n.gr=input$n.group
  })

  observeEvent(input$dict.name,{
    data$name<-input$dict.name
    dict.names[[id]]<<-input$dict.name
  })

  gr.name<-reactive({
    tmp= lapply(1:data_reactive$n.gr, function(i) {
      if(is.null(input[[paste0("name.gr",i)]])){
        val=paste0("Group ", i)
      }else{
        val=input[[paste0("name.gr",i)]]
      }
      textInput(inputId = ns(paste0("name.gr", i)), label = paste("rename group", i),value=val)
    })
  })

  output$nomi<-renderUI({
    gr.name()
  })



  gr.list<-reactive({
    tmp<-lapply(1:length(unique(data[,4])), function(lista){
      if(length(unique(data[,4])[[lista]])>1){
        name<-str_c(unique(data[,4])[[lista]],collapse = " ")
      }else{
        name<-unique(data[,4])[[lista]]
      }
      if(name %in% input$Dynamic_Bucket[2:length(input$Dynamic_Bucket)]){
        fin.name<-NULL
      }else{
        fin.name<- name
      }
      return(fin.name)
    })
    return(tmp)
  })


  first.group<-reactive({
    tmp<-lapply(1:length(unique(data[,4])), function(lista){
      if(length(unique(data[,4])[[lista]])>1){
        name<-str_c(unique(data[,4])[[lista]],collapse = " ")
      }else{
        name<-unique(data[,4])[[lista]]
      }
      if(name %in% input$Dynamic_Bucket[2:length(input$Dynamic_Bucket)]){
        fin.name<-NULL
      }else{
        fin.name<- name
      }
      return(name)
      # return(fin.name)
    })

    rk1<-lapply(2:(input$n.group+1), function(x) {
      if(length(input$Dyanmic_Bucket)<x){
        lb<-NULL
      }else{
        lb<-input$Dyanmic_Bucket[[x]]
      }
      if(is.na(input[[paste0("name.gr",x-1)]]) | input[[paste0("name.gr",x-1)]]==""){
        name<-paste0("unkown group ", x-1)
      }else{
        name<-input[[paste0("name.gr",x-1)]]
      }
      add_rank_list(
        input_id =name,
        text = input[[paste0("name.gr",x-1)]],
        labels = unique(lb)
      )
    })
    return(rk1)
  })


  output$groups<-renderUI({
    fluidPage(
      fluidRow(
        do.call("bucket_list", args = c(
          list(header = "",
               group_name = ns("Dyanmic_Bucket"),
               orientation = "horizontal",
               add_rank_list(
                 text = "Events to group",
                 labels = gr.list(),
                 input_id = "rank_list_1"
               )
          ),

          first.group()
        )),
      ),

      fluidRow(
        plotOutput(ns("evt.dist")),
        DT::dataTableOutput(ns("bucket_outputs"))
      )
    )
  })



  observeEvent(input$save,{
  all.dict[[id]]<<-input$Dyanmic_Bucket
  names(all.dict)[which(names(all.dict)==id)]<- input$dict.name

  })


  df<-reactive({
    dic<-evt.tab(input$Dyanmic_Bucket,unique(data[,4]))
    return(dic)
  })



  dist<-reactive({
   pat.process<-pat
   df1<-evt.tab(input$Dyanmic_Bucket,unique(data[,4]))
   df<-applyDict(column.name="GROUP" ,
             dict.name = 'main',
             column.event.name= "EVENT",
             pat.process,
             param.EVENTName="EVENT",
             df1)[,2:6]
   plot_fin<-event_plot(df,unique(df[,4]),FALSE)
   return(plot_fin)
 })



  output$bucket_outputs <- DT::renderDataTable(df(), rownames = FALSE )
  output$evt.dist<-renderPlot(dist())
  output$prova1<- renderPrint({first.group()})


  # ################################ SHOW CFM BUTTON  ###########################
  #
  #
  #
  # conditionalPanel("output.showpan=='yes'", ns=ns,
  #                  # actionButton(ns("show.graph"), "show CFM graph")
  #                  CFM_group_ui(ns("cfm.ex"))
  #
  # )
  #
  # show.cfmgr = FALSE
  # data_reactive$show.cfmgr <- !(data_reactive$show.cfmgr)
  #
  # output$showpan <- renderText({
  #   if(data_reactive$show.cfmgr){
  #     "yes"
  #   } else{
  #     "no"
  #   }
  # })
  #
  # outputOptions(output, "showpan", suspendWhenHidden = FALSE)
  #
  #
  #
  #
  #
  # newEL<-reactive({
  #   pat.process<-pat
  #   df1<-evt.tab(input$Dyanmic_Bucket,unique(data[,4]))
  #   df<-applyDict(column.name="GROUP" ,
  #                 dict.name = 'main',
  #                 column.event.name= "EVENT",
  #                 pat.process,
  #                 param.EVENTName="EVENT",
  #                 df1)[,2:6]
  #   return(df)
  # })
  #
  # observeEvent(input$show.graph,{
  #     ObjDL<-dataLoader(verbose.mode = FALSE)
  #     ObjDL$load.data.frame(mydata =newEL() ,IDName = "ID",EVENTName = "EVENT",dateColumnName = "DATE_INI",
  #                           format.column.date = "%Y-%m-%d")
  #     ObjCFM<-careFlowMiner(verbose.mode = FALSE)
  #     ObjCFM$loadDataset(inputData = ObjDL$getData())
  #
  #     # cf.graph<-ObjCFM$plotCFGraph(depth = Inf,  #PROFONDITA
  #     #                              abs.threshold = 10, #support
  #     #                              kindOfGraph = "dot",
  #     #                              nodeShape = "square")$script
  #     # return(cf.graph())
  # })
  #
  #   callModule(CFM_group_server,
  #            "cfm.ex", newEL())
  #
  # ############################################################################


}







