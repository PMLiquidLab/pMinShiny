#' @import shiny
#'



CFM_group_ui <- function(id){
  ns <- NS(id)

  tagList(

    conditionalPanel(condition = 'output.showpanel == "yes"', ns = ns ,
                     absolutePanel(
                       # top = 20,
                       left = 50,
                       right = 50,
                       bottom = 500,
                       width = 900, height = 100,
                       draggable = TRUE,
                       style = "opacity: 1; z-index: 10;" ,
                       wellPanel(style = "overflow-y:scroll;
                                          background: #F5F5F5;
                                          max-height: 800px; height: 900px",
                                 fluidPage(
                                   fluidRow(
                                     column(11,
                                            titlePanel("Care Flow Graph"),
                                     ),
                                     column(1,
                                            br(),
                                            actionButton(ns("close"), label= "",icon = icon("fas fa-window-close"))
                                     ),
                                   ),
                                   fluidRow(
                                     column(12,
                                            tags$hr()
                                     )
                                   ),
                                   fluidRow(
                                     column(12,
                                            sidebarLayout(
                                              sidebarPanel(
                                                selectInput(ns("dict"), "Select dictionary",
                                                            choices = NULL

                                                )
                                              ),
                                              mainPanel(
                                                DT::dataTableOutput(ns("CareFlowGraph"))
                                              )
                                            )

                                            # grVizOutput(ns("CareFlowGraph"))
                                            )
                                   )

                                 )



                       )

                     )
    ),

    absolutePanel(
      # bottom = 35,
      # # right = 25,
      # width = 15,
      # height = 15,
      actionButton(ns("show.graph"), "show CFM graph"))

  )}




CFM_group_server <- function(input,
                             output,
                             session,
                             pat,
                             dict
                             ){





  rv.CFMgr <- reactiveValues(show.panel = FALSE)

  observeEvent(input$show.graph, ({
    rv.CFMgr$show.panel <- !(rv.CFMgr$show.panel)
    updateSelectInput(inputId = "dict", "Select dictionary",
                      choices = names(dict),
                      session = session

    )
  }))

  observeEvent(input$close,{
    rv.CFMgr$show.panel <- !(rv.CFMgr$show.panel)
  })

  output$showpanel <- renderText({
    if(rv.CFMgr$show.panel){
      "yes"
    } else{
      "hidded"
    }
  })

  outputOptions(output, "showpanel", suspendWhenHidden = FALSE)



  # newEL<-reactive({
  #   pat.process<-pat
  #   df1<-evt.tab(all.dict[[input$dict]],unique(data[,4]))
  #   df<-applyDict(column.name="GROUP" ,
  #                 dict.name = 'main',
  #                 column.event.name= "EVENT",
  #                 pat.process,
  #                 param.EVENTName="EVENT",
  #                 df1)[,2:6]
  #   return(df)
  # })





  # output$CareFlowGraph<-renderGrViz({
  #   grViz(CFgraph())
  # })

  # CFgraph<-reactive({
  #   df1<-evt.tab(all.dict[[id]],unique(data[,4]))
  #   df<-applyDict(column.name="GROUP" ,
  #                 dict.name = 'main',
  #                 column.event.name= "EVENT",
  #                 pat.process,
  #                 param.EVENTName="EVENT",
  #                 df1)[,2:6]
  #   # ObjDL<-dataLoader(verbose.mode = FALSE)
  #   # ObjDL$load.data.frame(mydata =df ,IDName = "ID",EVENTName = "EVENT",dateColumnName = "DATE_INI",
  #   #                       format.column.date = "%Y-%m-%d")
  #   # ObjCFM<-careFlowMiner(verbose.mode = FALSE)
  #   # ObjCFM$loadDataset(inputData = ObjDL$getData())
  #   #
  #   #
  #   # # ObjCFM<-careFlowMiner(verbose.mode = FALSE)
  #   # # ObjCFM$loadDataset(inputData = ObjDL$getData())
  #   # cf.graph<-ObjCFM$plotCFGraph(depth = Inf,  #PROFONDITA
  #   #                              abs.threshold = 10, #support
  #   #                              kindOfGraph = "dot",
  #   #                              nodeShape = "square")$script
  #   return(df)
  # })

  # output$CareFlowGraph<- DT::renderDataTable(newEL)




}

























