#'@title grouping module: main page
#'
#'
#'@import rlang
#'@import shiny
#'@import shinythemes
#'@import dplyr
#'@import shinyWidgets
#'@import DT
#'@impot  ggplot2
#'@import sortable
#'@import pMineR



server.group<-function(input,output,session) {

  tab<-callModule(import_data_server,"uploadEL","EventLog")

  data_reactive<-reactiveValues(
    EventLog = data.frame(),
    pat.process=list(),
    visual=array(),
    tabs=list(),
    ObjDL.out =list()
  )

  observeEvent(input$loadEL,{
    data_reactive$EventLog <- all.data[["EventLog"]]

    if(is_empty(data_reactive$EventLog)){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Load your EventLog, then press 'Load Event Log'  button",
        type = "primary"
        )
      data_reactive$EventLog<-data.frame()
    } else{
      objDL.new <- dataLoader(verbose.mode = FALSE)
      objDL.new$load.data.frame(mydata =data_reactive$EventLog ,IDName = "ID",EVENTName = "EVENT",dateColumnName = "DATE_INI",
                                format.column.date = "%Y-%m-%d")

      obj.out<-objDL.new$getData()
      data_reactive$ObjDL.out<-obj.out
      data_reactive$pat.process<-obj.out$pat.process




  ################   ADD DICTIONARY TAB        ###################        ADD DICTIONARY TAB     ######################       ADD DICTIONARY TAB     ##############################################       ADD DICTIONARY TAB      ########################
      insertTab(inputId = "tabs",
                tabPanel("Add Dictionary",


                         ############################# ADD DICTIONARY & APPLY DICT BUTTONS #############################################################
                         fluidPage(
                           titlePanel("New Dictionary"),
                           br(),
                           dict_mod_ui("dict0", data_reactive$EventLog),
                           br(),
                           br(),
                           fluidRow(
                             actionButton("add",label = "Add new Dictionary")
                           ),
                           br(),
                           fluidRow(
                             actionButton("apply",label = "Apply Dictionary")
                           ),
                           br(),

                           ################################ SHOW GRAPH PANEL ############################################################################
                           fluidRow(
                             tagList(
                             conditionalPanel(condition = 'output.showpanel == "yes"',
                                              absolutePanel(
                                                # top = 20,
                                                left = 50,
                                                right = 50,
                                                bottom = 500,
                                                width = 900, height = 100,
                                                draggable = TRUE,
                                                style = "opacity: 1; z-index: 10;" ,
                                                wellPanel(style = "overflow-y:scroll;background: #F5F5F5; max-height: 800px; height: 900px",
                                                          fluidPage(
                                                            fluidRow(
                                                              column(11,
                                                                     titlePanel("Process Model Preview")),
                                                              column(1,
                                                                     br(),
                                                                     actionButton("close", label= "",icon = icon("fas fa-window-close")))
                                                              ),

                                                            fluidRow(
                                                              column(12,
                                                                     tags$hr())
                                                              ),

                                                            fluidRow(
                                                              column(12,
                                                                     sidebarLayout(
                                                                       sidebarPanel(
                                                                         selectInput("dict", "Select dictionary",
                                                                                     choices = NULL),
                                                                         selectInput("PDalg", "Select algorithm",
                                                                                     # SCELTA ALGORITMO
                                                                                     choices = c("CareFlow Miner","FOMM"),
                                                                                     selected = "FOMM"),

                                                                         conditionalPanel("input.PDalg == 'FOMM'",
                                                                                          tags$hr(),
                                                                                          numericInput("th", label = "Select Thresshold:", min = 0, max = 1, step = 0.01, value = 0),
                                                                                          materialSwitch(
                                                                                            inputId = "al",
                                                                                            label = "Autoloops",
                                                                                            status = "default",
                                                                                            right = TRUE
                                                                                          )
                                                                         ),



                                                                         conditionalPanel("input.PDalg == 'CareFlow Miner'",
                                                                                          tags$hr(),
                                                                                          numericInput("depth", label = "Select depth:", value = 5),
                                                                                          numericInput("support", label ="Select support value:", value = 10),
                                                                                          )

                                                                         ),

                                                                       mainPanel(
                                                                         grVizOutput("CareFlowGraph")

                                                                         )
                                                                       ))
                                                              )
                                                            )
                                                          )
                                                )),
                             absolutePanel(
                               actionButton("show.graph", "show CFM graph")))
                             ),

                           ###################################################################################################################################
                           br(),
                           br()
                         )
                         ),
                target = "Loading Data",
                position = "after")
    }


    callModule(dict_mod_server,"dict0",data_reactive$EventLog, "dict0",data_reactive$pat.process,objDL.new)


    observeEvent(input$add,{
      insertUI(
        selector = "#add",
        where = "beforeBegin",
        ui = dict_mod_ui(paste0("dict", input$add), data_reactive$EventLog)
      )
      callModule(dict_mod_server,paste0("dict", input$add),data_reactive$EventLog, paste0("dict", input$add),data_reactive$pat.process,objDL.new)
    })


    })

 ###################################################################################################################################################################################################################################################################################




  ########## SHOW GRAPH SERVER LOGIC  ########## SHOW GRAPH SERVER LOGIC  ########## SHOW GRAPH SERVER LOGIC  ########## SHOW GRAPH SERVER LOGIC  ########## SHOW GRAPH SERVER LOGIC#####################################
  rv.CFMgr <- reactiveValues(show.panel = FALSE,
                             PD.alg = ""
                             )

  observeEvent(input$show.graph, ({
    rv.CFMgr$show.panel <- !(rv.CFMgr$show.panel)
    if(is_empty(all.dict)){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "no dictionaries saved: create your new dictionary, save it with the 'save dictionary' button and then continue with the 'apply dictionary' button",
        type = "primary"
      )
    }else{
      updateSelectInput(inputId = "dict", "Select dictionary",
                        choices =  c("Plase select a dictionary",unlist(dict.names, recursive = TRUE, use.names = FALSE)),
                        selected = "Plase select a dictionary",
                        session = session)

    }
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

  Mod.graph<-reactive({
    if(input$dict == "Plase select a dictionary"){
      graph<-NULL
    }else {
      df1<-evt.tab(all.dict[[which(dict.names %in% input$dict)]],unique(all.data[[1]][,4]))
      df<-applyDict(column.name="GROUP" ,
                    dict.name = 'main',
                    column.event.name= "EVENT",
                    data_reactive$pat.process,
                    param.EVENTName="EVENT",
                    df1)[,2:6]
      ObjDL<-dataLoader(verbose.mode = FALSE)
      ObjDL$load.data.frame(mydata =df ,IDName = "ID",EVENTName = "EVENT",dateColumnName = "DATE_INI",
                            format.column.date = "%d/%m/%Y")
      out<-ObjDL$getData()

      if(input$PDalg == 'CareFlow Miner'){
        ObjCFM<-careFlowMiner(verbose.mode = FALSE)
        ObjCFM$loadDataset(inputData = out)
        graph<-ObjCFM$plotCFGraph(depth =input$depth,  #PROFONDITA
                                     abs.threshold = input$support, #support
                                     kindOfGraph = "dot",
                                     nodeShape = "square")$script
      }else if(input$PDalg == 'FOMM'){
        param= list("threshold"=data_reactive$th, "considerAutoLoop"= data_reactive$al)
        FOMM<-firstOrderMarkovModel(parameters.list = param)
        FOMM$loadDataset(dataList = ObjDL$getData())
        FOMM$trainModel()
        graph<-FOMM$getModel(kindOfOutput = "grViz")

      }

    }

    return(graph)
  })


  output$CareFlowGraph<- renderGrViz({
    if(is.null( Mod.graph())){
      validate("In this section you can see a preview of the graphs generated by applying a spcific process discovery algorithm to the selected dictionary.
               Please select the dictionary and the type of PD algorithm (and its required parameters). The operation may take a few moments")
    }else{
      grViz( Mod.graph())
    }
  })


 ############################################################################################################################################################################################################################













  ######################       APPLY DICTIONARY TAB     ##############################       APPLY DICTIONARY TAB     #################################       APPLY DICTIONARY TAB     ########################       APPLY DICTIONARY TAB      ##############################       APPLY DICTIONARY TAB     #################



  observeEvent(input$apply,{
    if(is_empty(all.dict)){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "no dictionaries saved: create your new dictionary, save it with the 'save dictionary' button and then continue with the 'apply dictionary' button",
        type = "primary"
      )
    }else{
      tab<-list()
      for (i in c(1:length(all.dict))){
        tab[[i]]<-tabPanel(paste0(dict.names[[i]]),
                           group_visual_mod(paste0("visual",i))
        )
      }

      lapply(1:length(all.dict), function(i){
        callModule(group_visual_server,
                   paste0("visual",i),data_reactive$EventLog, i,data_reactive$pat.process, data_reactive$visual)
      })

      data_reactive$tabs<-tab

      removeTab(inputId = "tabs", target = "Apply Dictionary")
      insertTab(inputId = "tabs",
                tabPanel("Apply Dictionary",
                         titlePanel("Apply Dictionary"),
                         br(),
                         # uiOutput("dict.tabs"),
                         sidebarLayout(
                           sidebarPanel(
                             p(h5("Each tab allows the display of each dictionary added.
                                  You can view the grouping of events allowed by the dictionary through the display of the matching table
                                  or through an histogram showing the new events distribution")),
                             br(),
                             selectInput(inputId = "show.visual", label = "Select the visualization: ",
                                         choices = c("Group Table","Bar Plot"),
                                         selected = "Bar Plot"),
                             br(),
                             p(h5("You can download the new event log by clicking the",strong("download button ")))

                             ),

                           mainPanel(
                             uiOutput("dict.tabs")
                           )
                         ),
                         br(),

                ),
                target = "Add Dictionary",
                position = "after")

    }

  })



  output$dict.tabs<-renderUI({
    tagList(
      do.call(tabsetPanel,data_reactive$tabs)
    )
  })

  observeEvent(input$show.visual,{
    data_reactive$visual<-input$show.visual
    tab<-list()
    for (i in c(1:length(all.dict))){
      tab[[i]]<-tabPanel(paste0(dict.names[[i]]),
                         group_visual_mod(paste0("visual",i))
      )
    }

    lapply(1:length(all.dict), function(i){
      callModule(group_visual_server,
                 paste0("visual",i),data_reactive$EventLog, i,data_reactive$pat.process, data_reactive$visual)
    })

    data_reactive$tabs<-tab
  })


#############################################################################################################################################################################################################################################################################################################################################






}












