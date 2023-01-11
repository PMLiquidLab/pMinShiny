#'@title FOMM module: main page
#'
#'@import shiny
#'@import dplyr
#'@import shinyWidgets
#'@import DT
#'@import pMineR
#'@import DiagrammeR
#'@import shinyjqui
#'@import survminer
#'@import shinyjs

server.FOMM<-function(input,output,session){
  #visualizzazione EventLog
  tab<-callModule(import_data_server,"uploadEL","EventLog")

  # reactiveValues: initializing data as null data frame
  data_reactive <- reactiveValues(
    EventLog=data.frame(),
    th = c(),  #threshold
    al = c(),  #autoloops
    FOMM = c(),
    numerical.att = c(),
    fun.train.out= list()
  )

  observe({
    toggleState("loadEL", tab$complete == TRUE)
  })


  #TAB EVENTLOG: data visualization of eventlog
  observeEvent(input$loadEL,{
    id.ind<-which(colnames(all.data[[1]])==tab$id)
    date.ind<-which(colnames(all.data[[1]])==tab$date)
    event.ind<-which(colnames(all.data[[1]])==tab$event)

    data_reactive$EventLog <- all.data[["EventLog"]]



    tryCatch({
    if(is_empty(data_reactive$EventLog)){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Load your EventLog, then press 'Load Event Log'  button",
        type = "primary"
      )
      data_reactive$EventLog<-data.frame()
    }
    else if ((id.ind == date.ind) ||
             (id.ind == event.ind) || (date.ind == event.ind)) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Please check the mapping: column selected twice",
        type = "primary"
      )
      data_reactive$EventLog <- data.frame()
    }else if(is.na(as.Date(all.data[[1]][, date.ind][1], "%Y-%m-%d"))){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Please check the Date Format",
        type = "primary"
      )
      data_reactive$EventLog<-data.frame()
    }else{
      colnames(all.data[[1]])[id.ind]<<-"ID"
      colnames(all.data[[1]])[date.ind]<<-"DATE_INI"
      colnames(all.data[[1]])[event.ind]<<-"EVENT"
      data_reactive$EventLog <- all.data[["EventLog"]]
      arr.attributi<-c()
      for (i in c(1:length(colnames(data_reactive$EventLog)))) {
        name <- colnames(data_reactive$EventLog)[i]
        if (length(unique(data_reactive$EventLog[, name])) > 20) {
          arr.attributi[i] <- colnames(data_reactive$EventLog)[i]
        }
      }
      arr.attributi<-arr.attributi[-which(arr.attributi %in% c("ID","DATE_INI","EVENT",NA))]

      data_reactive$numerical.att<-arr.attributi


      #factor check
      if(is.factor(data_reactive$EventLog$EVENT)) { data_reactive$EventLog$EVENT <- as.character(data_reactive$EventLog$EVENT)  }

      showModal(modalDialog(title = "Data loading may take a few moments",easyClose = TRUE, footer=NULL))
      # DLobj
      ObjDL<<-dataLoader(verbose.mode = FALSE)
      ObjDL$load.data.frame(mydata =data_reactive$EventLog ,IDName = "ID",EVENTName = "EVENT",dateColumnName = "DATE_INI",
                            format.column.date = "%Y-%m-%d")

      # FOMM obj
      param= list("threshold"=data_reactive$th, "considerAutoLoop"= data_reactive$al)
      data_reactive$param<-param
      FOMMobj<-FOMM(parameters.list = param,verbose.mode = F)
      # FOMMobj<-firstOrderMarkovModel(parameters.list = param)
      FOMMobj$loadDataset(dataList = ObjDL$getData())
      FOMMobj$trainModel()
      data_reactive$FOMM<-FOMMobj
      removeModal()
      globalList <- reactiveValues(ManyChoices = unique(data_reactive$EventLog$EVENT), SelectedPrev = c())



      ############################################################# FOMM GRAPH TAB #########################################################################
      removeTab(inputId = "tabs", target = "FOMM")
      insertTab(inputId = "tabs",
                tabPanel("FOMM",
                         titlePanel("Process Discovery: FOMM"),
                         br(),
                         fluidRow(
                           column(12,
                                  sidebarLayout(
                                    sidebarPanel(
                                      width = 3,
                                      p(h3("Parameter Setting")),
                                      tags$hr(),

                                      #parametro soglia
                                      fluidRow(
                                        column(7,
                                               numericInput("th", label = "Select Thresshold:", min = 0, max = 1, step = 0.01, value = 0),
                                               ),
                                        column(5,
                                               br(),
                                               br(),
                                               materialSwitch(
                                                 inputId = "al",
                                                 label = "Autoloops",
                                                 status = "default",
                                                 right = TRUE)
                                               )
                                        )
                                      ),

                                    mainPanel(jqui_resizable(grVizOutput("fomm.Graph")))
                                    )
                                  )
                           )
                         ),
                target = "Loading EventLog",
                position = "after"
      )
      ############################################################# PATH ANALYSIS TAB #########################################################################
      removeTab(inputId = "tabs", target = "Path Analysis")
      insertTab(inputId = "tabs",
                tabPanel("Path Analysis",
                         titlePanel("Process Discovery: Survival Analysis and Covariate time evolution"),
                         fluidRow(
                           column(12,
                                  sidebarLayout(
                                    sidebarPanel(
                                      width = 4,

                                      #Info popup
                                      fluidRow(
                                        column(3,offset = 9,
                                               dropdownButton(
                                                 tags$h4(strong("Survival Analysis with Kaplan Meier")),

                                                 tags$h5("The cohort consists of patients transiting through the node chosen as the start node,
                                                         which must be selected in the ", strong("\"node id start\" field."),"and who have experienced a certain state of interest,
                                                         which must be made explicit in the", strong("\"node id end\" field.")),

                                                 tags$h5("Through the", strong("\"id node censored\" field") ," it will be possible to indicate in which nodes the patients will have to transit
                                                 to in order to be considered censored."),


                                                 tags$h5("using the input ",strong("\"use leaf as cens\"")," it will be possible to choose whether to follow the clinical follow-up of patients up to the last event they experienced"),

                                                 circle = FALSE,
                                                 status = "info",
                                                 size = "xs",
                                                 icon = icon("fas fa-info"),
                                                 width = "300px",
                                                 right = TRUE,
                                                 tooltip = tooltipOptions(title = "Click to more info")
                                               )
                                        )
                                      ),

                                      #path selection
                                      fluidRow(
                                        tabsetPanel(id = "path.tab",
                                                    #Tab path
                                                    tabPanel("Path 1",
                                                             path_mod_ui("path1",tit = "Path 1" ,is.fomm= TRUE,node.list=data_reactive$node.list,el.data = data_reactive$EventLog,is.strat.var = FALSE )
                                                             )
                                                    )
                                        ),

                                      fluidRow(column(12,br())),

                                      #cov selection (only if cov analysis tab is selected)
                                      conditionalPanel(condition = "input.tabselected== 2 ",
                                                       fluidRow(
                                                         column(8,
                                                                pickerInput(inputId = "covariate.time",
                                                                            label = "Select covariate",
                                                                            choices =colnames(data_reactive$EventLog)[!(colnames(data_reactive$EventLog) %in% c("ID","DATE_INI","EVENT"))],
                                                                            selected = NULL,
                                                                            multiple = FALSE,
                                                                            options = list(title = "select var"))),
                                                         column(4,br())
                                                       )
                                      ),

                                      fluidRow(
                                        column(4,actionButton("add.path","Add path")),
                                        column(4,actionButton("plot.all.surv","Show Graphs")),
                                        column(4,actionButton("reset","Reset path"))
                                        )

                                      ),

                                    mainPanel(

                                      tabsetPanel(id="tabselected",
                                        #PANEL 1: KM CURVES + LOG RANK TEST TAB
                                        tabPanel("Surv",value = 1,
                                                 fluidRow(
                                                   column(10,span(textOutput("error.mex"),style="color:gray")),
                                                   column(1,
                                                          dropdownButton(
                                                            grVizOutput("prev.fomm"),
                                                            circle = FALSE,
                                                            status = "primary",
                                                            size = "xs",
                                                            icon = icon("project-diagram"),
                                                            width = "1000px",
                                                            right = TRUE,
                                                            tags$div(style = "height: 100px;"),
                                                            tooltip = tooltipOptions(title = "Click to more info"))
                                                          ),
                                                   column(1,
                                                          dropdownButton(
                                                            p(h4(strong("Select path to plot"))),
                                                            checkboxGroupInput("path.plot", label = "",
                                                                               choices = "path1",
                                                                               selected = "path1"),
                                                            actionButton("render.km.graph","Refresh graph"),
                                                            circle = FALSE,
                                                            status = "danger",
                                                            size = "xs",
                                                            icon = icon("cogs"),
                                                            width = "100px",
                                                            right = TRUE,
                                                            tooltip = tooltipOptions(title = "Click to more info"))
                                                          )
                                                   ),

                                                 fluidRow(
                                                   column(12,plotOutput("km.curves",width =  "100%"))
                                                   ),

                                                 fluidRow(
                                                   column(12,DT::dataTableOutput("logrank.res"))
                                                   )

                                                 ),
                                        #PANEL 2: COVARIATE DIST
                                        tabPanel("cov-time",value = 2,
                                                 fluidRow(
                                                   column(10,shiny::plotOutput("cov_time_graph")),
                                                   column(1,
                                                          dropdownButton(
                                                            fluidRow(column(12,grVizOutput("prev.fomm.cov"))),

                                                            circle = FALSE,
                                                            status = "primary",
                                                            size = "xs",
                                                            icon = icon("project-diagram"),
                                                            width = "1000px",
                                                            right = TRUE,
                                                            tags$div(style = "height: 100px;"),
                                                            tooltip = tooltipOptions(title = "Click to more info"))
                                                          ),

                                                   column(1,
                                                          dropdownButton(
                                                            p(h4(strong("Plot settings"))),
                                                            fluidRow(
                                                              column(6,
                                                                     selectInput(inputId = "UM.cov.plot",
                                                                                 label = "Select Time unit",
                                                                                 choices = c("mins","days","weeks","months","years"),
                                                                                 selected = "days")
                                                                     ),
                                                              column(6,
                                                                     selectInput(inputId = "legend.pos.cov",
                                                                                 label = "Set legend position",
                                                                                 choices = c("bottomright", "bottom", "bottomleft",
                                                                                             "left", "topleft", "top", "topright", "right", "center"),
                                                                                 selected = "topleft")
                                                                     )
                                                              ),

                                                            fluidRow(
                                                              column(6,
                                                                     materialSwitch(
                                                                       inputId = "reg.line",
                                                                       label = "plot regression line",
                                                                       status = "primary",
                                                                       right = TRUE)
                                                                     )
                                                            ),

                                                            fluidRow(
                                                              column(6,
                                                                     materialSwitch(
                                                                       inputId = "mean.ci",
                                                                       label = "plot mean and c.i.",
                                                                       status = "primary",
                                                                       right = TRUE)
                                                                     ),

                                                              column(6,
                                                                     numericInput("delta.mean.ci", label = "Select time window:", value = 10,min = 5)
                                                                     )
                                                              ),

                                                            circle = FALSE,
                                                            status = "danger",
                                                            size = "xs",
                                                            icon = icon("cogs"),
                                                            width = "400px",
                                                            right = TRUE,
                                                            tooltip = tooltipOptions(title = "Click to more info"))
                                                          )
                                                   )
                                                 )
                                        )
                                      )
                                    )
                                  )
                           )
                         ),
                target = "FOMM",
                position = "after"
                )


      ################################################# TAB PREDICTIVE MODEL:LR ########################################################
      removeTab(inputId = "tabs", target = "Predictive Model")
      insertTab(inputId = "tabs",
                tabPanel("Predictive Model",
                         titlePanel("Predictive Process Discovery"),
                         fluidRow(
                           column(12,
                                  sidebarLayout(
                                    sidebarPanel(
                                      #RIGA 1:input x event start & event goal
                                      fluidRow(
                                        column(6,
                                               pickerInput(inputId ="eventoDiPartenza", label = "select event start",
                                                           choices =  unique(data_reactive$EventLog$EVENT),
                                                           multiple = FALSE,
                                                           selected = NULL,
                                                           options = list(
                                                             title = "select event"))
                                               ),
                                        column(6,
                                               pickerInput(inputId ="eventoGoal", label = "select event goal",
                                                           choices =  unique(data_reactive$EventLog$EVENT),
                                                           multiple = FALSE,
                                                           selected = NULL,
                                                           options = list(
                                                             title = "select event"))
                                               )
                                      ),

                                      #RIGA 2: FILTERING ON PATH
                                      fluidRow(
                                        column(6,
                                               pickerInput(inputId ="passing", label = "passing through",
                                                           choices =  unique(data_reactive$EventLog$EVENT),
                                                           multiple = TRUE,
                                                           selected = NULL,
                                                           options = list(
                                                             title = "select event"))
                                        ),
                                        column(6,
                                               pickerInput(inputId ="NOTpassing", label = "NOT passing through",
                                                           choices =  unique(data_reactive$EventLog$EVENT),
                                                           multiple = TRUE,
                                                           selected = NULL,
                                                           options = list(
                                                             title = "select event"))
                                        )
                                      ),

                                      fluidRow(
                                        column(3,
                                               numericInput("min.time", label = "min time", value = 0, min = 0, max = Inf)

                                        ),
                                        column(3,
                                               numericInput("max.time", label = "max time", value = 30, min = 0, max = Inf)
                                        ),
                                        column(2,
                                               checkboxInput("max.time.inf", label = h6("max = Inf"), value = TRUE),
                                        ),
                                        column(4,
                                               selectInput("um.time",
                                                           label = "Select the time scale",
                                                           choices = c("mins","hours","days","weeks","months"),
                                                           selected = "days")
                                               )
                                      ),

                                      #RIGA 3: input x arr_att & feature-selection
                                      fluidRow(
                                        column(6,
                                               pickerInput(
                                                 inputId = "arr.attributi",
                                                 label = "Select/deselect attributes",
                                                 choices = data_reactive$numerical.att,
                                                 options = list(
                                                   `actions-box` = TRUE),
                                                 multiple = TRUE)
                                               ),
                                        column(6,
                                               br(),
                                               materialSwitch(
                                                 inputId = "feature_selection",
                                                 label = "Feature Selection",value = FALSE
                                               )
                                               )

                                      ),
                                      tags$hr(),

                                      #RIGA FACOLTATIVA IN CASO DI FEATURE SELECTION
                                      conditionalPanel(condition = "input.feature_selection",
                                        fluidRow(
                                          column(6,
                                                 numericInput("n.att", label = "number of covariate", value = 1, min = 1, max = length(data_reactive$numerical.att)),
                                                 ),
                                          column(6,
                                                 numericInput("p.thr", label = "min p-value train", value = 0.05, min = 0.05,
                                                              max = 1,step = 0.01)
                                                 )

                                        ),
                                        fluidRow(
                                          column(6,
                                                 selectInput(inputId = "tec",label = "select feature selection technique",choices = c("K-fold Cross Vaidation","Hold out"))
                                                 #select CROSSVAL-HOLD OUT
                                                 ),
                                          conditionalPanel(condition = "input.tec =='K-fold Cross Vaidation'",
                                            column(6,
                                                   numericInput("k", label = "select number of fold", value = 2, min = 2, max = 10)
                                                   )
                                          ),
                                          conditionalPanel(condition = "input.tec== 'Hold out'",
                                                           column(6,
                                                                  numericInput("p.train", label = "select prop of train", value = 0.7, min = 0.1, max = 1,step = 0.1)
                                                           )
                                          )
                                        )
                                      ),
                                      fluidRow(
                                        column(2,offset = 7,
                                               actionButton(inputId = "train.model","Train Model")
                                               )
                                      ),
                                    ),
                                    mainPanel(
                                      uiOutput("show.train.result")
                                    )
                                  )
                                  )
                         )

                ),
                target = "Path Analysis",
                position = "after"
      )}
  },
  error = function(e) {
    sendSweetAlert(
      session = session,
      title = "Error",
      text = "Please check the mapping: column selected as DATE seems to have no date format",
      type = "primary"
    )

  })

    #########################################################  MODEL TRAINING #################################################################

    arr.ID.train<-sample(x = unique(all.data[[1]]$ID),size = 700)
    arr.ID.test<-unique(all.data[[1]]$ID)[-which(unique(all.data[[1]]$ID) %in% arr.ID.train)]

    observeEvent(input$train.model,{
      data_reactive$fun.out<-list()

      if(input$tec=="Hold out"){
        k<-1
      }else{
        k<-input$k
      }

      if(is.null(input$passing)){
        passing<-c()
      }else{
        passing<-input$passing
      }

      if(is.null(input$NOTpassing)){
        NOTpassing<-c()
      }else{
        NOTpassing<-input$NOTpassing
      }

      if(input$max.time.inf){
        max_time<-Inf
      }else{
        max_time<-input$max.time
      }

      if(!input$feature_selection){
        n.att<-length(input$arr.attributi)
      }else{
        n.att<-input$n.att
      }

      showModal(modalDialog(title = "Training may take a few moments",
                            easyClose = TRUE, footer=NULL))

      fun.train.out<- data_reactive$FOMM$Predictive.model(eventStart = input$eventoDiPartenza,
                                        eventGoal = input$eventoGoal,
                                        obj.out =  ObjDL$getData(),
                                        arr.attributes = input$arr.attributi,
                                        arr.ID.train = arr.ID.train,
                                        arr.ID.test = arr.ID.test,
                                        feature.selection = input$feature_selection,
                                        k = k,passing = passing,
                                        NOTpassing = NOTpassing,
                                        p.train=input$p.train,
                                        p.thr=input$p.thr,
                                        n.att =n.att,
                                        min.time= input$min.time,
                                        max.time= max.time,omit.missing=T,
                                        UM= input$um.time,
                                        )

      removeModal()

      data_reactive$fun.train.out<-fun.train.out$res


      output$show.train.result<-renderUI({
        fluidPage(
          fluidRow(
           textOutput("error.model")
          ),
          fluidRow(
            DT::dataTableOutput("mat.att")
          ),
          fluidRow(
            plotOutput("AUC_kernel_density_all")
          ),
          fluidRow(
            uiOutput("results")
          )
        )
      })

      output$error.model<-renderText({
        if (fun.train.out$run.check) {
          " "
        } else if(is.null(data_reactive$fun.train.out$lst.model)){
          "none of the selected attributes presented a pvalue lower than the chosen threshold  "
          }else{
         "the model did not reach convergence:probably the number of patients responding to the specified path is not enough to train the model"
        }
      })

      if(fun.train.out$run.check & (length(data_reactive$fun.train.out)!=0)){
        #caso HOLD OUT: kernel density plot non viene mostrato
        if(length(data_reactive$fun.train.out$lst.model)==1){
          output$AUC_kernel_density_all<-renderPlot({})
        }else{
        #caso CROSS VAL: mostro kernel density AUC
          lst.AUC.test<-lapply(data_reactive$fun.train.out$lst.model,function(model) {lapply(model, function(inner.fold){return(inner.fold$model_perf$AUC_test)})})

          output$AUC_kernel_density_all<-renderPlot({
            #caso feature selection OK
            if(input$feature_selection & (length(data_reactive$fun.train.out)!=0)){
              AUC_KD_fun(lst.AUC.test,all.fold = TRUE)
            }else{
              #CASO NO FEATURE SELECTION FALSE X PLOT KERNEL DENSITY
            }
          })

        }



        output$mat.att<- DT::renderDataTable(data_reactive$fun.train.out$mat.perf.total)

        }else{
        output$mat.att<- DT::renderDataTable(data.frame())
        }

      })

    observeEvent(input$feature_selection,{
      data_reactive$fun.train.out<-list()
    })

    observeEvent(input$mat.att_rows_selected,{
    fun.train.out<-data_reactive$fun.train.out

    output$results<-renderUI({
      fluidPage(
        fluidRow(
          plotOutput("final.roc")
        )
      )
    })

    # feature.selection<-input$feature_selection
    output$final.roc<-renderPlot({
      if(length(input$mat.att_rows_selected)){
          if(!input$feature_selection || (input$feature_selection & (length(data_reactive$fun.train.out$lst.model)==1))){
            df_roc_test<-fun.train.out$final.model$roc_test
            df_roc<-fun.train.out$final.model$roc_train
            AUC_test<-fun.train.out$final.model$AUC_test
            AUC<-fun.train.out$final.mode$AUC_train
          }else{
            df_roc_test<-fun.train.out$final.model[[input$mat.att_rows_selected[1]]]$final.model$roc_test
            df_roc<-fun.train.out$final.model[[input$mat.att_rows_selected[1]]]$final.model$roc_train
            AUC_test<-fun.train.out$final.model[[input$mat.att_rows_selected[1]]]$final.model$AUC_test
            AUC<-fun.train.out$final.model[[input$mat.att_rows_selected[1]]]$final.model$AUC_train
          }


          plot.roc <- plot(df_roc$FPR, df_roc$TPR, type="l", xlim=c(0,1), ylim=c(0,1), lwd=2,
                           xlab= "FRP",ylab="TPR",col= "gray")+
            abline(0,1, col="red", lty=2)+
            text(x = 0.8,y = 0.12, paste("AUC_train =",round(AUC,digits = 4)))+
            points(df_roc_test$FPR, df_roc_test$TPR, type="b", xlim=c(0,1), ylim=c(0,1), lwd=2,
                   xlab= "FRP",ylab="TPR")+
            lines(df_roc_test$FPR, df_roc_test$TPR)+
            abline(0,1, col="red", lty=2)+
            text(x = 0.8,y = 0.22, paste("AUC_test =",round(AUC_test,digits = 4)))
          legend('topleft',
                 legend=c("Test", "Train"),
                 col=c("black","gray"), lty=c(2,1),
                 cex=0.8,
                 bty = 'o'

          )
        }


      })

    })

    #########################################################  PATH ANALYSIS FUNCTIONS #################################################################
    #server function for path1
    callModule(path_data_server,"path1",data_reactive$EventLog,input$add.path+1,is.fomm= TRUE)

    #ADD TAB PATH E AGGIORNO CHECKBOX PATH TO PLOT
    observeEvent(input$add.path,{
      insertTab("path.tab",
                tabPanel(paste("Path",input$add.path+1),
                         path_mod_ui(paste0("path",input$add.path+1),tit = paste("Path",input$add.path+1),is.fomm= TRUE,node.list=data_reactive$node.list,el.data = data_reactive$EventLog,is.strat.var=FALSE)
                         ),
                target = paste("Path", input$add.path),
                position = "after",
                select = TRUE)
      #server fun x path mod
      callModule(path_data_server,session = session, paste0("path",input$add.path+1),data_reactive$EventLog,input$add.path+1,is.fomm= TRUE)

      paths.rm <- c()
      for (nomi.path in names(all.path)) {
        if ((all.path[[nomi.path]][["id.end"]]=="") || (all.path[[nomi.path]][["id.start"]]==""))
          paths.rm <- c(paths.rm, nomi.path)
      }

      if (length(paths.rm) ==1) {
        all.path[[paths.rm]] <- NULL
      }else if(length(paths.rm)>1){
        for (i in c(1:length(paths.rm))) {
          all.path[[paths.rm[i]]]<-NULL
          }
        }

      #aggiorno lista path salvati
      data_reactive$paths<-all.path
      data_reactive$paths.rm<-paths.rm
      #e quelli che voglio plottare (tutti)
      choices_list<-names(all.path)
      updateCheckboxGroupInput(session, inputId = "path.plot",
                               label = "",
                               choices = choices_list ,
                               selected =choices_list)
      })

    #CREAZIONE DEL PLOT KM
    observeEvent(input$plot.all.surv,{
      paths.rm <- c()
      for (nomi.path in names(all.path)) {
        if ((all.path[[nomi.path]][["id.end"]]=="") || (all.path[[nomi.path]][["id.start"]]==""))
          paths.rm <- c(paths.rm, nomi.path)
      }

      if (length(paths.rm) ==1) {
        all.path[[paths.rm]] <- NULL
      }else if(length(paths.rm)>1){
        for (i in c(1:length(paths.rm))) {
          all.path[[paths.rm[i]]]<-NULL
        }

      }


      data_reactive$paths<-all.path
      data_reactive$paths.rm<-paths.rm
      choices_list<-names(data_reactive$paths)

      updateCheckboxGroupInput(session, inputId = "path.plot",
                               label = "",
                               choices = choices_list ,
                               selected =choices_list)
      data_reactive$paths.to.plot<-names(data_reactive$paths)

      if(length(data_reactive$paths)==0){
        sendSweetAlert(
          session = session,
          title = "Error",
          text = "At least one path must be entered",
          type = "primary"
        )
      }else{

        fun.out<-render.km.graph.FOMM(data_reactive$paths,data_reactive$paths.to.plot)

        output$error.mex<-renderText({
          if (length(data_reactive$paths.rm) == 0) {
            " "

          } else{
            paste( data_reactive$paths.rm,
                  "not shown: please check values entred")
          }

        })


        output$km.curves<-renderPlot({
          if(length(fun.out)==3){
            survminer::ggsurvplot(fun.out$final.surv,
                       data = fun.out$final.data,
                       conf.int = TRUE,          # Add confidence interval
                       risk.table = TRUE,        # Add risk table
                       risk.table.height = 0.27,
                       # Risk table color by groups
                       risk.table.col = "strata")
            }
          })

        output$logrank.res<- DT::renderDataTable({
          p(h4(strong("Results of Logrank test on Paths")))
          if(length(all.path)>1){
            logrank_fun(fun.out,data_reactive$paths)
          }else{
            data.frame()
            }
        })

        }

      #creazione plot andamento covariate nel tempo
      if(is.null(input$covariate.time) || input$covariate.time==""){

      }else{
        arr.from<-do.call("cbind",do.call("rbind",data_reactive$paths)[,1])
        lst.to<-do.call("rbind",data_reactive$paths)[,2]
        pass<-do.call("rbind",data_reactive$paths)[,9]
        not.pass<-do.call("rbind",data_reactive$paths)[,10]


        #SERIE DI CONTROLLI CHE VANNO SISTEMATI:
        #sto assumendo che se length(unique(data_reactive$EventLog[,input$covariate]))<7 allora la cov che sto considerando è categorica
        if(is.null(arr.from) |is.null(lst.to[[1]])){
          sendSweetAlert(
            session = session,
            title = "Error",
            text = "Please enter values for id node start and/or id node end",
            type = "primary"
          )
          output$cov_time_graph<- shiny::renderPlot()

        }else{
          output$cov_time_graph<- shiny::renderPlot({
            if(length(unique(data_reactive$EventLog[,input$covariate.time]))<10){
              sendSweetAlert(
                session = session,
                title = "Error",
                text = "Please select numerical variables as covariate",
                type = "primary")

            }else{
              df_tot<-pre_fun_fomm(ObjDL = ObjDL,
                                   FOMM = FOMM,
                                   arr.from = arr.from,
                                   lst.to = lst.to,
                                   covariate = input$covariate.time,
                                   lst.passingThrough=pass,
                                   lst.passingNotThrough=not.pass)

              if(!is.null(df_tot)){
                plot_cov_graph(df_tot = df_tot,
                               plot.ci.mean=input$mean.ci,
                               delta = input$delta.mean.ci,
                               points.symbols=20,
                               plot.RegressionLine = input$reg.line,
                               legend.position = input$legend.pos.cov,
                               UM = input$UM.cov.plot,
                               size.symbols=1.5,
                               line.width=2,
                               y.int.legend=0.8,
                               legend.text.size=0.8)}
              }
          })
        }
      }
      })



    #RENDER DEL GRAFICO KM IN CADO SI DESELEZIONE DEI PATH
    observeEvent(input$render.km.graph,{
      data_reactive$paths<-all.path
      if(length(data_reactive$paths)<1){
        sendSweetAlert(
          session = session,
          title = "Error",
          text = "Every time you create a new path please save it using the proper button",
          type = "primary"
        )
      }else{

        if(is.null(input$path.plot)){
          fun.out<-list("id.not.valid"=c())
        }else{
          fun.out<-render.km.graph.FOMM(data_reactive$paths,input$path.plot)
        }

        output$error.mex<-renderText({
          if(length(fun.out)==3 & !is.null(fun.out$id.not.valid)){
            paste("Path",fun.out$id.not.valid, "not shown: please check values entred" )
            # paste("please check values entered for path:",fun.out)
          }else if(length(fun.out)==1 & !is.null(fun.out$id.not.valid)){
            paste("please check values entered for path:",fun.out)
          }else{
            ""
          }
        })

        output$km.curves<-renderPlot({
          if(length(fun.out)==3){
            ggsurvplot(fun.out$final.surv,
                       data = fun.out$final.data,
                       conf.int = TRUE,          # Add confidence interval
                       risk.table = TRUE,        # Add risk table
                       risk.table.height = 0.27,
                       risk.table.col = "strata"# Risk table color by groups
            )
          }
        })
      }


    })


    #RESET ALL PATH
    observeEvent(input$reset,{
      all.path<<-list()
      data_reactive$paths<<-all.path
      data_reactive$data_reactive$paths.to.plot<-"path1"
      choices_list<-"path1"
      updateCheckboxGroupInput(session, inputId = "path.plot",
                               label = "",
                               choices = choices_list ,
                               selected =choices_list)
      output$km.curves<-renderPlot({
        NULL
      })
      output$logrank.res<- DT::renderDataTable({
        data.frame()
      })

      output$error.mex<-renderText("")

      removeTab(inputId = "tabs", target = "Path Analysis")
      insertTab(inputId = "tabs",
                tabPanel("Path Analysis",
                         titlePanel("Process Discovery: Survival Analysis and Covariate time evolution"),
                         fluidRow(
                           column(12,
                                  sidebarLayout(
                                    sidebarPanel(
                                      width = 4,
                                      fluidRow(
                                        column(9,
                                        ),
                                        column(3,
                                               dropdownButton(
                                                 tags$h4(strong("Survival Analysis with Kaplan Meier")),

                                                 tags$h5("The cohort consists of patients transiting through the node chosen as the start node,
                                                         which must be selected in the ", strong("\"node id start\" field."),"and who have experienced a certain state of interest,
                                                         which must be made explicit in the", strong("\"node id end\" field.")),

                                                 tags$h5("Through the", strong("\"id node censored\" field") ," it will be possible to indicate in which nodes the patients will have to transit
                                                 to in order to be considered censored."),


                                                 tags$h5("using the input ",strong("\"use leaf as cens\"")," it will be possible to choose whether to follow the clinical follow-up of patients up to the last event they experienced"),


                                                 circle = FALSE,
                                                 status = "info",
                                                 size = "xs",
                                                 icon = icon("fas fa-info"),
                                                 width = "300px",
                                                 right = TRUE,
                                                 tooltip = tooltipOptions(title = "Click to more info")
                                               )
                                        )
                                      ),
                                      fluidRow(
                                        tabsetPanel(id = "path.tab",
                                                    tabPanel("Path 1",
                                                             path_mod_ui("path1",tit = "Path 1" ,
                                                                         is.fomm= TRUE,
                                                                         node.list=data_reactive$node.list,
                                                                         el.data = data_reactive$EventLog,
                                                                         is.strat.var = FALSE )
                                                    )
                                        )
                                      ),
                                      fluidRow(
                                        column(12,
                                               br()
                                        )
                                      ),
                                      conditionalPanel(condition = "input.tabselected== 2 ",
                                                       fluidRow(
                                                         column(8,
                                                                pickerInput(inputId = "covariate.time",
                                                                            label = "Select covariate",
                                                                            choices =colnames(data_reactive$EventLog)[!(colnames(data_reactive$EventLog) %in% c("ID","DATE_INI","EVENT"))],
                                                                            selected = NULL,
                                                                            multiple = FALSE,
                                                                            options = list(title = "select var"))),
                                                         column(4,br())
                                                       )
                                      ),



                                      fluidRow(
                                        column(4,
                                               actionButton("add.path","Add path")
                                        ),
                                        column(4,
                                               actionButton("plot.all.surv","Show Graphs")
                                        ),
                                        column(4,
                                               actionButton("reset","Reset path")
                                        )
                                      )

                                    ),
                                    mainPanel(
                                      tabsetPanel(id = "tabselected",
                                                  tabPanel("Survival", value = 1,
                                                           fluidRow(
                                                             column(10,

                                                                    span(textOutput("error.mex"),style="color:gray")

                                                             ),
                                                             column(1,
                                                                    dropdownButton(
                                                                      grVizOutput("prev.fomm"),

                                                                      circle = FALSE,
                                                                      status = "primary",
                                                                      size = "xs",
                                                                      icon = icon("project-diagram"),
                                                                      width = "1000px",
                                                                      right = TRUE,
                                                                      tags$div(style = "height: 100px;"),
                                                                      tooltip = tooltipOptions(title = "Click to more info")
                                                                    )
                                                             ),
                                                             column(1,
                                                                    dropdownButton(
                                                                      p(h4(strong("Select path to plot"))),
                                                                      checkboxGroupInput("path.plot", label = "",
                                                                                         choices = "path1",
                                                                                         selected = "path1"),
                                                                      actionButton("render.km.graph","Refresh graph"),


                                                                      circle = FALSE,
                                                                      status = "danger",
                                                                      size = "xs",
                                                                      icon = icon("cogs"),
                                                                      width = "100px",
                                                                      right = TRUE,
                                                                      tooltip = tooltipOptions(title = "Click to more info")
                                                                    )
                                                                    # uiOutput("select.path")

                                                             )

                                                           ),
                                                           fluidRow(
                                                             column(12,
                                                                    plotOutput("km.curves",width =  "100%")
                                                             )
                                                           ),
                                                           fluidRow(
                                                             column(12,
                                                                    DT::dataTableOutput("logrank.res")
                                                             )
                                                           )
                                                  ),
                                                  tabPanel("cov-time",value = 2,
                                                           fluidRow(
                                                             column(10,
                                                                    shiny::plotOutput("cov_time_graph")
                                                             ),
                                                             column(1,
                                                                    dropdownButton(

                                                                      fluidRow(
                                                                        column(12,
                                                                               grVizOutput("prev.fomm.cov")
                                                                        )
                                                                      ),
                                                                      # grVizOutput("prev.cfm"),

                                                                      circle = FALSE,
                                                                      status = "primary",
                                                                      size = "xs",
                                                                      icon = icon("project-diagram"),
                                                                      width = "1000px",
                                                                      right = TRUE,
                                                                      tags$div(style = "height: 100px;"),
                                                                      tooltip = tooltipOptions(title = "Click to more info")
                                                                    )
                                                             ),
                                                             column(1,
                                                                    dropdownButton(
                                                                      p(h4(strong("Plot settings"))),
                                                                      fluidRow(
                                                                        column(6,
                                                                               selectInput(inputId = "UM.cov.plot",
                                                                                           label = "Select Time unit",
                                                                                           choices = c("mins","days","weeks","months","years"),
                                                                                           selected = "days")
                                                                        ),
                                                                        column(6,
                                                                               selectInput(inputId = "legend.pos.cov",
                                                                                           label = "Set legend position",
                                                                                           choices = c("bottomright", "bottom", "bottomleft",
                                                                                                       "left", "topleft", "top", "topright", "right", "center"),
                                                                                           selected = "topleft")
                                                                        )
                                                                      ),
                                                                      fluidRow(
                                                                        column(6,
                                                                               materialSwitch(
                                                                                 inputId = "reg.line",
                                                                                 label = "plot regression line",
                                                                                 status = "primary",
                                                                                 right = TRUE
                                                                               )
                                                                        )
                                                                      ),
                                                                      fluidRow(
                                                                        column(6,
                                                                               materialSwitch(
                                                                                 inputId = "mean.ci",
                                                                                 label = "plot mean and c.i.",
                                                                                 status = "primary",
                                                                                 right = TRUE
                                                                               )
                                                                        ),
                                                                        column(6,
                                                                               numericInput("delta.mean.ci", label = "Select time window:", value = 10,min = 5),
                                                                        )

                                                                      ),
                                                                      circle = FALSE,
                                                                      status = "danger",
                                                                      size = "xs",
                                                                      icon = icon("cogs"),
                                                                      width = "400px",
                                                                      right = TRUE,
                                                                      tooltip = tooltipOptions(title = "Click to more info")
                                                                    )
                                                             )
                                                           )
                                                  )

                                      )


                                    )
                                  )
                           )
                         )

                ),
                target = "FOMM",
                position = "after",select = TRUE
      )



    })


    observeEvent(input$th,{
      data_reactive$th<-input$th
    })

    observeEvent(input$al,{
      data_reactive$al<-input$al
    })



    fomm.graph<-reactive({

      param= list("threshold"=data_reactive$th, "considerAutoLoop"= data_reactive$al)
      data_reactive$param<-param
      FOMMobj<<-FOMM(parameters.list = param)
      # FOMMobj<-firstOrderMarkovModel(parameters.list = param)
      FOMMobj$loadDataset(dataList = ObjDL$getData())
      FOMMobj$trainModel()
      data_reactive$FOMM<-FOMMobj
      fomm.plot<-data_reactive$FOMM$getModel(kindOfOutput = "grViz")
      return(fomm.plot)
    })

    output$fomm.Graph<-renderGrViz({
      grViz(fomm.graph())
    })

    output$prev.fomm.cov<-renderGrViz({
      grViz(fomm.graph())
    })


    #################################################### FOMM GRAPH TAB FUNCTIONS ##############################################################################

    output$prev.fomm<-renderGrViz({
      grViz(fomm.graph())
    })

    surv<-reactive({
      FOMM<-data_reactive$FOMM
      pass.th<-input$pass.thr
      pass.not.th<-input$pass.not.thr
      pdv<-input$PDVat
      if(pass.th=="" || is.null(pass.th)){

        pass.th<-c()
      }
      if(pass.not.th=="" || is.null(pass.not.th)){
        pass.not.th<-c()
      }

      if(pdv=="" || is.null(pdv)){
        pdv<-c()
      }

      KM <- KaplanMeier(fromState = input$event.from,
                        toState = input$event.to,
                        ObjDL,
                        passingThrough=pass.th,
                        passingNotThrough=pass.not.th,
                        PDVAt=pdv,
                        UM=input$UM)
      if(is.null(KM)){
        to_ret<-NULL
      }else if(input$event.from == input$event.to){
        to_ret<-NULL
      }else{
        to_ret<-plot(KM$KM, main=paste0(input$event.from, "->", input$event.to),
                     xlab=input$UM,
                     ylab="p",
                     mark.time=TRUE)
      }

      return(to_ret)


    })

    output$surv.curve<-renderPlot({
      if(is.null(surv())){
        validate("Error: please check ")
      }else{
        surv()
      }
    })

    ##################################################################################################################################################################





  })










}


