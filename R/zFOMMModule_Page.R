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





server.FOMM<-function(input,output,session){
  #visualizzazione EventLog
  tab<-callModule(import_data_server,"uploadEL","EventLog")



  # reactiveValues: initializing data as null data frame
  data_reactive <- reactiveValues(
    EventLog=data.frame(),
    th = c(),  #threshold
    al = c(),  #autoloops
    FOMM = c()
  )


  #TAB EVENTLOG: data visualization of eventlog
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
    }else if(length(which(colnames(all.data[[1]]) %in% c("ID","DATE_INI","EVENT")))<3){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "It is necessary to explicit which columns of the uploaded Event Log contain information about: ID, DATE and EVENT label ",
        type = "primary"
      )
      data_reactive$EventLog<-data.frame()

    }else if(is.na(as.Date(all.data[[1]]$DATE_INI[1], "%Y-%m-%d"))){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Please check the Date Format",
        type = "primary"
      )
      data_reactive$EventLog<-data.frame()
    }else{
      # Creating Dl obj e CFM obj

      if(is.factor(data_reactive$EventLog$EVENT)) { data_reactive$EventLog$EVENT <- as.character(data_reactive$EventLog$EVENT)  }
      showModal(modalDialog(title = "Data loading may take a few moments",
                            easyClose = TRUE, footer=NULL))
      ObjDL<<-dataLoader(verbose.mode = FALSE)
      ObjDL$load.data.frame(mydata =data_reactive$EventLog ,IDName = "ID",EVENTName = "EVENT",dateColumnName = "DATE_INI",
                            format.column.date = "%Y-%m-%d")
      removeModal()
      globalList <- reactiveValues(ManyChoices = unique(all.data[[1]]$EVENT), SelectedPrev = c())



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
                                                 right = TRUE
                                               )
                                        )
                                      )
                                    ),

                                    mainPanel(
                                      jqui_resizable(grVizOutput("fomm.Graph"))
                                    )
                                  )
                           )
                         )

                ),
                target = "Loading EventLog",
                position = "after"
      )

      removeTab(inputId = "tabs", target = "Survival Analysis")
      insertTab(inputId = "tabs",
                tabPanel("Survival Analysis",
                         titlePanel("Process Discovery: Survival Analysis"),
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

                                      fluidRow(
                                        column(4,
                                               actionButton("add.path","Add path")
                                        ),
                                        column(4,
                                               actionButton("plot.all.surv","Show Kaplan Meier")
                                        ),
                                        column(4,
                                               actionButton("reset","Reset path")
                                        )

                                      )

                                    ),
                                    mainPanel(

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
                                      ), height = "100%"
                                    )
                                  )
                           )
                         )

                ),
                target = "FOMM",
                position = "after"
      )

      ################################################################### PLOT COV NODE PANEL ###############################################################
      removeTab(inputId = "tabs", target = "Covariate Visualization")
      insertTab(inputId = "tabs",
                tabPanel("Covariate Visualization",
                         titlePanel("FOMM event-node Descriptive: Covariate Visualization"),
                         fluidRow(
                           column(12,
                                  sidebarLayout(
                                    sidebarPanel(
                                      fluidRow(
                                        column(8,
                                               pickerInput(inputId = "covariate",
                                                           label = "Select covariate",
                                                           choices =colnames(data_reactive$EventLog)[!(colnames(data_reactive$EventLog) %in% c("ID","DATE_INI","EVENT"))],
                                                           selected = NULL,
                                                           multiple = FALSE,
                                                           options = list(
                                                             title = "select var"))
                                        ),
                                        column(4,
                                               br(),

                                        )
                                      ),
                                      fluidRow(
                                        column(9,
                                               selectizeInput("node.start.cov",
                                                              label = "Select node start",
                                                              choices = c(" ", globalList$ManyChoices),
                                                              selected = " ",
                                                              multiple = TRUE,
                                                              options = list(closeAfterSelect = TRUE, openOnFocus = TRUE))
                                               # selectInput(inputId = "node.start.cov",
                                               #             label = "Select node start",
                                               #             choices =unique(data_reactive$EventLog$EVENT),
                                               #             selected = NULL,
                                               #             multiple = TRUE)
                                        ),
                                        column(3,
                                               br(),
                                               actionButton("add.node.end","add node end",width = "100%")
                                        )
                                      ),
                                      tags$hr(),
                                      fluidRow(
                                        column(12,
                                               uiOutput("node.end.sel")
                                        )
                                      ),
                                      fluidRow(
                                        column(3,offset = 9,
                                               br(),
                                               actionButton("plot.cov.graph","plot graph",width = "100%")
                                        )

                                      )

                                    ),
                                    mainPanel(
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

                ),
                target = "Survival Analysis",
                position = "after"
      )}

    observeEvent(input$add.node.end,{
      if(!is.null(input$node.start.cov)){
        tabs<-list()
        arr.from<-input$node.start.cov[2:length(input$node.start.cov)]
        for (i in c(1:length(arr.from))) {
          id.choices<-unique(data_reactive$EventLog$EVENT)


          tabs[[i]]<-fluidRow(
            column(5,
                   selectInput(inputId = paste0("id.end",i),
                               label = paste("select node end for node start:",arr.from[i]),
                               choices =  id.choices,
                               multiple = TRUE,
                               selected = NULL)
            ),
            column(3,
                   selectInput(inputId = paste0("pass",i),
                               label = paste("Passing through:"),
                               choices =  id.choices,
                               multiple = TRUE,
                               selected = NULL)
                   ),
            column(4,
                   selectInput(inputId = paste0("not.pass",i),
                               label = paste("Not passing through:"),
                               choices =  id.choices,
                               multiple = TRUE,
                               selected = NULL)
                   )

          )
        }
        data_reactive$tabs.node.end<-tabs
      }
    })


    observeEvent(input$node.start.cov, {

      # if sth was added
      if(length(input$node.start.cov) > length(globalList$SelectedPrev)) {
        #find out what was modified
        vDiff <- setdiff(input$node.start.cov, globalList$SelectedPrev) %>% setdiff(., " ")
        # used when removing " " and selecting sth to double the selection
        if(length(vDiff) == 0) vDiff <- input$node.start.cov[length(input$node.start.cov)]
        req(input$node.start.cov != " ") # if only " " is selected then there is no need to update
        # get the position of selected element
        vIndex <- which(globalList$ManyChoices == vDiff)
        vLength <- length(globalList$ManyChoices)
        # create new choices in the correct order
        globalList$ManyChoices <- c(globalList$ManyChoices[1:vIndex],
                                    paste0(vDiff, " "),
                                    if(vIndex < vLength) {globalList$ManyChoices[(vIndex + 1):vLength]})
      } else {
        # remove the version with additional space when value was removed
        vDiff <- setdiff(globalList$SelectedPrev, input$node.start.cov)
        globalList$ManyChoices <- setdiff(globalList$ManyChoices, paste0(vDiff, " "))
      }

      # update previous selection
      globalList$SelectedPrev <- input$node.start.cov

      # update input with same selection but modified choices
      updateSelectizeInput(session = session,
                           inputId = "node.start.cov",
                           selected = c(" ", input$node.start.cov),
                           choices = c(" ", globalList$ManyChoices))
    })





    output$node.end.sel<-renderUI({
      tagList(
        data_reactive$tabs.node.end
        # do.call(fluidPage,data_reactive$tabs.node.end)
      )
    })

    observeEvent(input$plot.cov.graph,{
      arr.from<-array()
      for (i in c(1:length(input$node.start.cov)-1)) {
        if(endsWith(input$node.start.cov[i+1]," ")){
          arr.from.string<-strsplit(input$node.start.cov[i+1],split = " ")
          to.paste<-which(arr.from.string[[1]]!="")
          arr.from[i]<-paste(arr.from.string[[1]][to.paste],collapse = "_")


          # arr.from[i]<-substr(input$node.start.cov[i+1],start = 1,stop = nchar(input$node.start.cov[i+1])-1)
        }else{
          arr.from[i]<-input$node.start.cov[i+1]
        }

      }
      # arr.from<-input$node.start.cov[2:length(input$node.start.cov)]
      lst.to<-lapply(1:length(arr.from),function(i){
        path.name<-paste0("id.end",i)
        return(input[[path.name]])
      })
      pass<-lapply(1:length(arr.from),function(i){
        pass.val<-paste0("pass",i)
        if(!is.null(input[[pass.val]])){
         to_ret<-input[[pass.val]]
        }else{
          to_ret<-""
        }
        return(to_ret)
      })

      not.pass<-lapply(1:length( arr.from),function(i){
        not.pass.val<-paste0("not.pass",i)
        if(!is.null(input[[not.pass.val]])){
          to_ret<-input[[not.pass.val]]
        }else{
          to_ret<-""
        }


        return(to_ret)
      })






      #SERIE DI CONTROLLI CHE VANNO SISTEMATI:
      #sto assumendo che se length(unique(data_reactive$EventLog[,input$covariate]))<7 allora la cov che sto considerando è categorica
      if(is.null(arr.from) |is.null(lst.to[[1]])){
        sendSweetAlert(
          session = session,
          title = "Error",
          text = "Please enter values for id node start and/or id node end",
          type = "primary"
        )
        output$cov_time_graph<- shiny::renderPlot({


        })
      }else{
        output$cov_time_graph<- shiny::renderPlot({
          if(length(unique(data_reactive$EventLog[,input$covariate]))<10){
            sendSweetAlert(
              session = session,
              title = "Error",
              text = "Please select numerical variables as covariate",
              type = "primary"
            )

          }else{

            df_tot<-pre_fun_fomm(ObjDL = ObjDL,
                                 FOMM = FOMM,
                                 arr.from = arr.from,
                                 lst.to = lst.to,
                                 covariate = input$covariate,
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
                             legend.text.size=0.8)

            }else{

            }




          }

        })
      }
    })


    callModule(path_data_server,
               "path1",
               data_reactive$EventLog,
               input$add.path+1,
               is.fomm= TRUE)


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

      removeTab(inputId = "tabs", target = "Survival Analysis")
      insertTab(inputId = "tabs",
                tabPanel("Survival Analysis",
                         titlePanel("Process Discovery: Survival Analysis"),
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

                                      fluidRow(
                                        column(4,
                                               actionButton("add.path","Add path")
                                        ),
                                        column(4,
                                               actionButton("plot.all.surv","Show Kaplan Meier")
                                        ),
                                        column(4,
                                               actionButton("reset","Reset path")
                                        )
                                      )

                                    ),
                                    mainPanel(

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
                                      ), height = "100%"
                                    )
                                  )
                           )
                         )

                ),
                target = "FOMM",
                position = "after",select = TRUE
                )



    })


    #ADD TAB PATH E AGGIORNO CHECKBOX PATH TO PLOT
    observeEvent(input$add.path,{

      insertTab("path.tab",
                tabPanel(paste("Path",input$add.path+1),
                         path_mod_ui(paste0("path",input$add.path+1),
                                     tit = paste("Path",input$add.path+1),
                                     is.fomm= TRUE,
                                     node.list=data_reactive$node.list,
                                     el.data = data_reactive$EventLog,
                                     is.strat.var=FALSE
                         )
                ),
                # path_mod_ui("path",tit = paste0("Path",input$add.path+1) ,node.list=data_reactive$node.list),
                target = paste("Path", input$add.path),
                position = "after",select = TRUE
      )





      callModule(path_data_server,session = session,
                 paste0("path",input$add.path+1),
                 data_reactive$EventLog,
                 input$add.path+1,
                 is.fomm= TRUE)

      data_reactive$paths<-all.path
      choices_list<-names(all.path)

      # choices_list<-unlist(lapply(0:length(all.path)+1, function(path.num){ paste("path",path.num)}))




      updateCheckboxGroupInput(session, inputId = "path.plot",
                               label = "",
                               choices = choices_list ,
                               selected =choices_list)

    })

    #CREAZIONE DEL PLOT KM
    observeEvent(input$plot.all.surv,{
      data_reactive$paths<-all.path

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
          text = "Every time you create a new path please save it using the proper button",
          type = "primary"
        )
      }else{

        fun.out<-render.km.graph.FOMM(data_reactive$paths,data_reactive$paths.to.plot)


        output$error.mex<-renderText({
          #caso in cui non ho trovato
          if(length(fun.out)!=3){
            paste("please check values entered for path:",fun.out)
          }else if(!is.null(fun.out$id.not.valid)){
            paste("Path",fun.out$id.not.valid, "not shown: please check values entred" )
          }else{
            ""
          }
        })

        output$km.curves<-renderPlot({
          if(length(fun.out)==3){
            survminer::ggsurvplot(fun.out$final.surv,
                       data = fun.out$final.data,
                       conf.int = TRUE,          # Add confidence interval
                       risk.table = TRUE,        # Add risk table
                       risk.table.height = 0.27,
                       risk.table.col = "strata"# Risk table color by groups
            )
          }else{

          }
        })

        output$logrank.res<- DT::renderDataTable({
          p(h4(strong("Results of Logrank test on Paths")))
          if(length(all.path)>1){
            logrank_fun(fun.out)
          }else{
            data.frame()
          }

        })

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



        # output$error.mex<-renderText({
        #   if(length(fun.out)!=3){
        #     paste("please check values entered for path:",fun.out)
        #   }else if(!is.null(fun.out$id.not.valid)){
        #     paste("Path",fun.out$id.not.valid, "not shown: please check values entred" )
        #   }else{
        #     ""
        #   }
        # })

        output$km.curves<-renderPlot({
          if(length(fun.out)==3){
            ggsurvplot(fun.out$final.surv,
                       data = fun.out$final.data,
                       conf.int = TRUE,          # Add confidence interval
                       risk.table = TRUE,        # Add risk table
                       risk.table.height = 0.27,
                       risk.table.col = "strata"# Risk table color by groups
            )
          }else{

          }
        })
      }


    })


    observeEvent(input$th,{
      data_reactive$th<-input$th
    })

    observeEvent(input$al,{
      data_reactive$al<-input$al
    })


    fomm.graph<-reactive({
      param= list("threshold"=data_reactive$th, "considerAutoLoop"= data_reactive$al)
      FOMM<-firstOrderMarkovModel(parameters.list = param)
      FOMM$loadDataset(dataList = ObjDL$getData())
      FOMM$trainModel()
      data_reactive$FOMM<-FOMM
      fomm.plot<-FOMM$getModel(kindOfOutput = "grViz")
      return(fomm.plot)
    })

    output$fomm.Graph<-renderGrViz({
      grViz(fomm.graph())
    })

    output$prev.fomm.cov<-renderGrViz({
      grViz(fomm.graph())
    })


    #################################################### SURVIVAL ANALYSIS TAB ##############################################################################



    output$prev.fomm<-renderGrViz({
      grViz(fomm.graph())
    })


    # observeEvent(input$event.from,{
    #   updateSelectInput(session = session,
    #                     inputId = "PDVat",
    #                     label = "Censored at:",
    #                     choices = unique(all.data[[1]]$EVENT)[!unique(all.data[[1]]$EVENT) %in% c(input$event.from)],
    #                     selected = NULL
    #
    #                     )
    #
    #   updateSelectInput(session = session,
    #                     inputId = "event.to",
    #                     label = "To State: ",
    #                     choices = unique(all.data[[1]]$EVENT)[!unique(all.data[[1]]$EVENT) %in% c(input$event.from)],
    #                     selected = NULL
    #
    #   )
    # })



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


