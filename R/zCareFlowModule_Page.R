#'@title Careflow module: main page
#'
#'@rawNamespace import(shiny, except = c(renderDataTable,dataTableOutput))
#'@import dplyr
#'@import shinyWidgets
#'@importFrom DT renderDataTable dataTableOutput
#'@import pMineR
#'@import DiagrammeR
#'@import shinyjqui
#'@import survival
#'@import survminer
#'@importFrom shinyjs useShinyjs toggleState





server.careFlow <- function(input, output, session) {
  #visualizzazione EventLog
  tab <-
    callModule(import_data_server, "uploadEL", "EventLog")  #script Import_data_Fun




  # reactiveValues: initializing data as null data frame
  data_reactive <- reactiveValues(
    EventLog = data.frame(),
    depth = array(),
    max_depth = FALSE,
    support = array(),
    leaf = FALSE,
    median_time = FALSE,
    node.list = list(),
    strat.plot = c(),
    paths = list(),
    paths.to.plot = c(),
    tabs.node.end = list()
  )

  observe({
    toggleState("loadEL", tab$complete == TRUE)
  })




  #TAB EVENTLOG: data visualization of eventlog
  observeEvent(input$loadEL, {
    id.ind <- which(colnames(all.data[[1]]) == tab$id)
    date.ind <- which(colnames(all.data[[1]]) == tab$date)
    event.ind <- which(colnames(all.data[[1]]) == tab$event)

    data_reactive$EventLog <- all.data[["EventLog"]]

    tryCatch({
      if (is_empty(data_reactive$EventLog)) {
        data_reactive$EventLog <- data.frame()
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
      }
      else if (is.na(as.Date(all.data[[1]][, date.ind][1], "%Y-%m-%d"))) {
        sendSweetAlert(
          session = session,
          title = "Error",
          text = "Please check the Date Format",
          type = "primary"
        )
        data_reactive$EventLog <- data.frame()
      } else{
        colnames(all.data[[1]])[id.ind] <<- "ID"
        colnames(all.data[[1]])[date.ind] <<- "DATE_INI"
        colnames(all.data[[1]])[event.ind] <<- "EVENT"

        data_reactive$EventLog <- all.data[["EventLog"]]

        #check factors
        if (is.factor(data_reactive$EventLog$EVENT)) {
          data_reactive$EventLog$EVENT <-
            as.character(data_reactive$EventLog$EVENT)
        }

        # Creating Dl obj e CFM obj
        showModal(
          modalDialog(
            title = "Data loading may take a few moments",
            easyClose = TRUE,
            footer = NULL
          )
        )
        ObjDL <<- dataLoader(verbose.mode = FALSE)
        ObjDL$load.data.frame(
          mydata = data_reactive$EventLog ,
          IDName = "ID",
          EVENTName = "EVENT",
          dateColumnName = "DATE_INI",
          format.column.date = "%Y-%m-%d"
        )
        ObjCFM <<- careFlowMiner(verbose.mode = FALSE)
        ObjCFM$loadDataset(inputData = ObjDL$getData())
        data_reactive$node.list <-
          ObjCFM$getDataStructure()$lst.nodi
        removeModal()


        #################################################################### CARE FLOW OUTPUT TAB ################################################################################
        removeTab(inputId = "tabs", target = "CareFlowMiner")
        insertTab(
          inputId = "tabs",
          tabPanel(
            "CareFlowMiner",
            titlePanel("CareFlow Miner"),

            ##########################################  SIDE BAR (setting parametri CFM + CFM strat) ########################################################
            sidebarLayout(
              sidebarPanel(
                width = 3,
                fluidRow(column(9, p(
                  h3("Parameter Setting")
                )),
                column(
                  3,
                  br(),

                  dropdownButton(
                    p(h4("CareFlow Miner Section")),
                    br(),
                    p(
                      h5(
                        "This section presents the process model computed with the Care Flow Mining algorithm.
                        It is possible to change the parameters of the algorithm (support and depth) by acting on the specific inputs."
                      )
                    ),
                    br(),

                    p(
                      h5("Through the switches it is possible both to color the different nodes of the graph based on the median of the times to reach the node
                         and to show the leaf nodes even if the chosen depth is lower than the maximum depth"
                      )
                    ),

                    tags$hr(),

                    p(
                      h5(
                        "The inferential analysis presented is accomplished by considering the",
                        strong("number of patients passing through each node."),
                        "it is possible to compare the different sub-cohorts with respect to",
                        strong("time to arrive at the node"),
                        "and for the",
                        strong("probability of experiencing a given event of interest")
                      )
                    ),

                    circle = FALSE,
                    status = "info",
                    size = "xs",
                    icon = icon("fas fa-info"),
                    width = "300px",
                    right = TRUE,
                    tooltip = tooltipOptions(title = "Click to more info")
                  )
                )),

                #Riga1: profondità + switch x max depth
                fluidRow(
                  column(
                    7,
                    numericInput(
                      "depth",
                      label = "Select depth:",
                      value = 5,
                      min = 1
                    )
                  ),
                  column(
                    5,
                    br(),
                    br(),
                    materialSwitch(
                      inputId = "max_depth",
                      label = "max depth",
                      status = "primary",
                      right = TRUE
                    )
                  )
                ),

                #Riga 2: parametro supporto
                fluidRow(column(
                  12,
                  numericInput(
                    "support",
                    label = "Select support value:",
                    value = 10,
                    min = 1
                  )
                )),

                #Riga 3: visualizza media
                fluidRow(column(
                  12,
                  materialSwitch(
                    inputId = "time",
                    label = "Show median Time",
                    status = "default",
                    right = TRUE
                  )
                )),

                #Riga 4: parametro foglie out
                fluidRow(column(
                  12,
                  materialSwitch(
                    inputId = "leaf",
                    label = "Show far leaf",
                    status = "default",
                    right = TRUE
                  )
                )),

                tags$hr(),

                #Riga 5: CFM inferenziale
                fluidRow(column(
                  12,
                  materialSwitch(
                    inputId = "strat_CFM",
                    label = "Inferential Analysis",
                    status = "default",
                    right = TRUE
                  )
                )),



                ################################################################## side bar x STRATIFIED CFM ##############################################################################
                conditionalPanel(
                  "input.strat_CFM",
                  fluidRow(#stratification var
                    column(
                      6,
                      selectInput(
                        "strat.var",
                        label = "Select variable for the stratification:",
                        choices = colnames(data_reactive$EventLog)[!(colnames(data_reactive$EventLog) %in% c("ID", "DATE_INI", "EVENT"))],
                        selected = NULL
                      )
                    ),
                    #stratification var TYPE
                    column(
                      6,
                      selectInput(
                        "strat_var_type",
                        label = "Select the strat var type:",
                        choices = c("Categorical", "Numeric"),
                        selected = NULL
                      )
                    )),

                  # CARTEGORICAL
                  conditionalPanel(condition = "input.strat_var_type=='Categorical'",
                                   fluidRow(
                                     column(
                                       6,
                                       selectInput(
                                         "strat.value1",
                                         label = "Select possible value fot the selected var:",
                                         choices = NULL,
                                         multiple = FALSE
                                       )
                                     ),
                                     column(
                                       6,
                                       selectInput(
                                         "strat.value2",
                                         label = "Select possible value fot the selected var:",
                                         choices = NULL,
                                         multiple = TRUE
                                       )
                                     )
                                   )),
                  # NUMERIC
                  conditionalPanel(condition = "input.strat_var_type=='Numeric'",
                                   fluidRow(column(
                                     12,
                                     selectInput(
                                       "strat.rule",
                                       label = "Stratification threshold:",
                                       choices = c("median")
                                     )
                                   ))),


                  tags$hr(),

                  fluidRow(column(12,
                                  p(
                                    h4(strong("Compare for:"))
                                  ),
                                  br())),


                  fluidRow(
                    column(
                      6,
                      materialSwitch(
                        inputId = "strat.time",
                        label = "times",
                        status = "default",
                        right = TRUE
                      )
                    ),
                    column(
                      6,
                      materialSwitch(
                        inputId = "perc.end",
                        label = "future state",
                        status = "default",
                        right = TRUE
                      )
                    )
                  ),

                  fluidRow(column(6,),
                           column(
                             6,
                             selectInput(
                               "final.state",
                               label = "Future State:",
                               choices = unique(data_reactive$EventLog["EVENT"])
                             )
                           )),

                  tags$hr(),

                  fluidRow(column(8,),
                           column(
                             4, actionButton("refresh", "Refresh graph")
                           ))
                )
              ),


              mainPanel(
                conditionalPanel("input.strat_CFM",
                                 shinyjqui::jqui_resizable(grVizOutput("CF.strat"))),
                conditionalPanel("!input.strat_CFM",
                                 shinyjqui::jqui_resizable(grVizOutput(
                                   "CareFlowGraph"
                                 )))
              )
            )
          ),
          target = "Loading EventLog",
          position = "after"
        )



        ################################################################### PREDICTIVE PANEL ####################################################################
        removeTab(inputId = "tabs", target = "Probabilistic CareFlowMiner")
        insertTab(
          inputId = "tabs",
          tabPanel(
            "Probabilistic CareFlowMiner",
            titlePanel(
              "Process Discovery: CareFlowMiner - Probabilistic Model"
            ),
            br(),
            fluidRow(column(
              12,
              sidebarLayout(
                sidebarPanel(
                  width = 3,
                  p(h3("Parameter Setting")),
                  tags$hr(),
                  fluidRow(
                    column(
                      7,
                      numericInput(
                        "depth.pred",
                        label = "Select depth:",
                        value = 5,
                        min = 1
                      ),

                    ),
                    column(
                      5,
                      br(),
                      br(),
                      materialSwitch(
                        inputId = "max_depth.pred",
                        label = "max depth",
                        status = "primary",
                        right = TRUE
                      )
                    )
                  ),

                  tags$hr(),
                  #parametro supporto
                  fluidRow(column(
                    12,
                    numericInput("support.pred", label =
                                   "Select support value:", value = 10),

                  )),
                  #param outcome to predict
                  tags$hr(),

                  fluidRow(column(
                    12,
                    shiny::selectInput(
                      "pred.outcome",
                      label = "Select the outcome to predict",
                      choices = unique(data_reactive$EventLog["EVENT"])
                    )
                  )),

                  #param colori
                  p(
                    h5(
                      "It is possible to highlight with different colors events
                                           that may be more interesting. (Max 7 events)"
                    )
                  ),
                  fluidRow(column(
                    12,
                    shiny::selectInput(
                      "pred.outcome.col",
                      label = "Select event to highlight",
                      choices = unique(data_reactive$EventLog["EVENT"]),
                      selected = NULL,
                      multiple = TRUE
                    )
                  ))

                ),

                mainPanel(jqui_resizable(
                  grVizOutput("CareFlowGraph.pred")
                ))
              )
            ))

          ),
          target = "CareFlowMiner",
          position = "after"
        )


        ################################################################### PATH PANEL (KM + covariate) ###############################################################
        removeTab(inputId = "tabs", target = "Path Analysis")
        insertTab(
          inputId = "tabs",
          tabPanel(
            "Path Analysis",
            titlePanel("Process Discovery: Path Analysis"),
            fluidRow(column(
              12,
              sidebarLayout(
                sidebarPanel(
                  width = 4,
                  fluidRow(column(9, ),
                           column(
                             3,
                             dropdownButton(
                               tags$h4(strong(
                                 "Path Analysis"
                               )),

                               tags$h5("In this section it is possible to perform analysis on the pathways taken by patients.
                                       In the sidebar, it is possible to outline the paths of interest through the proposed inputs."),

                               tags$h5(strong("Survival Analysis with Kaplan Meier")),

                               tags$h5(
                                 "The first tab is dedicated to the survival analysis. The cohort consists of patients transiting through the node chosen as the start node,
                                                         which must be selected in the ",
                                 strong("\"node id start\" field."),
                                 "and who have experienced a certain state of interest,
                                                         which must be made explicit in the",
                                 strong("\"node id end\" field.")
                               ),

                               tags$h5(
                                 "Through the",
                                 strong("\"id node censored\" field") ,
                                 " it will be possible to indicate in which nodes the patients will have to transit
                                                 to in order to be considered censored."
                               ),


                               tags$h5(
                                 "using the input ",
                                 strong("\"use leaf as cens\""),
                                 " it will be possible to choose whether to follow the clinical follow-up of patients up to the last event they experienced"
                               ),

                               tags$h5(strong("Covariate analysis")),
                               tags$h5("In this second tab you can view the evolution of the selected covariate over time, for each path"),


                               circle = FALSE,
                               status = "info",
                               size = "xs",
                               icon = icon("fas fa-info"),
                               width = "300px",
                               right = TRUE,
                               tooltip = tooltipOptions(title = "Click to more info")
                             )
                           )),


                  fluidRow(tabsetPanel(
                    id = "path.tab",
                    tabPanel(
                      "Path 1",
                      path_mod_ui(
                        "path1",
                        tit = "Path 1" ,
                        is.fomm = FALSE,
                        node.list =
                          data_reactive$node.list,
                        el.data = data_reactive$EventLog,
                        is.strat.var = FALSE
                      )
                    )
                  )),

                  fluidRow(column(12, br())),

                  conditionalPanel(condition = "input.tabselected== 2 ",
                                   fluidRow(
                                     column(
                                       8,
                                       pickerInput(
                                         inputId = "covariate",
                                         label = "Select covariate",
                                         choices =
                                           colnames(data_reactive$EventLog)[!(colnames(data_reactive$EventLog) %in% c("ID", "DATE_INI", "EVENT"))],
                                         selected = NULL,
                                         multiple = FALSE,
                                         options = list(title = "select var")
                                       )
                                     ),
                                     column(4, br())
                                   )),


                  fluidRow(
                    column(4,
                           actionButton("add.path", "Add path")),
                    column(4,
                           actionButton("plot.all.surv", "Show Graphs")),
                    column(4,
                           actionButton("reset", "Reset path"))
                  )
                ),



                mainPanel(tabsetPanel(
                  id = "tabselected",
                  tabPanel(
                    "Surv",
                    value = 1,
                    fluidRow(
                      column(10, span(textOutput(
                        "error.mex"
                      ), style = "color:gray")),

                      column(
                        1,
                        dropdownButton(
                          fluidRow(column(
                            12,
                            selectInput(
                              inputId = "prev.cfm.type",
                              label = "select which type of CFM chart you want to inspect",
                              choices = c("CFM", "Stratified CFM", "Predictive CFM")
                            )
                          )),

                          fluidRow(column(12,
                                          grVizOutput("prev.cfm"))),


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

                      column(
                        1,
                        dropdownButton(
                          p(h4(strong(
                            "Select path to plot"
                          ))),
                          checkboxGroupInput(
                            "path.plot",
                            label = "",
                            choices = "path1",
                            selected = "path1"
                          ),
                          actionButton("render.km.graph", "Refresh graph"),


                          circle = FALSE,
                          status = "danger",
                          size = "xs",
                          icon = icon("cogs"),
                          width = "100px",
                          right = TRUE,
                          tooltip = tooltipOptions(title = "Click to more info")
                        )
                      )
                    ),


                    fluidRow(column(
                      12,
                      plotOutput("km.curves", width =  "100%")
                    )),

                    fluidRow(column(
                      12,
                      DT::dataTableOutput("logrank.res")
                    ))
                  ),


                  tabPanel(
                    "cov-time",
                    value = 2,

                    fluidRow(
                      column(10,
                             shiny::plotOutput("cov_time_graph")),
                      column(
                        1,
                        dropdownButton(
                          fluidRow(column(
                            12,
                            selectInput(
                              inputId = "prev.cfm.type.cov",
                              label = "select which type of CFM chart you want to inspect",
                              choices = c("CFM", "Stratified CFM", "Predictive CFM")
                            )
                          )),
                          fluidRow(column(12,
                                          grVizOutput(
                                            "prev.cfm.cov"
                                          ))),


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
                      column(
                        1,
                        dropdownButton(
                          p(h4(strong(
                            "Plot settings"
                          ))),
                          fluidRow(column(
                            6,
                            selectInput(
                              inputId = "UM.cov.plot",
                              label = "Select Time unit",
                              choices = c("mins", "days", "weeks", "months", "years"),
                              selected = "days"
                            )
                          ),
                          column(
                            6,
                            selectInput(
                              inputId = "legend.pos.cov",
                              label = "Set legend position",
                              choices = c(
                                "bottomright",
                                "bottom",
                                "bottomleft",
                                "left",
                                "topleft",
                                "top",
                                "topright",
                                "right",
                                "center"
                              ),
                              selected = "topleft"
                            )
                          )),
                          fluidRow(column(
                            6,
                            materialSwitch(
                              inputId = "reg.line",
                              label = "plot regression line",
                              status = "primary",
                              right = TRUE
                            )
                          )),

                          fluidRow(column(
                            6,
                            materialSwitch(
                              inputId = "mean.ci",
                              label = "plot mean and c.i.",
                              status = "primary",
                              right = TRUE
                            )
                          )),
                          conditionalPanel(condition = "input.mean.ci",
                                           fluidRow(column(
                                             6,
                                             numericInput(
                                               "delta.mean.ci",
                                               label = "Select time window:",
                                               value = 10,
                                               min = 5
                                             )
                                           ))),
                          # ),

                          # column(6,
                          #        numericInput("delta.mean.ci", label = "Select time window:", value = 10,min = 5))
                          # ),

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
                ))
              )
            ))
          ),
          target = "Probabilistic CareFlowMiner",
          position = "after"
        )
      }
    },
    error = function(e) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Please check the mapping: column selected as DATE seems to have no date format",
        type = "primary"
      )

    })


    observeEvent(input$add.node.end, {
      if (!is.null(input$node.start.cov)) {
        tabs <- list()
        for (i in c(1:length(input$node.start.cov))) {
          id.choices <- ObjCFM$findReacheableNodes(input$node.start.cov[i])[1, ]
          tabs[[i]] <- fluidRow(column(
            8,
            selectInput(
              inputId = paste0("id.end", i),
              label = paste(
                "select node end for node start:",
                input$node.start.cov[i]
              ),
              choices =  id.choices,
              multiple = TRUE,
              selected = NULL
            )
          ))
        }
        data_reactive$tabs.node.end <- tabs
      }

    })


    output$node.end.sel <- renderUI({
      tagList(data_reactive$tabs.node.end)
    })






    callModule(path_data_server,
               #Rfile: "path_module"
               "path1",
               data_reactive$EventLog,
               input$add.path + 1,
               is.fomm = FALSE)


    #ADD TAB PATH E AGGIORNO CHECKBOX PATH TO PLOT
    observeEvent(input$add.path, {
      insertTab(
        "path.tab",
        tabPanel(
          paste("Path", input$add.path + 1),
          path_mod_ui(
            paste0("path", input$add.path + 1),
            tit = paste("Path", input$add.path + 1),
            is.fomm = FALSE,
            node.list = data_reactive$node.list,
            el.data = data_reactive$EventLog,
            is.strat.var = FALSE
          )
        ),
        # path_mod_ui("path",tit = paste0("Path",input$add.path+1) ,node.list=data_reactive$node.list),
        target = paste("Path", input$add.path),
        position = "after",
        select = TRUE
      )





      callModule(
        path_data_server,
        session = session,
        paste0("path", input$add.path + 1),
        data_reactive$EventLog,
        input$add.path + 1,
        is.fomm = FALSE
      )


      paths.rm <- c()
      for (nomi.path in names(all.path)) {
        if (is.null(all.path[[nomi.path]][["id.end"]]))
          paths.rm <- c(paths.rm, nomi.path)
      }

      if (length(paths.rm) ==1) {
        all.path[[paths.rm]] <- NULL
      }else if(length(paths.rm)>1){
        for (i in c(1:length(paths.rm))) {
          all.path[[paths.rm[i]]]<-NULL
        }

      }

      data_reactive$paths <- all.path
      data_reactive$paths.rm<-paths.rm
      choices_list <- names(data_reactive$paths)


      updateCheckboxGroupInput(
        session,
        inputId = "path.plot",
        label = "",
        choices = choices_list ,
        selected = choices_list
      )

    })

    observeEvent(input$reset, {
      all.path <<- list()
      data_reactive$paths <<- all.path
      data_reactive$data_reactive$paths.to.plot <- "path1"
      choices_list <- "path1"
      updateCheckboxGroupInput(
        session,
        inputId = "path.plot",
        label = "",
        choices = choices_list ,
        selected = choices_list
      )
      output$km.curves <- renderPlot({
        NULL
      })
      output$logrank.res <- DT::renderDataTable({
        data.frame()
      })

      output$error.mex <- renderText("")

      removeTab(inputId = "tabs", target = "Path Analysis")
      insertTab(
        inputId = "tabs",
        tabPanel(
          "Path Analysis",
          titlePanel("Process Discovery: Path Analysis"),
          fluidRow(column(
            12,
            sidebarLayout(
              sidebarPanel(
                width = 4,
                fluidRow(column(9,),
                         column(
                           3,
                           dropdownButton(
                             tags$h4(strong(
                               "Survival Analysis with Kaplan Meier"
                             )),

                             tags$h5(
                               "The cohort consists of patients transiting through the node chosen as the start node,
                                                         which must be selected in the ",
                               strong("\"node id start\" field."),
                               "and who have experienced a certain state of interest,
                                                         which must be made explicit in the",
                               strong("\"node id end\" field.")
                             ),

                             tags$h5(
                               "Through the",
                               strong("\"id node censored\" field") ,
                               " it will be possible to indicate in which nodes the patients will have to transit
                                                 to in order to be considered censored."
                             ),


                             tags$h5(
                               "using the input ",
                               strong("\"use leaf as cens\""),
                               " it will be possible to choose whether to follow the clinical follow-up of patients up to the last event they experienced"
                             ),


                             circle = FALSE,
                             status = "info",
                             size = "xs",
                             icon = icon("fas fa-info"),
                             width = "300px",
                             right = TRUE,
                             tooltip = tooltipOptions(title = "Click to more info")
                           )
                         )),
                fluidRow(tabsetPanel(
                  id = "path.tab",
                  tabPanel(
                    "Path 1",
                    path_mod_ui(
                      "path1",
                      tit = "Path 1" ,
                      is.fomm = FALSE,
                      node.list =
                        data_reactive$node.list,
                      el.data = data_reactive$EventLog,
                      is.strat.var = FALSE
                    )
                  )
                )),
                fluidRow(column(12,
                                br())),
                conditionalPanel(condition = "input.tabselected== 2 ",
                                 fluidRow(
                                   column(
                                     8,
                                     pickerInput(
                                       inputId = "covariate",
                                       label = "Select covariate",
                                       choices =
                                         colnames(data_reactive$EventLog)[!(colnames(data_reactive$EventLog) %in% c("ID", "DATE_INI", "EVENT"))],
                                       selected = NULL,
                                       multiple = FALSE,
                                       options = list(title = "select var")
                                     )
                                   ),
                                   column(4, br())
                                 )),

                fluidRow(
                  column(4,
                         actionButton("add.path", "Add path")),
                  column(4,
                         actionButton("plot.all.surv", "Show Graphs")),
                  column(4,
                         actionButton("reset", "Reset path"))
                )

              ),
              mainPanel(tabsetPanel(
                tabPanel(
                  "Surv",
                  fluidRow(
                    column(10,

                           span(textOutput("error.mex"), style =
                                  "color:gray")),
                    column(
                      1,
                      dropdownButton(
                        fluidRow(column(
                          12,
                          selectInput(
                            inputId = "prev.cfm.type",
                            label = "select which type of CFM chart you want to inspect",
                            choices = c("CFM", "Stratified CFM", "Predictive CFM")
                          )
                        )),
                        fluidRow(column(12,
                                        grVizOutput("prev.cfm"))),


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
                    column(
                      1,
                      dropdownButton(
                        p(h4(strong(
                          "Select path to plot"
                        ))),
                        checkboxGroupInput(
                          "path.plot",
                          label = "",
                          choices = "path1",
                          selected = "path1"
                        ),
                        actionButton("render.km.graph", "Refresh graph"),


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
                  fluidRow(column(
                    12,
                    plotOutput("km.curves", width =  "100%")
                  )),
                  fluidRow(column(
                    12,
                    DT::dataTableOutput("logrank.res")
                  ))
                ),
                tabPanel("cov-time",

                         fluidRow(
                           column(10,
                                  shiny::plotOutput("cov_time_graph")),
                           column(
                             1,
                             dropdownButton(
                               fluidRow(column(
                                 12,
                                 selectInput(
                                   inputId = "prev.cfm.type.cov",
                                   label = "select which type of CFM chart you want to inspect",
                                   choices = c("CFM", "Stratified CFM", "Predictive CFM")
                                 )
                               )),
                               fluidRow(column(12,
                                               grVizOutput(
                                                 "prev.cfm.cov"
                                               ))),
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
                           column(
                             1,
                             dropdownButton(
                               p(h4(strong(
                                 "Plot settings"
                               ))),
                               fluidRow(column(
                                 6,
                                 selectInput(
                                   inputId = "UM.cov.plot",
                                   label = "Select Time unit",
                                   choices = c("mins", "days", "weeks", "months", "years"),
                                   selected = "days"
                                 )
                               ),
                               column(
                                 6,
                                 selectInput(
                                   inputId = "legend.pos.cov",
                                   label = "Set legend position",
                                   choices = c(
                                     "bottomright",
                                     "bottom",
                                     "bottomleft",
                                     "left",
                                     "topleft",
                                     "top",
                                     "topright",
                                     "right",
                                     "center"
                                   ),
                                   selected = "topleft"
                                 )
                               )),
                               fluidRow(column(
                                 6,
                                 materialSwitch(
                                   inputId = "reg.line",
                                   label = "plot regression line",
                                   status = "primary",
                                   right = TRUE
                                 )
                               )),
                               fluidRow(column(
                                 6,
                                 materialSwitch(
                                   inputId = "mean.ci",
                                   label = "plot mean and c.i.",
                                   status = "primary",
                                   right = TRUE
                                 )
                               ),
                               column(
                                 6,
                                 numericInput(
                                   "delta.mean.ci",
                                   label = "Select time window:",
                                   value = 10,
                                   min = 5
                                 ),
                               )),

                               circle = FALSE,
                               status = "danger",
                               size = "xs",
                               icon = icon("cogs"),
                               width = "400px",
                               right = TRUE,
                               tooltip = tooltipOptions(title = "Click to more info")
                             )
                           )
                         ))

              ))
            )
          ))

        ),
        target = "Probabilistic CareFlowMiner",
        position = "after",
        select = TRUE
      )


    })


    # #################################################################################################################################################################################



    #CREAZIONE DEL PLOT KM
    observeEvent(input$plot.all.surv, {
      paths.rm <- c()
      for (nomi.path in names(all.path)) {
        if (is.null(all.path[[nomi.path]][["id.end"]]))
          paths.rm <- c(paths.rm, nomi.path)
      }

      if (length(paths.rm) ==1) {
        all.path[[paths.rm]] <- NULL
      }else if(length(paths.rm)>1){
        for (i in c(1:length(paths.rm))) {
          all.path[[paths.rm[i]]]<-NULL
        }

      }

      data_reactive$paths <- all.path
      data_reactive$paths.rm<-paths.rm
      choices_list <- names(data_reactive$paths)


      updateCheckboxGroupInput(
        session,
        inputId = "path.plot",
        label = "",
        choices = choices_list ,
        selected = choices_list
      )

      data_reactive$paths.to.plot <- names(data_reactive$paths)



      if (length(data_reactive$paths) == 0) {
        sendSweetAlert(
          session = session,
          title = "Error",
          text = "At least one path must be entered",
          type = "primary"
        )

      } else{
        fun.out <-
          render.km.graph(data_reactive$paths, data_reactive$paths.to.plot)   #script: render_km_graph


        output$error.mex <- renderText({
          if (length(data_reactive$paths.rm) == 0) {
            " "

          } else{
            paste("Path",
                  data_reactive$paths.rm,
                  "not shown: please check values entred")
          }
        })

        output$km.curves <- renderPlot({
          if (length(fun.out) == 3) {
            survminer::ggsurvplot(
              fun.out$final.surv,
              data = fun.out$final.data,
              conf.int = TRUE,
              # Add confidence interval
              risk.table = TRUE,
              # Add risk table
              risk.table.height = 0.27,
              risk.table.col = "strata"# Risk table color by groups
            )
          } else{

          }
        })



        output$logrank.res <- DT::renderDataTable({
          p(h4(strong(
            "Results of Logrank test on Paths"
          )))
          if (length(all.path) > 1) {
            logrank_fun(fun.out, data_reactive$paths)

          } else{
            data.frame()
          }
        })

      }

      if (is.null(input$covariate) || input$covariate == "") {

      } else{
        arr.from <- c()
        for (i in c(1:length(all.path))) {
          arr.from[i] <- all.path[[i]]$id.start
        }
        arr.from <- as.character(unique(arr.from))

        # arr.from<-do.call("cbind",do.call("rbind",all.path)[,1])
        lst.to.tmp <- do.call("rbind", all.path)[, 2]
        names(lst.to.tmp) <- do.call("rbind", all.path)[, 1]
        lst.to <-
          tapply(unlist(lst.to.tmp, use.names = FALSE),
                 rep(names(lst.to.tmp), lengths(lst.to.tmp)),
                 FUN = c)


        #SERIE DI CONTROLLI CHE VANNO SISTEMATI:
        #sto assumendo che se length(unique(data_reactive$EventLog[,input$covariate]))<7 allora la cov che sto considerando è categorica
        if (is.null(arr.from) | is.null(lst.to[[1]])) {
          sendSweetAlert(
            session = session,
            title = "Error",
            text = "Please enter values for id node start and/or id node end",
            type = "primary"
          )
          output$cov_time_graph <- shiny::renderPlot({

          })
        } else{
          output$cov_time_graph <- shiny::renderPlot({
            if (length(unique(data_reactive$EventLog[, input$covariate])) < 10) {
              sendSweetAlert(
                session = session,
                title = "Error",
                text = "Please select numerical variables as covariate",
                type = "primary"
              )

            } else{
              cov_time_fun(
                ObjDL,
                ObjCFM,
                input$covariate,
                arr.from = arr.from,
                lst.to,
                covariate.type = 'attribute',
                plot.ci.mean = input$mean.ci,
                delta = input$delta.mean.ci,


                # is.numerical=input$is.numerical,
                points.symbols = 20,
                plot.RegressionLine = input$reg.line,
                legend.position = input$legend.pos.cov,
                UM = input$UM.cov.plot,
                size.symbols = 1.5,
                line.width = 2,
                y.int.legend = 0.8,
                legend.text.size = 0.8
              )
            }

          })
        }
      }
    })




    #RENDER DEL GRAFICO KM IN CADO SI DESELEZIONE DEI PATH
    observeEvent(input$render.km.graph, {
      data_reactive$paths <- all.path
      if (length(data_reactive$paths) < 1) {
        sendSweetAlert(
          session = session,
          title = "Error",
          text = "Every time you create a new path please save it using the proper button",
          type = "primary"
        )
      } else{
        if (is.null(input$path.plot)) {
          fun.out <- list("id.not.valid" = c())
        } else{
          fun.out <- render.km.graph(data_reactive$paths, input$path.plot)
        }

        output$error.mex <- renderText({
          if (length(fun.out) == 3 & !is.null(fun.out$id.not.valid)) {
            paste("Path",
                  fun.out$id.not.valid,
                  "not shown: please check values entred")
            # paste("please check values entered for path:",fun.out)
          } else if (length(fun.out) == 1 &
                     !is.null(fun.out$id.not.valid)) {
            paste("please check values entered for path:", fun.out)
          } else{
            ""
          }
        })



        output$km.curves <- renderPlot({
          if (length(fun.out) == 3) {
            ggsurvplot(
              fun.out$final.surv,
              data = fun.out$final.data,
              conf.int = TRUE,
              # Add confidence interval
              risk.table = TRUE,
              # Add risk table
              risk.table.height = 0.27,
              risk.table.col = "strata"# Risk table color by groups
            )
          } else{

          }
        })
      }


    })


    observeEvent(input$path.plot, {
      data_reactive$paths.to.plot <- input$path.plot
    })

    # ####################################  RENDER OUTPUT CFM TAB #################################################
    observeEvent(input$depth, {
      if (is.na(input$depth)) {
        data_reactive$depth <- 1
      } else{
        data_reactive$depth <- input$depth
      }
      data_reactive$strat.plot <- strat_CFMgraph_fun()
    })

    observeEvent(input$max_depth, {
      data_reactive$strat.plot <- strat_CFMgraph_fun()
    })

    observeEvent(input$support, {
      data_reactive$strat.plot <- strat_CFMgraph_fun()
    })

    # GRAFICO CFM CLASSICO
    CFgraph <- reactive({
      if (input$max_depth) {
        dp <- Inf
      } else{
        dp <- input$depth
      }

      #  SETTING DEGLI INTERVALLI DI COLORE PER IL MEDIAN TIME
      if (input$time) {
        cf.graph <- ObjCFM$plotCFGraph(
          depth = dp,
          #PROFONDITA
          abs.threshold = input$support,
          #support
          kindOfGraph = "dot",
          nodeShape = "square"
        )
        len <- length(cf.graph$arr.nodi)
        id.nodi <- array()

        for (i in c(1:len)) {
          id.nodi[i] <- strsplit(cf.graph$arr.nodi[i], "'")[[1]][2]
        }

        id.nodi <- as.numeric(id.nodi[2:length(id.nodi)])

        lst.nodi <- ObjCFM$getDataStructure()$lst.nodi
        loadedDataset <- ObjDL$getData()
        median.time <- array()

        for (i in c(1:length(id.nodi))) {
          son <- as.character(id.nodi[i])
          tmp.tempi <-
            unlist(lapply(lst.nodi[[son]]$IPP, function(tmpIPP)
            {
              loadedDataset$pat.process[[tmpIPP]][lst.nodi[[son]]$depth, "pMineR.deltaDate"]
            }))
          if (length(tmp.tempi) > 0) {
            tmp.tempi <-
              as.numeric(unlist(lapply(tmp.tempi, function(x) {
                format((x / (24 * 60)), digits = 3)
              })))
            med <- median(tmp.tempi)
          } else{
            tmp.tempi <- NA
            med <- NA
          }
          median.time[i] <- med
        }

        time.unique <- unique(median.time[order(median.time)])
        cut.off <- floor(length(time.unique) / 4)
        col.threshold <-
          c(time.unique[cut.off], time.unique[cut.off * 2], time.unique[cut.off * 3])
      } else{
        col.threshold <- c()
      }


      cf.graph <- ObjCFM$plotCFGraph(
        depth = dp,
        #PROFONDITA
        withPercentages = TRUE,
        relative.percentages = TRUE,
        show.far.leaf = input$leaf,
        #leaf
        show.median.time.from.root = input$time,
        #time
        heatmap.based.on.median.time = col.threshold,
        abs.threshold = input$support,
        #support
        kindOfGraph = "dot",
        nodeShape = "square"
      )$script



      return(cf.graph)
    })

    output$CareFlowGraph <- renderGrViz({
      grViz(CFgraph())
    })


    # ##############################  CFM STRATIFICATO ################################################
    observeEvent(input$strat.var, {
      if(input$strat.var!=""){
        shiny::updateSelectInput(
          inputId = "strat.value1",
          label = "Select possible value fot the selected var:",
          choices = unique(data_reactive$EventLog[input$strat.var]),
          selected = NULL
        )

        shiny::updateSelectInput(
          inputId = "strat.value2",
          label = "Select possible value fot the selected var:",
          choices = unique(data_reactive$EventLog[input$strat.var]),
          selected = NULL
        )

      }

    })

    observeEvent(input$strat.value1, {
      if(input$strat.value1!=""){
        shiny::updateSelectInput(
          inputId = "strat.value2",
          label = "Select possible value fot the selected var:",
          choices = unique(data_reactive$EventLog[input$strat.var])[!unique(data_reactive$EventLog[input$strat.var]) %in% input$strat.value1],
          selected = NULL
        )
      }


    })

    toListen <- reactive({
      list("strat.time"   = input$strat.time,
           "perc.end"     = input$perc.end,
           "refresh"    = input$refresh,
           "final.state"  = input$final.state)
    })

    observeEvent(toListen(),{
      data_reactive$strat.plot <- strat_CFMgraph_fun()
    },ignoreInit = TRUE)

    observeEvent(input$perc.end,{
      data_reactive$strat.plot <- strat_CFMgraph_fun()
    })

    observeEvent(input$refresh, {
      data_reactive$strat.plot <- strat_CFMgraph_fun()
    })

    strat_CFMgraph_fun <- reactive({
      if (input$max_depth) {
        dp <- Inf
      } else{
        dp <- input$depth
      }

      if (input$strat_var_type == "Categorical") {
        if (length(input$strat.value2) > 1) {
          script <- ObjCFM$plotCFGraphComparison(
            stratifyFor = input$strat.var,
            arr.stratificationValues.A = input$strat.value1,
            arr.stratificationValues.B = input$strat.value2,
            depth = dp,
            abs.threshold = input$support,
            checkDurationFromRoot = input$strat.time,
            hitsMeansReachAGivenFinalState = input$perc.end,
            finalStateForHits = input$final.state ,
            kindOfGraph = "dot",
            nodeShape = "square"
          )$script

        } else{
          script <- ObjCFM$plotCFGraphComparison(
            stratifyFor = input$strat.var,
            stratificationValues = c(input$strat.value1, input$strat.value2),
            depth = dp,
            abs.threshold = input$support,
            checkDurationFromRoot = input$strat.time,
            hitsMeansReachAGivenFinalState = input$perc.end,
            finalStateForHits = input$final.state ,
            kindOfGraph = "dot",
            nodeShape = "square"
          )$script
        }

      } else{
        thr.rule <- input$strat.rule

        switch (input$strat.rule,
                "median" = {
                  strat.value <-
                    median(as.numeric(data_reactive$EventLog[, input$strat.var]), na.rm = T)
                })




        script <-
          ObjCFM$plotCFGraphComparison(
            stratifyFor = input$strat.var,
            stratificationThreshold = strat.value,
            depth = dp,
            abs.threshold = input$support,
            checkDurationFromRoot = input$strat.time,
            hitsMeansReachAGivenFinalState = input$perc.end,
            finalStateForHits = input$final.state ,
            kindOfGraph = "dot",
            nodeShape = "square"
          )$script

      }
      return(script)
    })


    output$CF.strat <- renderGrViz({
      if (is.null(data_reactive$strat.plot)) {
        validate("please click the refresh button")
      } else{
        grViz(data_reactive$strat.plot)
      }
    })

    ##############################################################################################################




    # ######################################################### CFM RAPPRESENTAZIONI X PREV (messe nei pop up del modulo path analysis) ############################################
    output$prev.cfm <- renderGrViz({
      cfm.type <- input$prev.cfm.type
      switch (
        cfm.type,
        "CFM" = {
          grViz(CFgraph())
        },
        "Stratified CFM" = {
          if (is.null(data_reactive$strat.plot)) {
            validate(
              "To view this representation, it is first necessary to set the necessary parameters in the \"Care Flow Miner\" section"
            )
          } else{
            grViz(data_reactive$strat.plot)
          }
        },
        "Predictive CFM" = {
          grViz(CFgraph.pred())
        }
      )

    })

    output$prev.cfm.cov <- renderGrViz({
      cfm.type <- input$prev.cfm.type.cov
      switch (
        cfm.type,
        "CFM" = {
          grViz(CFgraph())
        },
        "Stratified CFM" = {
          if (is.null(data_reactive$strat.plot)) {
            validate(
              "To view this representation, it is first necessary to set the necessary parameters in the \"Care Flow Miner\" section"
            )
          } else{
            grViz(data_reactive$strat.plot)
          }
        },
        "Predictive CFM" = {
          grViz(CFgraph.pred())
        }
      )

    })

    ########################################################### #PLOT CAREFLOW PREDITTIVO ##################################################################

    CFgraph.pred <- reactive({
      if (input$max_depth.pred) {
        dp <- Inf
      } else{
        dp <- input$depth.pred
      }

      shades <-
        c(
          "Red",
          "LightGoldenrodYellow",
          "Lavender",
          "LightCyan",
          "LightSalmon",
          "SandyBrown",
          "	LightYellow",
          "LightGreen"
        )


      if (is.null(input$pred.outcome.col)) {
        sub.shades <- shades[1]
        names(sub.shades) <- input$pred.outcome

      } else{
        len <- length(input$pred.outcome.col) + 1
        sub.shades <- shades[1:len]
        names <- c(input$pred.outcome, input$pred.outcome.col)
        names(sub.shades) <- names
      }


      graph <- ObjCFM$plotCFGraph(
        depth = dp,
        predictive.model = TRUE,
        predictive.model.outcome = input$pred.outcome,
        arr.States.color = sub.shades,
        abs.threshold = input$support.pred,
        kindOfGraph = "dot",
        nodeShape = "square"
      )$script


      return(graph)
    })

    output$CareFlowGraph.pred <- renderGrViz({
      grViz(CFgraph.pred())
    })

  })
}
