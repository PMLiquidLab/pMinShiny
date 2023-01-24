#'@title descriptive module: main page
#'
#'
#'@import rlang
#'@import shiny
#'@import dplyr
#'@import shinyWidgets
#'@import DT
#'@import kableExtra
#'@import timevis
#'@import pMineR




# library(rlang)
# library(shiny)
# library(shinythemes)
# library(dplyr)
# library(shinyWidgets)
# library(DT)


# ui.descr<-tagList(
#   fluidPage(
#     # tags$head(
#     #   tags$style(HTML("
#     #   .shiny-output-error-validation {
#     #     color: red;
#     #   }
#     # "))
#     # ),
#
#     #Pagina Principale
#     navbarPage("pMining: EventLog Visual Analysis", id="tabs",
#                tabPanel("Loading Data",
#                         titlePanel("Data Uploading"),
#                         br(),
#                         import_mod_ui("uploadEL","Upload EventLog file",FALSE),
#                         actionButton("loadEL","Load Event Log",width = '32%') ,
#                )
#     )
#   )
# )

server.descr<-function(input,output,session){
  #visualizzazione EventLog
  tab<-callModule(import_data_server_visual,"uploadEL","EventLog")


  # reactiveValues: initializing data as null data frame
  data_reactive <- reactiveValues(
    EventLog=data.frame(),
    ancillaryData=list(),
    tabs=list(),
    event_delete=array(),
    event_density=array(),
    date_range=array(),
    ev.start=array(),
    ev.end= array(),
    ev.bet=array()
  )


  observeEvent(input$eventList,{
    if(length(data_reactive$event_delete)<length(input$eventList)){
      objDL.new <<- dataLoader(verbose.mode = FALSE)
      objDL.new$load.data.frame(mydata =data_reactive$EventLog ,IDName = "ID",EVENTName = "EVENT",dateColumnName = "DATE_INI",
                                format.column.date = "%Y-%m-%d")
    }
    data_reactive$event_delete<-input$eventList

  })

  observeEvent(input$eventdensityList,{
    data_reactive$event_density<-input$eventdensityList
  })





  #TAB EVENTLOG: data visualization of eventlog
  observeEvent(input$loadEL,{
    # null.col<-array(dim = nrow(all.data[["EventLog"]]))
    df<-cbind(all.data[["EventLog"]])
    data_reactive$EventLog<-df
    # data_reactive$EventLog <- select(df,"ID","DATE_INI","DATE_END","EVENT") df[, c("ID","DATE_INI","DATE_END","EVENT")]
    #


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
      data_reactive$EventLog <- df[, c("ID","DATE_INI","DATE_END","EVENT")]
      if(is.factor(data_reactive$EventLog$EVENT)) { data_reactive$EventLog$EVENT <- as.character(data_reactive$EventLog$EVENT)  }
      data_reactive$event_delete<-unique(data_reactive$EventLog$EVENT)

      objDL.new <<- dataLoader(verbose.mode = FALSE)
      objDL.new$load.data.frame(mydata =data_reactive$EventLog ,IDName = "ID",EVENTName = "EVENT",dateColumnName = "DATE_INI",
                                format.column.date = "%Y-%m-%d")
      objDL.out<<-objDL.new$getData()



      objQOD <<- QOD()
      objQOD$loadDataset(dataList = objDL.out)





    #loading data uploaded in the upload section
    removeTab(inputId = "tabs", target = "EventLog data analysis")
    if(!is_empty(data_reactive$EventLog)){
      insertTab(inputId = "tabs",
                tabPanel("EventLog data analysis",
                         titlePanel("Descriptive Data Analysis"),
                         br(),
                         fluidRow(
                           column(12,
                                  sidebarLayout(
                                    sidebarPanel(
                                      p(h4(strong("Select variable to plot"))),
                                      awesomeCheckboxGroup("eventList","",
                                                           choices = unique(data_reactive$EventLog[,"EVENT"]),
                                                           selected =unique(data_reactive$EventLog[,"EVENT"]),
                                                           status = "#primary"
                                      )
                                    ),
                                    mainPanel(
                                      tabsetPanel(
                                        #TAB PANEL1: PLOT THE EVENT DISTRIBUTION
                                        tabPanel("Event Distribution over events",
                                                 fluidRow(
                                                   column(11,
                                                   ),
                                                   column(1,
                                                          dropdownButton(
                                                            tags$h4(strong("Event Distribution")),
                                                            tags$h5("this graph shows the" ,strong("percentage of occurrence")," of the selected events,
                                                   calculated as the number of events of the single type out of the total
                                                   of events present in the event log"),
                                                            tags$h5("switch to plot absolute counts instead of percentage on the y axis:"),

                                                            switchInput(
                                                              inputId = "eventDist_count",
                                                              label = "counts",
                                                              labelWidth = "20px",
                                                              size = "mini"
                                                            ),
                                                            circle = FALSE,
                                                            status = "info",
                                                            size = "xs",
                                                            icon = icon("fas fa-info"),
                                                            width = "300px",
                                                            right = TRUE,
                                                            tooltip = tooltipOptions(title = "Click to more info"))
                                                   )),
                                                 fluidRow(
                                                   br(),
                                                   plotOutput("eventdist"),
                                                 )
                                        ),

                                        #TAB PANEL 2: EVENT DISTRIBUTION OVER PATIENTS
                                        tabPanel("Event distribution over patiens",
                                                 fluidRow(
                                                   column(11,
                                                   ),
                                                   column(1,
                                                          dropdownButton(
                                                            tags$h4(strong("Event Distribution over Patients")),
                                                            tags$h5("the graph shows the" ,strong("distribution"),"of events by patient,
                                                                  showing as a percentage, how many patients shared a specific type of event"),
                                                            tags$h5("switch to plot absolute counts instead of percentage on the y axis:"),
                                                            switchInput(
                                                              inputId = "eventPaz_count",
                                                              label = "counts",
                                                              labelWidth = "20px",
                                                              size = "mini"
                                                            ),
                                                            circle = FALSE,
                                                            status = "info",
                                                            size = "xs",
                                                            icon = icon("fas fa-info"),
                                                            width = "300px",
                                                            right = TRUE,
                                                            tooltip = tooltipOptions(title = "Click to more info"))
                                                   )),
                                                 fluidRow(
                                                   br(),
                                                   plotOutput("eventdist_Paz")
                                                 )
                                        ),
                                        #TAB PANEL 3: PANEL FOR EVENT DISTRIBUTION OVER TIME
                                        tabPanel("Event distribution over time",
                                                 fluidRow(
                                                   column(11,
                                                   ),
                                                   column(1,
                                                          dropdownButton(
                                                            tags$h4(strong("Event Distribution over time")),
                                                            tags$h5("The bar plot shows" ,strong("the distribution"),"of the  events in different time."),
                                                            tags$h5("the time axis has been discretized using ",strong("quarters"),
                                                                    "in order to highlight how the distribution of each event changes over time"),

                                                            sliderInput(
                                                              "date.range",
                                                              "Select Data Range:",
                                                              min = min(as.Date(data_reactive$EventLog[,"DATE_INI"])),
                                                              max = max(as.Date(data_reactive$EventLog[,"DATE_INI"])),
                                                              value=c(min(as.Date(data_reactive$EventLog[,"DATE_INI"])),max(as.Date(data_reactive$EventLog[,"DATE_INI"]))),
                                                              timeFormat="%Y-%m-%d"),


                                                            circle = FALSE,
                                                            status = "info",
                                                            size = "xs",
                                                            icon = icon("fas fa-info"),
                                                            width = "300px",
                                                            right = TRUE,
                                                            tooltip = tooltipOptions(title = "Click to more info")
                                                          )
                                                   )),
                                                 fluidRow(
                                                   plotOutput("eventdist_Time")
                                                 )
                                        ),


                                        #TABPANEL 4:PANEL FOR THE PLOT HO HEAT MAP
                                        tabPanel("Co-occurence Matrix",
                                                 fluidRow(
                                                   column(11,
                                                   ),
                                                   column(1,
                                                          dropdownButton(
                                                            tags$h4(strong("Co-occurence Matrix")),
                                                            tags$h5("this graph shows the" ,strong("co-occurrence")," of the selected events,
                                                                  calculated as the number of times (among the total) each event occurs in the presence of another,
                                                                  within the clinical path of the same patient."),
                                                            tags$h5(strong("Shades of red"),"are used to highlight", strong("high correlated events")),
                                                            tags$h5(strong("Shades of yellow"),"are used to", strong("low correlated events")),
                                                            tags$h5("Only co-occurrence values graater than 0.5 are shown"),
                                                            circle = FALSE,
                                                            status = "info",
                                                            size = "xs",
                                                            icon = icon("fas fa-info"),
                                                            width = "300px",
                                                            right = TRUE,
                                                            tooltip = tooltipOptions(title = "Click to more info"))
                                                   )),
                                                 fluidRow(

                                                   plotOutput("heatmap")
                                                 )
                                        ),

                                        #TABPANEL 5: TABLE PANEL for UPSET Plot
                                        tabPanel("Upset Plot",
                                                 fluidRow(
                                                   column(11,
                                                   ),
                                                   column(1,
                                                          dropdownButton(
                                                            tags$h4(strong("Upset-plot")),
                                                            tags$h5("The upsetplot is another way to analyze" ,strong("the co-occurrence"),"of the  events."),
                                                            tags$h5("In the lower part of the graph are represented the",strong("combination sets"),
                                                                    ",which are the combination of events that co-occur in the clinical path of at least one patient."),
                                                            tags$h5("The graph shows the frequencies of occurrence for each combination set"),
                                                            circle = FALSE,
                                                            status = "info",
                                                            size = "xs",
                                                            icon = icon("fas fa-info"),
                                                            width = "300px",
                                                            right = TRUE,
                                                            tooltip = tooltipOptions(title = "Click to more info"))
                                                   )),
                                                 fluidRow(
                                                   plotOutput("upsetplot")
                                                 )
                                        ),

                                        #TABPANEL 6: TRACE EVOLUTION plot
                                        tabPanel("Trace Evolution plot",
                                                 fluidRow(
                                                   column(11,
                                                   ),
                                                   column(1,
                                                          dropdownButton(
                                                            tags$h4(strong("Trace-plot")),
                                                            tags$h5("This plot shows the traces and reveal the contribute of all the traces during the time."),
                                                            tags$h5("Switch if you want to see the",strong("cumulative"),":"),

                                                            switchInput(
                                                              inputId = "trace_cum",
                                                              label = "",
                                                              labelWidth = "20px",
                                                              size = "mini"
                                                            ),

                                                            tags$h5("It is also possible to set the ",strong("temporal scale")," and to select the",strong("max time value")),

                                                            selectInput("time_scale",
                                                                        label = "",
                                                                        choices = c("hours","days","weeks","months"),
                                                                        selected = "weeks"

                                                            ),

                                                            numericInput("max.t", label = "Set max time:", value = 10),


                                                            circle = FALSE,
                                                            status = "info",
                                                            size = "xs",
                                                            icon = icon("fas fa-info"),
                                                            width = "300px",
                                                            right = TRUE,
                                                            tooltip = tooltipOptions(title = "Click to more info"))
                                                   )),
                                                 fluidRow(
                                                   plotOutput("traceplot")
                                                 )
                                        ),
                                      )
                                    ),
                                  ),
                           )
                         ),
                ),
                target = "Loading Data",
                position = "after")
    }


    removeTab(inputId = "tabs", target = "EventLog trace inspection")
    if(!is_empty(data_reactive$EventLog)){
      insertTab("tabs",
                tabPanel("EventLog trace inspection",
                         titlePanel("Trace Analysis"),
                         br(),
                         fluidRow(
                           column(12,
                                  sidebarLayout(
                                    sidebarPanel(
                                      p("In this section it is possible to scout which traces satisfy some rules
                                        that you can define by completing the following fields: "),
                                      fluidRow(
                                        column(6,
                                               selectInput("event.start",
                                                           label = "starting event:",
                                                           choices = unique(data_reactive$EventLog[,4])
                                               )
                                        ),
                                        column(6,
                                               selectInput("event.end",
                                                           label = "last event:",
                                                           choices = unique(data_reactive$EventLog[,4]),
                                                           selected = unique(data_reactive$EventLog[,4])[2]


                                               )
                                        )

                                      ),



                                      p("Select the time window where a transition between
                                        the selected \"starting\" and \"last\" event should be considered valid."),

                                      fluidRow(
                                        column(4,
                                               numericInput("time.b", label = "Set min time:", value = 0)
                                               ),
                                        column(4,
                                               numericInput("time.a", label = "Set max time:", value = 20)
                                               ),
                                        column(4,
                                               switchInput(
                                                 inputId = "inf",
                                                 label = "max",
                                                 labelWidth = "20px",
                                                 size= "mini"
                                               )

                                        )
                                      ),

                                      fluidRow(
                                        column(12,
                                               selectInput("um.time",
                                                           label = "Select the time scale",
                                                           choices = c("mins","hours","days","weeks","months"),
                                                           selected = "days"
                                               )
                                        )
                                      ),

                                      p("you can select wich event should and should not be in between the \"starting\" and \"last\" event"),

                                      fluidRow(
                                        column(6,
                                               selectInput("event.between",
                                                           label = "Event Between:",
                                                           choices = unique(data_reactive$EventLog[,4]),
                                                           multiple = TRUE)
                                               ),

                                        column(6,
                                               selectInput("event.NOT.between",
                                                           label = "Event NOT Between:",
                                                           choices = unique(data_reactive$EventLog[,4]),
                                                           multiple = TRUE)
                                               )
                                      ),
                                      # fluidRow(
                                      #   tags$hr(),
                                      #   materialSwitch(
                                      #     inputId = "more.graph",
                                      #     label = "show more graph",
                                      #     status = "primary",
                                      #     right = TRUE
                                      #   )
                                      # ),
                                      fluidRow(
                                        selectizeInput(inputId = "id","Select patient or a group of patients",
                                                       choices = NULL, multiple = TRUE)
                                      )
                                      # conditionalPanel("output.moregr== 'yes' ",
                                      #                  selectizeInput(inputId = "id","Select paz or a group of paz",
                                      #                                 choices = NULL, multiple = TRUE)
                                      #                  # selectInput("id","Select paz or a group of paz",
                                      #                  #             choices = NULL, multiple = TRUE)
                                      #                  )
                                    ),
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Trace Table",
                                                   DT::dataTableOutput("trace.id")
                                                 ),

                                        tabPanel("Time Line Plot",
                                                 fluidRow(
                                                   column(11,
                                                          plotOutput("time.line")
                                                          ),
                                                   column(1,
                                                          dropdownButton(
                                                            tags$h4(strong("Time Line Plot")),
                                                            tags$h5("This plot shows graphically the succession of the events that have been
                                                                    experienced by each patient whose trace responds to the query expressed in the left panel"),

                                                            br(),
                                                            tags$h5("Switch if you want to show patient ID on y axes"),
                                                            switchInput(
                                                              inputId = "id.legend",
                                                              label = "show ID",
                                                              labelWidth = "15px",
                                                              size = "mini"
                                                            ),
                                                            circle = FALSE,
                                                            status = "info",
                                                            size = "xs",
                                                            icon = icon("fas fa-info"),
                                                            width = "300px",
                                                            right = TRUE,
                                                            tooltip = tooltipOptions(title = "Click to more info"))
                                                          )
                                                 ),
                                                 uiOutput("more.graph.ui")
                                                 # conditionalPanel("output.moregr",
                                                 #                  # fluidRow(
                                                 #                  #   timevis::timevisOutput("timeVis.timeline")
                                                 #                  # )
                                                 #                  uiOutput("more.graph.ui")
                                                 #                  )

                                              )
                                        )
                                      )
                                    )
                                  )
                           )
                         ),
                target = "EventLog data analysis",
                position = "after"
      )}
    }

  })

  # rv.moregr <- reactiveValues(show.moregr = FALSE)
  #
  # observeEvent(input$more.graph, ({
  #   rv.moregr$show.moregr <- !(rv.moregr$show.moregr)
  # }))
  #
  # output$moregr <- renderText({
  #   if(!rv.moregr$show.moregr){
  #     "yes"
  #   } else{
  #     "no"
  #   }
  # })
  #
  # outputOptions(output, "moregr", suspendWhenHidden = FALSE)

  output$more.graph.ui<-renderUI({
    fluidRow(
      timevis::timevisOutput("timeVis.timeline")
    )
  })



  observeEvent(list(input$event.start,
                    input$event.end,
                    input$time.b,
                    input$time.a,
                    input$inf,
                    input$um.time,
                    input$event.between,
                    input$event.NOT.between),{
    shiny::updateSelectizeInput(inputId = "id",label = "Select patient or a group of patients",
                      choices = matrix_taceid()[,1],selected =  "NA")
  },ignoreInit = T)




  observeEvent(input$event.start,{
    data_reactive$ev.start<-input$event.start
    shiny::updateSelectInput(
      inputId = "event.end",
      label = "last event:",
      choices = unique(data_reactive$EventLog[,4])[!unique(data_reactive$EventLog[,4]) %in% data_reactive$ev.start]
    )
    shiny::updateSelectInput(
      inputId = "event.NOT.between",
      label = "Event NOT Between:",
      choices = unique(data_reactive$EventLog[,4])[!unique(data_reactive$EventLog[,4]) %in% data_reactive$ev.bet &
                                                     !unique(data_reactive$EventLog[,4]) %in% data_reactive$ev.start &
                                                     !unique(data_reactive$EventLog[,4]) %in% data_reactive$ev.end]
    )


  })

  observeEvent(input$event.end,{
    data_reactive$ev.end<-input$event.end
    shiny::updateSelectInput(
      inputId = "event.NOT.between",
      label = "Event NOT Between:",
      choices = unique(data_reactive$EventLog[,4])[!unique(data_reactive$EventLog[,4]) %in% data_reactive$ev.bet &
                                                     !unique(data_reactive$EventLog[,4]) %in% data_reactive$ev.start &
                                                     !unique(data_reactive$EventLog[,4]) %in% data_reactive$ev.end]
    )

  })

  observeEvent(input$event.between,{
    data_reactive$ev.bet<-input$event.between
    shiny::updateSelectInput(
      inputId = "event.NOT.between",
      label = "Event NOT Between:",
      choices = unique(data_reactive$EventLog[,4])[!unique(data_reactive$EventLog[,4]) %in% data_reactive$ev.bet &
                                                   !unique(data_reactive$EventLog[,4]) %in% data_reactive$ev.start &
                                                   !unique(data_reactive$EventLog[,4]) %in% data_reactive$ev.end]
    )


  })


#======================================================== ID TRACE DATA TABLE ===============================================
  matrix_taceid<-reactive({
    matrix.id<-trace.id(objQOD,
                        input$event.start,
                        input$event.end,
                        input$time.b,
                        input$time.a,
                        input$inf,
                        input$um.time,
                        input$event.between,
                        input$event.NOT.between,
                        comp.mat = TRUE)

    datax<-as.data.frame(matrix.id)
    colnames(datax)[1]<-"ID"
    return(datax)
  })


  output$trace.id<- DT::renderDataTable({
    if(is.na(matrix_taceid()[1,1])){
      validate("No patient with traces that meet the inherent requirements")
    }
     matrix_taceid()
  })


  #======================================================== TRACE TIME LINE ===================================================

  plot.traceid<-reactive({
    id<-trace.id(objQOD,
                 input$event.start,
                 input$event.end,
                 input$time.b,
                 input$time.a,
                 input$inf,
                 input$um.time,
                 input$event.between,
                 input$event.NOT.between,
                 comp.mat = FALSE)
    plot.trace<-plot.timeline.fun(objQOD,id,input$um.time, input$time.a, input$inf, input$id.legend)
    return(plot.trace)
  })

  output$time.line<-renderPlot({
    if(is.na(matrix_taceid()[1,1])){
      validate("No patient with traces that meet the inherent requirements")
    }
    plot.traceid()
  })

  #========================== Event distribution plot (first tabPanel: output$eventdist)=================================
  plot_obj_eldist<-reactive({
    plot_fin<-event_plot(data_reactive$EventLog,data_reactive$event_delete,input$eventDist_count)
    return(plot_fin)
  })

  #plot bar plot
  output$eventdist<-renderPlot({
    plot_obj_eldist()
  })

  #==================================== Event distribution over Patiens (second tabPanel: output$eventdist_Paz)===================
  plot_obj_event_paz<-reactive({
    plot_fin<-event_pat(data_reactive$EventLog,data_reactive$event_delete,input$eventPaz_count)
    return(plot_fin)
  })

  output$eventdist_Paz<-renderPlot({
    plot_obj_event_paz()
  })

  #==================================== Event distribution over time (third tabPanel: output$eventdist_Time)===================
  observeEvent(input$date.range,{
    data_reactive$date_range<-input$date.range
  })



  plot_obj_event_time<-reactive({
    plot_fin<-time_dist(data_reactive$EventLog,data_reactive$event_delete, data_reactive$date_range)
    return(plot_fin)
  })

  output$eventdist_Time<-renderPlot({
    plot_obj_event_time()
  })
  #=================================== Event heatmap (fourth tabPanel: output$heatmap)==========================================
  plot_obj_heatmap<-reactive({
    validate(
      need(length(data_reactive$event_delete)>1, "You need at least 2 different events")
    )

    plot_fin<-heatmap(data_reactive$EventLog,data_reactive$event_delete,objDL.new)
    return(plot_fin)
  })

  output$heatmap<-renderPlot({
    plot_obj_heatmap()
  })
  #=================================== Event upsetPlot plot (fiveth tabPanel: output$upsetplot) =================================
  plot_obj_upsetplot<-reactive({
    validate(
      need(length(data_reactive$event_delete)>1, "You need at least 2 different events")
    )

    plot_fin<-upset_fun(data_reactive$EventLog,data_reactive$event_delete,objDL.new)
    return(plot_fin)
  })

  output$upsetplot<-renderPlot({
    if(is.null(plot_obj_upsetplot())){
      validate("there are no co-occurring events")
    }else{
      plot_obj_upsetplot()
    }

  })

  #=================================== Event trace plot (sixth tabPanel: output$traceplot) =================================
  plot_obj_traceplot<-reactive({
    validate(
      need(length(data_reactive$event_delete)>1, "You need at least 2 different events")
    )

    cum.flag<-input$trace_cum
    temp.scale<-input$time_scale

    plot_fin<-trace.evolution(data_reactive$event_delete,objDL.new,cum.flag,temp.scale,input$max.t)
    return(plot_fin)
  })

  output$traceplot<-renderPlot({
    plot_obj_traceplot()
  })


  timevis.plot<-reactive({
    id<-input$id
    if(!is.null(id)){
      if(length(input$id)==1){
        plottv<-timeLine.data(id[1],objDL.out,single = TRUE)
      }else{
        plottv<-timeLine.data(id,objDL.out,single = FALSE)
      }
    }else{
      plottv<-NULL
    }
    return(plottv)

  })

  output$timeVis.timeline<-timevis::renderTimevis(timevis.plot())

}


