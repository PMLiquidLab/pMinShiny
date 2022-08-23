#'@title Merge module: main page
#'
#'@import shiny
#'@import dplyr
#'@import shinyWidgets
#'@import DT
#'@import rlang
#'@import shinybusy
#'@import pMineR




server<-function(input,output,session){
  #visualizzazione EventLog
  tab<-callModule(import_data_server,"uploadEL","EventLog")


  # reactiveValues: initializing data as null data frame
  data_reactive <- reactiveValues(
    EventLog=data.frame(),
    ancillaryData=list(),
    tabs=list(),
    event_delete=array(),
    event_density=array(),
    final_EL=data.frame(),

  )


  #TAB EVENTLOG: data visualization of eventlog
  observeEvent(input$loadEL,{
    data_reactive$EventLog <- all.data[["EventLog"]]
    all.data[["Data0"]]<<-NULL
    # data_reactive$EventLog <- loadData("uploadEL.csv")
    if(is_empty(data_reactive$EventLog)){
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Load your EventLog, then press 'Load Event Log'  button",
        type = "primary"
      )
      data_reactive$EventLog<-data.frame()
    }
    data_reactive$ancillaryData= list()





    #loading data uploaded in the upload section
    removeTab(inputId = "tabs", target = "Loading Ancillary")
    insertTab(inputId = "tabs",
              tabPanel("Loading Ancillary",
                       titlePanel("Ancillary Data Loading"),
                       br(),
                       fluidRow(
                         column(12,
                                import_mod_ui("upload0","Load at least one Ancillary Data Set",TRUE),
                                actionButton("add","Upload other file", width ='32%')
                         )
                       ),
                       fluidRow(
                         column(12,
                                actionButton("merge","Merge", width = '32%')
                         )
                       )
              ),
              target = "Loading EventLog",
              position = "after"
    )
    #Data Long visualization
    if(is_empty(data_reactive$ancillaryData)){
      callModule(import_data_server,"upload0","Data0") #Flag=DATALONG for merge EventLog+Longitudinal data
    }else{
      callModule(import_data_server,"upload0","Data0",TRUE) #Flag=DATALONG for merge EventLog+Longitudinal data
    }
  })


  # #Data Long visualization
  # if(is_empty(data_reactive$ancillaryData)){
  #   callModule(import_data_server,"upload0","Data0") #Flag=DATALONG for merge EventLog+Longitudinal data
  # }else{
  #   callModule(import_data_server,"upload0","Data0",TRUE) #Flag=DATALONG for merge EventLog+Longitudinal data
  # }


  observeEvent(input$add,{
    insertUI(
      selector = "#add",
      where = "beforeBegin",
      ui = import_mod_ui(paste0("upload", input$add),
                         "Upload other Ancillary file",TRUE)
    )
    callModule(import_data_server,paste0("upload", input$add),paste0("Data", input$add))
  })



  observeEvent(input$merge,{
    data_reactive$ancillaryData<-all.data[!names(all.data)=="EventLog"]
    tabs<-list()
    for (i in seq_along(data_reactive$ancillaryData)){
      tabs[[i]]<-tabPanel(paste0("File",i),
                          ui_mod_long(paste0("tab",i),data_reactive$ancillaryData[[i]],i))
    }

    lapply(seq_along(data_reactive$ancillaryData), function(i){
      callModule(server_mod_long,
                 paste0("tab",i),
                 data_reactive$ancillaryData[[i]],
                 i)
    })

    data_reactive$tabs<-tabs



    #loading data uploaded in the upload section
    removeTab(inputId = "tabs", target = "Enrichment techniques")
    insertTab(inputId = "tabs",
              tabPanel("Enrichment techniques",
                       titlePanel("Enrichment techniques"),
                       # br(),
                       tags$hr(),
                       fluidRow(
                         column(12,
                                sidebarLayout(
                                  sidebarPanel(
                                    p(strong(h4("Select the merging technique:"))),
                                    p(h5("Both the proposed techniques generate a data frame containing
                                         the EventLog enriched with the ancillary data uploaded in the previous section.
                                         The main difference is about how to handle with longitudinal data, which are",
                                         strong("time-dependent data"))),

                                    p(h5("ClickIn the",strong("\"Enrichment as events\""),"technique the starting point is the EventLog: the longitudinal data
                                    are added to the event log as new events (new rows).
                                    Also in this case an interface will appear by clicking on the \"Merge by Event\" button.In this case it is
                                    important to explicit the characteristics that the event must have inside of the EventLog (i.e. DateIni, DateEnd, Event name)")),


                                    p(h5(strong("\"Enrichment by attribute:\""), "using this merging technique the new merged file will have the same size as the original Event log,
                                         but in this new file will be added new columns rappresenting the values of the dependent time variables.\n
                                         by clicking on the \"Merge by Attribute\" button, an interface will appear in which it will be possible to set all the parameters
                                         and to define rules in order to explicit how to trasforme time dipendent values into a unique value associated
                                         to the single event of the original Event Log")),

                                    p(h5("Whichever technique you choose, you can set the necessary parameters for
                                    the merge by clicking on the specific button:", strong("\"merge as event\""), "if you want to embed new information such as events," ,strong("\"merge by attribute\""), "if you want to
                                    incorporate variables as attributes of the event")),


                                    tags$hr(),

                                    br(),
                                    br(),
                                    br(),
                                    br(),
                                    actionBttn(
                                      inputId = "done",
                                      label = "compute merge",
                                      style = "minimal",
                                      color = "primary"
                                    ),
                                    width = 4
                                  ),
                                  mainPanel(
                                    uiOutput("files")
                                  )
                                )

                         )
                       )
              ),
              target = "Loading Ancillary",
              position = "after")
  })

  output$files <- renderUI({
    tagList(
      do.call(tabsetPanel,data_reactive$tabs)
    )
  })


  observeEvent(input$done,{
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, {
                   tryCatch(
                     {
                       data_reactive$final_EL<-merge_fun()
                     },
                     error = function(e) {
                       # return a safeError if a parsing error occurs
                       # stop(safeError(e))
                       data_reactive$final_EL<-data.frame()
                     })
                 })

    removeTab(inputId = "tabs", target = "New Event Log")
    insertTab(inputId = "tabs",
              tabPanel("New Event Log",
                       titlePanel("Enriched Event Log"),
                       br(),
                       tags$hr(),
                       if(is_empty(data_reactive$final_EL)){
                         fluidRow(
                           column(12,
                                  textOutput("Errore")
                           )
                         )
                       }else{
                         fluidPage(
                           fluidRow(
                             column(12,
                                    DT::dataTableOutput("final.EventLog")
                             )
                           ),
                           fluidRow(
                             column(12,
                                    br(),
                                    downloadButton("downloadData", "Download")
                             )
                           )
                         )

                       }


              ),
              target = "Enrichment techniques",
              position = "after")
  })






  output$Errore<-renderText("something has gone wrong. Perhaps the data you entered in the \"Enrichment rules\" section, were not correct")
  output$final.EventLog<-DT::renderDataTable(data_reactive$final_EL)
  output$downloadData <- downloadHandler(

    filename = function() {
      paste("finalEL", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_reactive$final_EL, file, row.names = FALSE)
    }
  )




}



