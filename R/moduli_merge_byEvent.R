#'@title Merge module: Merge by eventsettings
#'

#'@import dplyr
#'@rawNamespace import(shiny,  except = c(renderDataTable,dataTableOutput))
#'@importFrom DT renderDataTable dataTableOutput

ui_merge.ev<-function(id,data,num){
  ns<-NS(id)
  fluidPage(

    fluidRow(
      column(12,
             strong(h3("Variables arrangement:")),
             br()
      )
    ),

    fluidRow(
      column(4,
             pickerInput(
               inputId = ns("var"),
               label = "Select Crossectional variable for the enrichment", #scelgo variabili crossectional
               choices = append(colnames(data)[!colnames(data) %in% all.key[[num]]],"none"),
               multiple = TRUE,
               options = list(
                 title = "select Crossectional var"),
               selected = NULL
             )),
      column(4,
             pickerInput(
               inputId = ns("var_long"),
               label = "Select time-dependent variable for the enrichment", #scelgo var longitudinale UNA ed UNA SOLA
               choices = append(colnames(data)[!colnames(data) %in% all.key[[num]]],"none"),
               multiple = FALSE,
               options = list(
                 title = "select Longitudinal var"),
               selected = "none"
             )
      )
    ),


    fluidRow(
      column(12,
             tags$hr(),
             strong(h3("Creation of the Events:")),
             p(h5("It is necessary to explicit the characteristics of the new event to add to the EventLog file: \n"))
      )
    ),

    fluidRow(
      column(3,
             pickerInput(
               inputId = ns("ID"),
               label = "ID equals to the KEY",
               choices = all.key[[num]]
             )
      ),
      column(3,
             pickerInput(
               inputId = ns("DATA_INI"),
               label = "select var for DATE_INI\n",
               choices = colnames(data)[!colnames(data) %in% all.key[[num]]],
               options = list(
                 title = "select var as Date Ini"),
               selected = NULL
             )
      ),
      column(3,
             pickerInput(
               inputId = ns("DATA_END"),
               label = "select var for DATE_END",
               choices = colnames(data)[!colnames(data) %in% all.key[[num]]],
               options = list(
                 title = "select var as Date End"),
               selected = NULL
             )

      ),
      column(3,
             pickerInput(
               inputId = ns("EVENT"),
               label = "select var for EVENT\n",
               choices = append(colnames(data)[!colnames(data) %in% all.key[[num]]],"none"),
               options = list(
                 title = "select var as Event"),
               selected = NULL
             ),
             prettySwitch(ns("new.name"),"set new "),


      )

    ),

    fluidRow(
      column(3,offset = 9,
             conditionalPanel('output.event_name=="yes"', ns=ns,
                              textInput(ns("set.name"), label = "", value = "Set new Event description"),
             )
      )
    ),


    fluidRow(
      column(12,
             tags$hr(),
             p(h5("You can add crossectional variables for the new event log file, creating from the time-dependent variable.\n
                   You have to notice that when this new event log will be merged with the original one, only event coming from longitudinal data will have
                   this informations"))
      )
    ),

    fluidRow(
      column(3,
             pickerInput(
               inputId =ns("var_add"),
               label = "select var to add",
               choices = colnames(data)[!colnames(data) %in% all.key[[num]]],
               multiple = TRUE,
               options = list(
                 title = "select cross var for this new EventLog"),
               selected = NULL

             )
      )
    ),


    fluidRow(
      column(12,
             DT::dataTableOutput(ns("newEV")),
             tags$hr()
      )
    ),

    fluidRow(
      column(12,
             fluidRow(
               column(12,
                      br(),
                      actionBttn(
                        inputId = ns("wrtXML"),
                        label = "Save Settings",
                        style = "minimal",
                        color = "primary",
                        icon = icon("far fa-object-ungroup")
                      )
               )
             ),
      )
    )
  )
}

server_merge.ev<-function(input,output,session,data,id){
  data_ret.ev<-reactiveValues(
    key=all.key[[id+1]],
    var=array(), #crossectional
    var_long=array(), #longitudinal
    new.name=FALSE,

    #new EventLog
    data_ini=array(),
    data_end=array(),
    event=array(),
    setted_name=array(),
    var_crossEL=array(),
  )

  observeEvent(input$var,{
    data_ret.ev$var<-input$var
    #quando aggirono var cross, modifico contenuti di var long a tutti tranne quelli scelti come chiave e var cross
    updatePickerInput(
      session = session,
      inputId = "var_long",
      label = "Select time-dependent variable for the enrichment",
      choices = append(colnames(data)[!colnames(data) %in% data_ret.ev$var & !colnames(data) %in% data_ret.ev$key],"none"),
      selected = "none"
    )
    updatePickerInput(
      session = session,
      inputId = "var_add",
      label = "select var to add",
      choices = colnames(data)[!colnames(data) %in% data_ret.ev$var & !colnames(data) %in% data_ret.ev$key & !colnames(data) %in% data_ret.ev$event & !colnames(data) %in% data_ret.ev$data_ini & !colnames(data) %in% data_ret.ev$data_end],
      selected = "none"
    )
  },ignoreNULL = FALSE)


  observeEvent(input$var_long,{
    data_ret.ev$var_long<-input$var_long
    updatePickerInput(
      session = session,
      inputId = "var_add",
      label = "select var to add",
      choices = colnames(data)[!colnames(data) %in% data_ret.ev$var & !colnames(data) %in% data_ret.ev$key & !colnames(data) %in% data_ret.ev$event & !colnames(data) %in% data_ret.ev$data_ini & !colnames(data) %in% data_ret.ev$data_end],
      selected = "none"
    )
    updatePickerInput(
      session = session,
      inputId = "EVENT",
      label = "select var for EVENT\n",
      choices = data_ret.ev$var_long,
      selected = data_ret.ev$var_long
    )



  })



  observeEvent(input$ID,{
    data_ret.ev$key<-input$ID

  })

  observeEvent(input$DATA_INI,{
    data_ret.ev$data_ini<-input$DATA_INI
    updatePickerInput(
      session = session,
      inputId = "var_add",
      label = "select var to add",
      choices = colnames(data)[!colnames(data) %in% data_ret.ev$var & !colnames(data) %in% data_ret.ev$key & !colnames(data) %in% data_ret.ev$event & !colnames(data) %in% data_ret.ev$data_ini & !colnames(data) %in% data_ret.ev$data_end],
      selected = "none"
    )
  })

  observeEvent(input$DATA_END,{
    data_ret.ev$data_end<-input$DATA_END
    updatePickerInput(
      session = session,
      inputId = "var_add",
      label = "select var to add",
      choices = colnames(data)[!colnames(data) %in% data_ret.ev$var & !colnames(data) %in% data_ret.ev$key & !colnames(data) %in% data_ret.ev$event & !colnames(data) %in% data_ret.ev$data_ini & !colnames(data) %in% data_ret.ev$data_end],
      selected = "none"
    )
  })

  observeEvent(input$EVENT,{
    data_ret.ev$event<-input$EVENT
    updatePickerInput(
      session = session,
      inputId = "var_add",
      label = "select var to add",
      choices = colnames(data)[!colnames(data) %in% data_ret.ev$var & !colnames(data) %in% data_ret.ev$key & !colnames(data) %in% data_ret.ev$event & !colnames(data) %in% data_ret.ev$data_ini & !colnames(data) %in% data_ret.ev$data_end],
      selected = "none"
    )
  })

  observeEvent(input$var_add,{
    data_ret.ev$var_crossEL<-input$var_add
  },
  ignoreNULL = FALSE,
  )

  observeEvent(input$new.name,{
    data_ret.ev$new.name=input$new.name
  })

  output$event_name<-renderText({
    if(data_ret.ev$new.name){
      "yes"
    }else{
      "no"
    }
  })

  outputOptions(output, "event_name", suspendWhenHidden = FALSE)

  observeEvent(input$set.name,{
    data_ret.ev$setted_name<-input$set.name
  })

  var<-reactive({
    req(!data_ret.ev$event=="none" & !data_ret.ev$event=="" & !data_ret.ev$data_ini=="" & !data_ret.ev$data_end=="")
    if(is.null(data_ret.ev$var_crossEL)){
      df<-data.frame(matrix(0, nrow = nrow(data), ncol = 4))
      colnames(df)<-c(paste0("ID:", data_ret.ev$key),
                      paste0("DATA_INI:",data_ret.ev$data_ini),
                      paste0("DATA_INI:",data_ret.ev$data_end),
                      paste0("EVENT:",data_ret.ev$event))
      df[,]<-c(data[,data_ret.ev$key],
               data[,data_ret.ev$data_ini],
               data[,data_ret.ev$data_end],
               data[,data_ret.ev$event])


    }else{
      l<-length(data_ret.ev$var_crossEL)
      df<-data.frame(matrix(0,  nrow(data), ncol = 4+l))
      nameEL<-c(paste0("ID:", data_ret.ev$key),
                paste0("DATA_INI:",data_ret.ev$data_ini),
                paste0("DATA_INI:",data_ret.ev$data_end),
                paste0("EVENT:",data_ret.ev$event))
      colnames(df)[1:4]<-nameEL
      colnames(df)[5:ncol(df)]<-data_ret.ev$var_crossEL
      df[,1:4]<-c(data[,data_ret.ev$key],
                  data[,data_ret.ev$data_ini],
                  data[,data_ret.ev$data_end],
                  data[,data_ret.ev$event])
      for (i in c(1:l)) {
        df[,data_ret.ev$var_crossEL[i]]<-data[,data_ret.ev$var_crossEL[i]]
      }

    }

    if(data_ret.ev$new.name & !is.na(data_ret.ev$setted_name)){
      df[which(!is.na(df[,2])),4]<-data_ret.ev$setted_name
    }

    return(df)})


  output$newEV<-DT::renderDataTable(var(),options = list(iDisplayLength = 5))

  observeEvent(input$wrtXML,{
    event<-list()
    event[[1]]<-c(data_ret.ev$data_ini,data_ret.ev$data_end,data_ret.ev$event,"NA")
    if(data_ret.ev$new.name){
      event[[1]]<-c(data_ret.ev$data_ini,data_ret.ev$data_end,data_ret.ev$event,data_ret.ev$setted_name)
    }
    if(is.null(data_ret.ev$var_crossEL)){
      cross<-"none"
    }else{
      cross<-data_ret.ev$var_crossEL
    }

    XML.write.fun(id,data_ret.ev$key,data_ret.ev$var,event,cross,FALSE)
    sendSweetAlert(
      session = session,
      title = "Done!",
      text = "",
      type = "success"
    )

  }, ignoreInit = TRUE)      ####CHECK IGNOREINITTTTT


}


