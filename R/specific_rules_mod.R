#'@title Merge module: Merge by attribute settings
#'
#'@rawNamespace import(shiny, except = c(renderDataTable,dataTableOutput))
#'@import dplyr
#'@import shinyWidgets


ui_regole<-function(id,data,num){
  ns<-NS(id)
  fluidPage(
    fluidRow(
      column(12,
             strong(h3("Variables arrangement:")),
             br()
      )
    ),

    fluidRow(
      column(3,
             pickerInput(
               inputId = ns("var"),
               label = "Select Crossectional variable", #scelgo variabili crossectional
               choices = append(colnames(data)[!colnames(data) %in% all.key[[num]]],"none"),
               multiple = TRUE,
               options = list(
                 title = "select Crossectional var")
             )),
      column(3,
             pickerInput(
               inputId = ns("var_long"),
               label = "Select time-dependent variable", #scelgo var longitudinale UNA ed UNA SOLA
               choices =append(colnames(data)[!colnames(data) %in% all.key[[num]]],"none"),
               multiple = FALSE,
               options = list(
                 title = "select Longitudinal var"))
      )
    ),

    fluidRow(
      column(12,
             tags$hr(),
             strong(h3("Defining Merge rules:")),
             br()
      )
    ),
    fluidRow(
      column(4,
             pickerInput(ns("set_var_final"),
                         label = "select which variable do you want to use",
                         choices = NULL,
                         options = list(`actions-box` = TRUE),
                         multiple = TRUE)
      ),
      column(4,
             pickerInput(ns("value"),
                         label = "select which variable do you want to use as Value",
                         choices = NULL,
                         options = list(`actions-box` = TRUE),
                         multiple = FALSE
             )
      ),
      column(4,
             pickerInput(ns("time"),
                         label = "select which variable to use as temporal indicator",
                         choices = NULL,
                         options = list(`actions-box` = TRUE),
                         multiple = FALSE)
      )
    ),

    fluidRow(
      column(12,
             tags$hr(),
             p("Select \"Delta Before\" and \"Delta after\" as respectively how many",strong("days"),"before and after the event, you want to
                                       consider as time window to calculate the point estimation of the time-dipendent value")
      )
    ),

    fluidRow(
      column(4,
             sliderInput(ns("delta_b"), label = "Select Delta Before", min = 0,
                         max = 30, value = 10)
      ),

      column(4,
             sliderInput(ns("delta_a"), label = "Select Delta After", min = 0,
                         max = 30, value = 10),
             checkboxInput(ns("delta_next"), label = "to the next event", value = FALSE)
      ),



      column(4,
             pickerInput(ns("rule"),"Select rule\n",choices = c("mean","min value","max value","last measure","first measure","count","trend"))
      )
    ),

    fluidRow(
      column(12,
             prettySwitch(
               inputId = ns("n.rules"),
               label = "Set specific rules for each time-dependent variable"
             )
      )
    ),

    fluidRow(
      column(12,
             conditionalPanel(condition = 'output.showpanel == "yes"', ns = ns ,
                              p(h4("Select for which variable you want to set specific rules")),
                              pickerInput(ns("set_var_specific"),
                                          label = "select which variables",
                                          choices = NULL,
                                          options = list(`actions-box` = TRUE),
                                          multiple = TRUE
                              ),
                              uiOutput(ns("specific_rules"))
             ),
      )
    ),

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
    )
  )
}

server_regole<-function(input,output,session,data,num){
  ns<-session$ns
  data_ret.ev<-reactiveValues(
    key=all.key[[num+1]],
    var=array(), #crossectional
    var_long=array(), #longitudinal
    n_rules=FALSE,
    var_final=array(), #variabili da usare tra quelle presenti nella unique(longitudinal)-->es: che esami uso?
    var_specific=array(), #per alcune tra le variabili di var_final posso voler esplicitare regole specifiche

    #Rules
    delta_bef=array(),
    delta_after=array(),
    delta_next=array(),
    rule=array(),
    value=array(),
    time=array(),
    xml_list=list(),
    tabs=list()
  )


  observeEvent(input$var,{
    data_ret.ev$var<-input$var
    #quando aggirono var cross, modifico contenuti di var long a tutti tranne quelli scelti come chiave e var cross
    updatePickerInput(
      session = session,
      inputId = "var_long",
      label = "Select time-dependent variable",
      choices = append(colnames(data)[!colnames(data) %in% data_ret.ev$var & !colnames(data) %in% data_ret.ev$key],"none"),
      # selected = "none"
    )
  },ignoreNULL = FALSE)


  observeEvent(input$var_long,{
    data_ret.ev$var_long<-input$var_long
    req(data_ret.ev$var_long)
    updatePickerInput(
      session = session,
      inputId = "value",
      label = "select which variable do you want to use as Value",
      choices = append(colnames(data)[!colnames(data) %in% data_ret.ev$var &
                                        !colnames(data) %in%  data_ret.ev$key &
                                        !colnames(data) %in% data_ret.ev$var_long],"none"),
      selected = "none"
    )

    updatePickerInput(
      session = session,
      inputId = "time",
      label = "select which variable to use as temporal indicator",
      choices = colnames(data)[!colnames(data) %in% data_ret.ev$var &
                                 !colnames(data) %in%  data_ret.ev$key &
                                 !colnames(data) %in% data_ret.ev$var_long],
      selected = "none"
    )



    if(data_ret.ev$var_long!="none"){
      # data_ret.ev$var_final<-unique(data[,data_ret.ev$var_long])
      updatePickerInput(
        session = session,
        inputId = "set_var_final",
        label = "select which variable do you want to use",
        choices = unique(data[,data_ret.ev$var_long]),
        selected = "none"
      )
    }
  },ignoreNULL = FALSE)

  observeEvent(input$set_var_final,{
    data_ret.ev$var_final<-input$set_var_final
    # req(data_ret.ev$var_final)
    updatePickerInput(
      session = session,
      inputId = "set_var_specific",
      label = "select which variable do you want to use",
      choices = data_ret.ev$var_final,
    )
  })

  observeEvent(input$value,{
    data_ret.ev$value<-input$value
    updatePickerInput(
      session = session,
      inputId = "time",
      label = "select which variable to use as temporal indicator",
      choices = colnames(data)[!colnames(data) %in% data_ret.ev$var &
                                 !colnames(data) %in%  data_ret.ev$key &
                                 !colnames(data) %in% data_ret.ev$var_long &
                                 !colnames(data) %in% data_ret.ev$value],
      selected = "none"
    )

    if( data_ret.ev$value=="none"){
      updatePickerInput(
        session = session,
        inputId = "rule",
        label = "Select rule",
        choices = c("count"),
        selected = "count")

    }else{
      updatePickerInput(
        session = session,
        inputId = "rule",
        label = "Select rule",
        choices = c("mean","min value","max value","last measure","first measure","count","trend"))
    }
  })



  observeEvent(input$delta_b,{
    data_ret.ev$delta_bef<-input$delta_b
  })
  observeEvent(input$delta_a,{
    data_ret.ev$delta_after<-input$delta_a
  })
  observeEvent(input$rule,{
    data_ret.ev$rule<-input$rule
  })


  observeEvent(input$n.rules, ({
    data_ret.ev$n_rules <- !(data_ret.ev$n_rules)
  }))

  observeEvent(input$time,{
    data_ret.ev$time<-input$time
  })

  observeEvent(input$set_var_specific,{
    data_ret.ev$var_specific<-input$set_var_specific
  })

  observeEvent(input$delta_next,{
    data_ret.ev$delta_next<-input$delta_next
  })

  rows.rule<-reactive({
    req(data_ret.ev$var_specific)
    ui_parts <- c()
    for(i in c(1:length(data_ret.ev$var_specific))){
      ui_parts[[i]] <-spec_setting_ui(ns(paste0("riga",i)),data_ret.ev$var_specific[i],data_ret.ev$var_long_type)
    }

    sapply(seq_along(data_ret.ev$var_specific), function(i){
      name<-paste0("file",i,".txt")
      callModule(spec_setting_server,paste0("riga",i),name,data_ret.ev$var_specific[i])
    })


    return(ui_parts)
  })


  output$specific_rules<-renderUI({
    rows.rule()
  })


  output$showpanel <- renderText({
    if(!data_ret.ev$n_rules){
      "yes"
    } else{
      "hidded"
    }
  })
  outputOptions(output, "showpanel", suspendWhenHidden = FALSE)






  observeEvent(input$wrtXML,{
    xml_list<-list()
    if(data_ret.ev$delta_next){
      data_ret.ev$delta_bef<-0
      data_ret.ev$delta_after<-"to the next event"
    }
    for (i in c(1:length(data_ret.ev$var_final))) {
      xml_list[[i]]<-c(data_ret.ev$var_long,
                       data_ret.ev$time,
                       data_ret.ev$delta_bef,
                       data_ret.ev$delta_after,
                       data_ret.ev$rule,
                       data_ret.ev$value
      )
      #check if specific rules were setted aiiungo controllo var_spec non na
      if(!is.na(data_ret.ev$var_specific)[1] & (data_ret.ev$var_final[i] %in% data_ret.ev$var_specific)){
        xml_list[[i]]<-c(data_ret.ev$var_long,
                         data_ret.ev$time,
                         all.sRules[[data_ret.ev$var_final[i]]],
                         data_ret.ev$value)
      }

    }


    #check var_long has been selected
    if(xml_list[[1]][1]=="" | xml_list[[1]][1]=="none"){
      xml_list[[1]]<-c("NA","NA","NA","NA","NA","NA")

    }

    names(xml_list)<-data_ret.ev$var_final
    #check define merge rules section:
    if(is.na(names(xml_list)[1])){
      xml_list[[1]]<-c("NA","NA","NA","NA","NA","NA")
    }
    XML.write.fun(num,data_ret.ev$key,data_ret.ev$var,xml_list,data_ret.ev$var_long,TRUE)

    sendSweetAlert(
      session = session,
      title = "Done!",
      text = "",
      type = "success"
    )


  },ignoreInit = TRUE)





}
