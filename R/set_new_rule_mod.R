#'@title Merge module: Merge by attribute specific rules settings
#'
#'@import shiny
#'@import shinyWidgets


spec_setting_ui<-function(id,var.name,var_long_type){
  ns<-NS(id)

  fluidRow(
    tags$hr(),
    column(3,
           p(h5("Select specifi rules for: ",strong(var.name))),
    ),
    column(3,
           sliderInput(ns("delta_before"), label = "Select Delta before", min = 0,
                       max = 30, value = 10)

    ),

    column(3,
           sliderInput(ns("delta_after"), label = "Select Delta after", min = 0,
                       max = 30, value = 10),
           checkboxInput(ns("delta_next"), label = "to the next event", value = FALSE)
    ),

    column(3,
           pickerInput(ns("rule.new"),"Select rule",choices = c("mean","min value","max value","last measure","first measure","count","trend"), selected = NULL)
    ),

  )


}

spec_setting_server<-function(input,output,session,filename,var.name){
  ns<-session$ns
  data_input<-reactiveValues(
    delta_b=array(),
    delta_a=array(),
    rule=array(),
    delta_next=FALSE
  )

  observeEvent(input$delta_before,{
    data_input$delta_b<-input$delta_before
    # all.sRules[[var.name]]<<-c(data_input$delta_b,data_input$delta_a,data_input$rule)
    if(data_input$delta_next){
      all.sRules[[var.name]]<<-c("0","to the next event",data_input$rule)
    }else{
      all.sRules[[var.name]]<<-c(data_input$delta_b,data_input$delta_a,data_input$rule)
    }
  })

  observeEvent(input$delta_after,{
    data_input$delta_a<-input$delta_after
    # all.sRules[[var.name]]<<-c(data_input$delta_b,data_input$delta_a,data_input$rule)
    if(data_input$delta_next){
      all.sRules[[var.name]]<<-c("0","to the next event",data_input$rule)
    }else{
      all.sRules[[var.name]]<<-c(data_input$delta_b,data_input$delta_a,data_input$rule)
    }
  })

  observeEvent(input$delta_next,{
    data_input$delta_next<-input$delta_next
    if(data_input$delta_next){
      all.sRules[[var.name]]<<-c("0","to the next event",data_input$rule)
    }else{
      all.sRules[[var.name]]<<-c(data_input$delta_b,data_input$delta_a,data_input$rule)
    }
  })


  observeEvent(input$rule.new,{
    data_input$rule<-input$rule.new
    # all.sRules[[var.name]]<<-c(data_input$delta_b,data_input$delta_a,data_input$rule)
    if(data_input$delta_next){
      all.sRules[[var.name]]<<-c("0","to the next event",data_input$rule)
    }else{
      all.sRules[[var.name]]<<-c(data_input$delta_b,data_input$delta_a,data_input$rule)
    }

  })

}





