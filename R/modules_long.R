#'@title Merge module: page for ancillary data recap and both merging techniques
#'
#'@rawNamespace import(shiny,  except = c(renderDataTable,dataTableOutput))
#'@import shinyWidgets
#'@importFrom DT renderDataTable dataTableOutput




#library(shiny)
#library(shinyWidgets)
#library(DT)

ui_mod_long<-function(id,data,num){
  ns<-NS(id)
  fluidPage(
    fluidRow(
      column(12,
             strong(h3("Data recap:")),
             br(),
             DT::dataTableOutput(ns("showtab")),
             br()
      )
    ),
    fluidRow(
      br(),
      br()
    ),

    fluidRow(
      column(5,offset = 2,
             br(),
             EventUI(ns("mergetab"),data,num+1)
      ),
      column(5,
             br(),
             AttUI(ns("att.tab"),data,num+1)
      )
    )
  )
}





server_mod_long<-function(input,output,session,data,num){
  ns <- session$ns
  callModule(EventServer, "mergetab",data,num)
  callModule(AttServer,"att.tab",data,num)

  output$showtab <- DT::renderDataTable(data
                                    ,options = list(iDisplayLength = 5)
  )


}







