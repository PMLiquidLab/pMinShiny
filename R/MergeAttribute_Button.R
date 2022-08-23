#'@title Merge module: Merge by attribute button
#'
#'@import shiny
#'@import dplyr
#'@import shinyWidgets


AttUI <- function(id,data,num){
  ns <- NS(id)

  tagList(

    conditionalPanel(condition = 'output.showpanel == "yes"', ns = ns ,
                     absolutePanel(
                       # top = 20,
                       left = 50,
                       right = 50,
                       bottom = 500,
                       width = 900, height = 100,
                       draggable = TRUE,
                       style = "opacity: 1; z-index: 10;" ,
                       wellPanel(style = "overflow-y:scroll;
                                          background: #F5F5F5;
                                          max-height: 800px; height: 900px",
                                 fluidPage(
                                   fluidRow(
                                     column(11,
                                            titlePanel("Merge by Attribute"),
                                     ),
                                     column(1,
                                            br(),
                                            actionButton(ns("close"), label= "",icon = icon("fas fa-window-close"))
                                     ),
                                   ),
                                   fluidRow(
                                     column(12,
                                            tags$hr()
                                     )
                                   )

                                 ),


                                 ui_regole(ns("att"),data,num),



                       )

                     ),


    ),

    absolutePanel(
      bottom = 35,
      # right = 25,
      width = 15,
      height = 15,
      actionButton(ns("chatButton"), "MERGE BY ATTRIBUTE"))

  )}



AttServer <- function(input, output, session,data,num){
  rv.mergeEV <- reactiveValues(show.panel = FALSE)

  observeEvent(input$chatButton, ({
    rv.mergeEV$show.panel <- !(rv.mergeEV$show.panel)
  }))


  observeEvent(input$close,{
    rv.mergeEV$show.panel <- !(rv.mergeEV$show.panel)
  })

  output$showpanel <- renderText({
    if(rv.mergeEV$show.panel){
      "yes"
    } else{
      "hidded"
    }
  })

  callModule(server_regole,"att",data,num)
  outputOptions(output, "showpanel", suspendWhenHidden = FALSE)



}










