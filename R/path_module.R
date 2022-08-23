#'@title MODULE x STRAT KM
#'
#'@import pMineR
#'@import shiny
#'@import dplyr
#'@import shinyWidgets
#'@import DT


path_mod_ui<- function(id, tit,
                       is.fomm,
                       is.strat.var=FALSE,
                       node.list,el.data){

  ns<-NS(id)


  fluidPage(
    fluidRow(
      column(9,
             p(h3(tit)),
      ),
      column(3,
      )
    ),
    fluidRow(
      column(12,
             br()
      )
    ),

    #KAPLAN MAIER PARAM: FIRST ROW--> ID FROM & ID TO
    fluidRow(
      column(6,
             if(is.fomm){
               pickerInput(inputId =ns("id.start"), label = "select event start",
                           choices =  unique(el.data$EVENT),
                           multiple = FALSE,
                           selected = NULL,
                           options = list(
                             title = "select event")
               )
             }else{
               numericInput(ns("id.start"), label = "id node start:", value = 1,min = 0)
             }

      ),
      column(6,
             if(is.fomm){
               pickerInput(inputId =ns("id.end"), label = "select event end",
                           choices =  unique(el.data$EVENT),
                           multiple = FALSE,
                           selected = NULL,
                           options = list(
                             title = "select event ")
               )
             }else{
               # numericInput(ns("id.end"), label = "id node end:", value = NULL, min = 0)
               selectInput(inputId = ns("id.end"),
                           label = "id node end:",
                           choices = names(node.list),
                           selected = NULL,
                           multiple = TRUE
               )
             }

      )
    ),

    fluidRow(
      column(6,
             if(is.fomm){
               selectInput(inputId = ns("id.cens"),
                           label = "id node censored:",
                           choices =unique(el.data$EVENT),
                           selected = NULL,
                           multiple = TRUE)

             }else{
               selectInput(inputId = ns("id.cens"),
                           label = "id node censored:",
                           choices = names(node.list),
                           selected = NULL,
                           multiple = TRUE
               )
             }


      ),

      column(6,
            if(!is.fomm){
              materialSwitch(
                inputId = ns("cens.leaf"),
                label = "Use leaf for cens",
                status = "default",
                right = TRUE)
            }
      )
    ),

    p("Select the time window for the follow-up"),

    fluidRow(
      column(4,
             numericInput(ns("min_time"), label = "Set min time:", value = 0)
      ),
      column(4,
             numericInput(ns("max_time"), label = "Set max time:", value = 20)
      ),
      column(4,
             # switchInput(
             #   inputId = ns("inf"),
             #   label = "max",
             #   labelWidth = "20px",
             #   size= "mini"
             # ),
             materialSwitch(
               inputId = ns("inf"),
               label = "set max time",
               status = "default",
               right = TRUE)

      )
    ),

    fluidRow(
      column(12,
             selectInput(ns("um.time"),
                         label = "Select the time scale",
                         choices = c("mins","hours","days","weeks","months"),
                         selected = "days"
             )
      )
    ),


     fluidRow(
      if(is.fomm){
        column(12,
               p("you can select wich event should and should not be in between the \"starting\" and \"last\" event")
        )
      }
     ),

      fluidRow(

          column(6,
                 if(is.fomm){
                   selectInput(ns("event.between"),
                               label = "Event Between:",
                               choices = unique(el.data$EVENT),
                               multiple = TRUE)
                 }

          ),

          column(6,
                 if(is.fomm){selectInput(ns("event.NOT.between"),
                             label = "Event NOT Between:",
                             choices = unique(el.data$EVENT),
                             multiple = TRUE)}
          )

      ),

      fluidRow(
        if(is.strat.var){
          column(12,
                 br(),
                 tags$hr(),
                 p("Stratification setting:")
          )

        }
      ),


    fluidRow(


        #stratification var
        column(6,
               if(is.strat.var){pickerInput(inputId =ns("strat.var"), label = "Select variable for the stratification:",
                           choices =  colnames(el.data)[!(colnames(el.data) %in% c("ID","DATE_INI","EVENT"))],
                           multiple = FALSE,
                           selected = NULL,
                           options = list(
                             title = "select event")
               )}

        ),
        #stratification var TYPE
        column(6,
               if(is.strat.var){selectInput(ns("strat.var.type"), label ="Select the stratification variable type:",
                           choices = c("Categorical","Numeric"),
                           selected = NULL,
                           multiple = FALSE)}
        )



    ),

    #STRAT VAR VALUES
    fluidRow(


        column(6,
               if(is.strat.var){selectInput(ns("strat.value1"), label = "Select possible value fot the selected var:",
                           choices = NULL,
                           multiple = FALSE)}
        ),

        column(6,
               if(is.strat.var){selectInput(ns("strat.value2"), label = "Select possible value fot the selected var:",
                           choices = NULL,
                           multiple = TRUE)}
        )


    ),
    fluidRow(
      column(12,
             actionButton(ns("save"),label = "Save settings")
             )
    )
  )




}



path_data_server<- function(input,
                            output,
                            session,
                            el.data,
                            id,
                            is.fomm){


  ns <- session$ns

  observeEvent(input$strat.var,{
    if(input$strat.var!=""){
      shiny::updateSelectInput(
        inputId = "strat.value1",
        label = "Select possible value fot the selected var:",
        choices = unique(el.data[input$strat.var]),
        selected = NULL
      )
      shiny::updateSelectInput(
        inputId = "strat.value2",
        label = "Select possible value fot the selected var:",
        choices = unique(el.data[input$strat.var]),
        selected = NULL
      )
    }
  })

  observeEvent(input$strat.value1,{
    if(input$strat.var!=""){
      if(input$strat.var.type=="Categorical"){
        shiny::updateSelectInput(
          inputId = "strat.value2",
          label = "Select possible value fot the selected var:",
          choices = unique(el.data[input$strat.var])[!unique(el.data[input$strat.var]) %in% input$strat.value1],
          selected = NULL
        )
      }else{
        shiny::updateSelectInput(
          inputId = "strat.value2",
          label = "Select possible value fot the selected var:",
          choices = c("only for categorical var"),
          selected = NULL
        )
      }
    }


  })


  observeEvent(input$strat.var.type,{
    if(input$strat.var!=""){
      if(input$strat.var.type=="Categorical"){
        shiny::updateSelectInput(
          inputId = "strat.value1",
          label = "Select possible value fot the selected var:",
          choices = unique(el.data[input$strat.var]),
          selected = NULL
        )

        shiny::updateSelectInput(
          inputId = "strat.value2",
          label = "Select possible value fot the selected var:",
          choices = unique(el.data[input$strat.var])[!unique(el.data[input$strat.var]) %in% input$strat.value1],
          selected = NULL
        )
      }
      else if (input$strat.var.type=="Numeric"){
        shiny::updateSelectInput(
          inputId = "strat.value1",
          label = "Select possible value fot the selected var:",
          choices = c("only for categorical var"),
          selected = NULL
        )
        shiny::updateSelectInput(
          inputId = "strat.value2",
          label = "Select possible value fot the selected var:",
          choices = c("only for categorical var"),
          selected = NULL
        )
      }
    }
  })

  observeEvent(input$save,{

  if(input$inf){
    max_time<-Inf
  }else{
    max_time<-input$max_time
  }
    path.name<-substr(ns(""),1,(nchar(ns(""))-1))





  all.path[[path.name]]<<-list("id.start"   = input$id.start,
                                      "id.end"     = input$id.end,
                                      "id.cens"    = input$id.cens,
                                      "cens.leaf"  = input$cens.leaf,
                                      "min_time"   = input$min_time,
                                      "max_time"   = max_time,
                                      "um.time"    = input$um.time,
                                      "strat.var"  = input$strat.var,
                                      "strat.var.type" = input$strat.var.type,
                                      "strat.value1" = input$strat.value1,
                                      "strat.value2" = input$strat.value2
                                      )
  if(is.fomm){
    all.path[[paste0("path",id)]]["event.between"]<<-c(input$event.between)
    all.path[[paste0("path",id)]]["event.NOT.between"]<<-c(input$event.NOT.between)
  }

  })

}


# ui <- fluidPage(
#   sidebarLayout(
#     sidebarPanel(
# width = 4,
# tabsetPanel(
#   tabPanel(
#     path_mod_ui("path",tit = "path 1",node.list=s$lst.nodi,is.strat.var = TRUE,is.fomm = TRUE,el.data = all.data[[1]])
#   )
# )
#     ),
#     mainPanel()
#     )
# )
#
# server <- function(input, output, session) {
#   tab<-callModule(path_data_server,"path")
# }
#
# shinyApp(ui, server)
























