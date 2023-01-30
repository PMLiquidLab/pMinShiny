#'@title FOMM module fun
#'
#'@rawNamespace import(shiny, except = c(renderDataTable,dataTableOutput))
#'@import dplyr
#'@import shinyWidgets
#'@importFrom DT renderDataTable dataTableOutput
#'@importFrom shinyjs useShinyjs
#'@export





FOMM.mod<-function(){
  all.data<<-list()
  all.path<<-list()
  shinyApp(ui =fluidPage(
    #Pagina Principale
    navbarPage("pMinShiny: First Order Markov Model", id="tabs",
               tabPanel("Loading EventLog",
                        titlePanel("EventLog Uploading"),
                        br(),
                        import_mod_ui("uploadEL","Upload EventLog file",FALSE,col_setting=TRUE),
                        shinyjs::useShinyjs(),
                        actionButton("loadEL","Load Event Log",width = '32%') ,

               )
    )
  ),

  server = server.FOMM)
}
