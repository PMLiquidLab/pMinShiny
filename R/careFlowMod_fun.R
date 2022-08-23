#'@title Careflow module fun
#'
#'@import shiny
#'@import dplyr
#'@import shinyWidgets
#'@import DT
#'@export



careFlow.mod<-function(){
  all.data<<-list()
  all.path<<-list()
  shinyApp(ui =fluidPage(
    #Pagina Principale
    navbarPage("pMining: CareFlow Mining", id="tabs",
               tabPanel("Loading EventLog",
                        titlePanel("EventLog Uploading"),
                        br(),
                        import_mod_ui("uploadEL","Upload EventLog file",FALSE,col_setting=TRUE),
                        # uiOutput("colDef"),
                        actionButton("loadEL","Load Event Log",width = '32%') ,

               )
    )
  ),

  server = server.careFlow)
}


