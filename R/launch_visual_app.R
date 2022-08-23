#'@title Launch descriptive module
#'
#'@export



visual.mod<-function(){
  all.data<<-list()
  shinyApp(ui = tagList(
    fluidPage(
      navbarPage("pMining: EventLog Visual Analysis", id="tabs",
                 tabPanel("Loading Data",
                          titlePanel("Data Uploading"),
                          br(),
                          import_mod_ui_visual("uploadEL","Upload EventLog file",FALSE, col_setting=TRUE),
                          actionButton("loadEL","Load Event Log",width = '32%') ,
                 )
      )
    )
  ),
           server = server.descr)
}


