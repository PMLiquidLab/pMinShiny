#'@title Launch grouping module
#'
#'@export



group.mod<-function(){
  all.data<<-list()
  dict.names<<-list()
  all.dict<<-list()
  shinyApp(ui= navbarPage("pMining: Grouping Module", id="tabs",

                          tabPanel("Loading Data",
                                   titlePanel("Data Uploading"),
                                   br(),
                                   import_mod_ui("uploadEL","Upload EventLog file",FALSE),
                                   actionButton("loadEL","Load Event Log",width = '32%') ,
                          )),
           server.group)
}
