#'@title Launch merge module
#'
#'@export


#lancio app
launch.fun<-function(){
all.data<<-list()
all.key<<-list()
all.xml<<-list()
all.sRules<<-list()
shinyApp(ui = fluidPage(
  #Pagina Principale
  navbarPage("pMinShiny: EventLog Enrichment Module", id="tabs",
             tabPanel("Loading EventLog",
                      titlePanel("Event Log Loading"),
                      br(),
                      import_mod_ui("uploadEL","Upload EventLog file",TRUE),
                      actionButton("loadEL","Load Event Log",width = '32%') ,
             )
  )
),
         server = server)
}




