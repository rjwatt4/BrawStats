# world tab

worldPanel<-function(prefix="",asTable=FALSE,doAnyway=FALSE) {
  
  worldTable<-
    tags$table(width = "100%",class="myTable",
                                       tags$tr(
                                         tags$td(width = "40%", tags$div(style = localStyle, "Population PDF:")),
                                         tags$td(width = "30%",
                                                 selectInput(paste0(prefix, "world_distr"), label=NULL,
                                                             c("Single" = "Single",
                                                               "Uniform" = "Uniform",
                                                               "Gauss"="Gauss",
                                                               "Exp" = "Exp"),width="100%",selectize=FALSE)
                                         ),
                                         tags$td(width = "15%",
                                                 selectInput(paste0(prefix,"world_distr_rz"), label=NULL,
                                                             c("r" = "r",
                                                               "z" = "z"),width="100%",selectize=FALSE)
                                         ),
                                         tags$td(width = "15%",
                                                 conditionalPanel(condition=paste0("input.",prefix,"world_distr!=='Uniform' && input.",prefix,"world_distr!=='Single'"),
                                                                  numericInput(paste0(prefix, "world_distr_k"),label=NULL,
                                                                               min = 0,
                                                                               max = 1,
                                                                               step = 0.05,
                                                                               value = 0.2)
                                                 )
                                         )
                                       ),
                                       tags$tr(
                                         tags$td(width = "40%", tags$div(style = localStyle, "p(null):")),
                                         tags$td(width = "30%", numericInput(paste0(prefix, "world_Nullp"), label=NULL,min=0,max=1, step=0.025,value=0)),
                                         tags$td(width = "15%"),
                                         tags$td(width = "15%")
                                       )
    )
  
  if (!asTable) {
    worldTable<-tabPanel("World",value="World",
                         style = paste("background: ",subpanelcolours$hypothesisC),
                         worldTable)
  }
  if (switches$doWorlds || doAnyway){
    return(worldTable)
  } else {
    return(c())
  }
}

