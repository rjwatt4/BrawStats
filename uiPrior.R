# world tab
priorPanel<-function(prefix="",asTable=FALSE) {
  wellPanel(
    style = paste("background: ",subpanelcolours$likelihoodC,";"),
    tags$table(width = "100%",class="myTable",
               tags$tr(
                 tags$td(width = "40%", tags$div(style = localStyle, "Population PDF:")),
                 tags$td(width = "30%",
                         selectInput(paste0(prefix, "Prior_distr"), label=NULL,
                                     c("Single" = "Single",
                                       "Uniform" = "Uniform",
                                       "Gauss"="Gauss",
                                       "Exp" = "Exp"),
                                     selected="Uniform",
                                     width="100%",selectize=FALSE)
                 ),
                 tags$td(width = "15%",
                         selectInput(paste0(prefix,"Prior_distr_rz"), label=NULL,
                                     c("r" = "r",
                                       "z" = "z"),
                                     selected="r",
                                     width="100%",selectize=FALSE)
                 ),
                 tags$td(width = "15%",
                         conditionalPanel(condition=paste0("input.",prefix,"Prior_distr!=='Uniform'"),
                                          numericInput(paste0(prefix, "Prior_distr_k"),label=NULL,
                                                       min = 0,
                                                       max = 1,
                                                       step = 0.05,
                                                       value = 0.2)
                         )
                 )
               ),
               tags$tr(
                 tags$td(width = "40%", tags$div(style = localStyle, "p(null):")),
                 tags$td(width = "30%", numericInput(paste0(prefix, "Prior_Nullp"), label=NULL,min=0,max=1, step=0.025,value=0)),
                 tags$td(width = "15%"),
                 tags$td(width = "15%")
               )
    ),
    width="100%"
  )
 }
