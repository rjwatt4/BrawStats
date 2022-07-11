# world tab
worldTabReserve<-tabPanel("World",value="World",
         style = paste("background: ",subpanelcolours$hypothesisC),
         wellPanel(
           style = paste("background: ",subpanelcolours$hypothesisC,";"),
           tags$table(width = "100%",class="myTable",
                      tags$tr(
                        tags$td(width = "40%", tags$div(style = localStyle, "Population PDF:")),
                        tags$td(width = "30%",
                                selectInput("World_distrP", label=NULL,
                                            c("Single" = "Single",
                                              "Uniform" = "Uniform",
                                              "Gauss"="Gauss",
                                              "Exp" = "Exp"),selectize=FALSE)
                        ),
                        tags$td(width = "15%",
                                selectInput("World_RZ", label=NULL,
                                            c("r" = "r",
                                              "z" = "z"),selectize=FALSE)
                        ),
                        tags$td(width = "15%",
                                conditionalPanel(condition="input.World_distrP!=='Uniform' && input.World_distrP!=='Single'",
                                                 numericInput("World_distr_kP",label=NULL,
                                                              min = 0,
                                                              max = 1,
                                                              step = 0.05,
                                                              value = 0.2)
                                )
                        )
                      ),
                      tags$tr(
                        tags$td(width = "40%", tags$div(style = localStyle, "p(null):")),
                        tags$td(width = "30%", numericInput("World_Nullp", label=NULL,min=0,max=1, step=0.025,value=0)),
                        tags$td(width = "15%"),
                        tags$td(width = "15%")
                      )
           ),
           width="100%"
         )
)
if (switches$doWorlds){
  worldTab<-worldTabReserve
} else {
  worldTab<-c()
}
