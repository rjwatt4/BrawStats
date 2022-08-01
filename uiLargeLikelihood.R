source("uiDesignPart.R")

LGmodalPossible <-
  bsModal(id="LGmodalPossible", title=" ", trigger="trig", size = "large", 
          wellPanel(
            style = paste("background: ",maincolours$panelC,";"),
            fluidRow(
              style=paste0("height: ",LGPanelHeight),
              column(offset=0,width=3, 
                     wellPanel(
                       style = paste("background: ",subpanelcolours$likelihoodC,";"),
                       tags$table(width = "100%",class="myTable",style=paste("margin:0px;padding:0px;margin-left:-20px;margin-right:-20px;"),
                                  tags$tr(
                                    tags$td(width = "30%", tags$div(style = localStyle, "Possible:")),
                                    tags$td(width = "30%", 
                                            selectInput("LGshowPossible", label=NULL,
                                                        c("Samples" = "Samples",
                                                          "Populations" = "Populations"
                                                        ),selected="Populations",selectize=FALSE)
                                    ),
                                    tags$td(width = "25%"),
                                    tags$td(width = "15%"),
                                  )
                       )
                     ),
                     wellPanel(
                       style = paste("background: ",subpanelcolours$likelihoodC,";"),
                       priorPanel("LGlikelihood",asTable=TRUE),
                       tags$table(width = "100%",class="myTable",style=paste("margin:0px;padding:0px;margin-left:-20px;margin-right:-20px;"),
                                  tags$tr(
                                    tags$td(width = "40%", tags$div(style = localStyle, "Target Sample:")),
                                    tags$td(width = "30%", numericInput("LGlikelihoodSampRho", label=NULL,min=-1,max=1, step=0.1,value=0)),
                                    tags$td(width = "15%",
                                                    conditionalPanel(condition="input.LGshowPossible=='Samples'",
                                                      tags$div(style = localStyle, "cut:"))
                                            ),
                                    tags$td(width = "15%",
                                            conditionalPanel(condition="input.LGshowPossible=='Samples'",
                                                     checkboxInput("LGlikelihood_cutaway",label=NULL,value=FALSE))
                                            ),
                                    tags$td(width = "15%",
                                                    conditionalPanel(condition="input.LGshowPossible=='Populations'",
                                                      tags$div(style = localStyle, "prior:"))
                                            ),
                                    tags$td(width = "15%",
                                            conditionalPanel(condition="input.LGshowPossible=='Populations'",
                                                     checkboxInput("LGlikelihoodUsePrior",label=NULL,value=FALSE))
                                            )
                                  ),
                       ),
                       conditionalPanel(condition="input.LGshowPossible=='Samples'",
                         style = paste("background: ",subpanelcolours$likelihoodC,";"),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "25%", tags$div(style = localStyle, "Runs:")),
                                      tags$td(width = "20%", 
                                              selectInput("LGlikelihood_length", label=NULL,
                                                          SlikelihoodLengthChoices,selectize=FALSE)
                                      ),
                                      tags$td(width = "20%", tags$div(style = localStyle, "Append:")),
                                      tags$td(width = "10%", checkboxInput("LGlikelihood_append", label=NULL)),
                                      tags$td(width = "10%", actionButton("LGlikelihood_run", "Run"))
                                    )
                         ),
                       ),
                       conditionalPanel(condition="input.LGshowPossible=='Populations'",
                                        tags$table(width = "100%",class="myTable",
                                                   tags$tr(
                                                     tags$td(width = "25%", tags$div(style = localStyle, "Runs:")),
                                                     tags$td(width = "20%", 
                                                             selectInput("LGlikelihoodP_length", label=NULL,
                                                                         PlikelihoodLengthChoices,selectize=FALSE)
                                                     ),
                                                     tags$td(width = "20%", tags$div(style = localStyle, "Append:")),
                                                     tags$td(width = "10%", checkboxInput("LGlikelihoodP_append", label=NULL)),
                                                     tags$td(width = "10%", actionButton("LGlikelihoodP_run", "Run")),
                                                   )
                                        ),
                       )
                     ),
                       wellPanel(
                         style = paste("background: ",subpanelcolours$likelihoodC,";"),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "30%",  tags$div(style = labelStyle, "Options:")),
                                      tags$td(width = "30%"),
                                      tags$td(width = "20%"),
                                      tags$td(width = "20%")
                                    ),
                                    tags$tr(
                                      tags$td(width = "30%", tags$div(style = localStyle, "show theory:")),
                                      tags$td(width = "30%", checkboxInput("LGlikelihoodTheory", value=FALSE, label=NULL)),
                                      tags$td(width = "20%"),
                                      tags$td(width = "20%")
                                    )
                         ),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "30%", tags$div(style = localStyle, "longhand:")),
                                      tags$td(width = "5%", checkboxInput("LGlongHandLikelihood", value=TRUE, label=NULL)),
                                      tags$td(width = "40%", tags$div(style = localStyle, "sim slice:")),
                                      tags$td(width = "15%",numericInput("LGlikelihoodSimSlice",label=NULL,value=0.1,max=0.2,min=0.0001,step=0.01)),
                                      tags$td(width = "10%", checkboxInput("LGlikelihoodCorrection", value=FALSE, label=NULL)),
                                    )
                         ),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "15%", tags$div(style = localStyle, "view:")),
                                      tags$td(width = "20%", 
                                              selectInput("LGlikelihoodView", label=NULL,
                                                          c("3D" = "3D",
                                                            "2D" = "2D"),selectize=FALSE)
                                      ),
                                      tags$td(width = "5%", tags$div(style = localStyle, "az:")),
                                      tags$td(width = "15%", 
                                              numericInput("LGlikelihoodAzimuth",label=NULL,
                                                           min = -180,
                                                           max = 180,
                                                           step = 5,
                                                           value = 35)
                                      ),
                                      tags$td(width = "5%", tags$div(style = localStyle, "elev:")),
                                      tags$td(width = "15%", 
                                              numericInput("LGlikelihoodElevation",label=NULL,
                                                           min = 0,
                                                           max = 90,
                                                           step = 5,
                                                           value = 15)
                                      ),
                                      tags$td(width = "5%", tags$div(style = localStyle, "r:")),
                                      tags$td(width = "20%", 
                                              numericInput("LGlikelihoodRange",label=NULL,
                                                           min = 0,
                                                           max = 10000,
                                                           step = 100,
                                                           value = 1000)
                                      ),
                                    )
                         )
                         ),
                     designPanel("LGlikelihood"),
                     wellPanel(
                           style = paste("background: ",subpanelcolours$likelihoodC,";"),
                           tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "15%", tags$div(style = localStyle, " ")),
                                      tags$td(width = "20%",actionButton("LGlikelihoodClose","Close")),
                                    )
                           )
                         )
              ),# close of column
              column(width=9,plotOutput("LGshowPossibleOutput",height=LGGraphHeight))
            ) 
          )
  )
LGmodalPossible$attribs$`data-backdrop` <- "static"
# LGmodalPossible[[2]]$`data-backdrop` = "static"
# LGmodalPossible[[2]]$`data-keyboard` = "false"

