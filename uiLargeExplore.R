source("uiEffectPart.R")
source("uiDesignPart.R")


LGmodalExplore <-
  bsModal(id="LGmodalExplore", title=" ", trigger="trig", size = "large", 
          wellPanel(
            style = paste("background: ",maincolours$panelC,";"),
            fluidRow(
              style=paste0("height: ",LGPanelHeight),
              column(offset=0,width=3, 
                     wellPanel(
                       style = paste("background: ",subpanelcolours$exploreC,";"),
                       tags$table(width = "100%",class="myTable",style=paste("margin:0px;padding:0px;margin-left:-20px;margin-right:-20px;"),
                                  tags$tr(
                                    tags$td(width = "30%", tags$div(style = localStyle, "Explore:")),
                                    tags$td(width = "40%", 
                                            selectInput("LGExploreShow", label=NULL,
                                                        c("Hypothesis" = "Hypothesis",
                                                          "Design" = "Design"
                                                        ),selected="Hypothesis",selectize=FALSE)
                                    ),
                                    tags$td(width = "15%"),
                                    tags$td(width = "15%"),
                                  ),
                       )
                     ),
                     wellPanel(
                       style = paste("background: ",subpanelcolours$exploreC,";"),
                       conditionalPanel("input.LGExploreShow=='Hypothesis'",
                                        tags$table(width = "100%",class="myTable",
                                                   tags$tr(
                                                     tags$td(width = "10%", tags$div(style = localStyle, "Vary:")),
                                                     tags$td(width = "40%", 
                                                             selectInput("LGExplore_typeH",label=NULL,
                                                                         hypothesisChoices3,selectize=FALSE)
                                                     ),
                                                     tags$td(width = "25%", 
                                                             conditionalPanel(condition="input.LGExplore_typeH == 'IV' || input.LGExplore_typeH == 'DV' || input.LGExplore_typeH == 'IV2'",
                                                                              selectInput("LGExplore_VtypeH",label=NULL,
                                                                                          variableChoices,selectize=FALSE)
                                                             )
                                                     ),
                                                   ),
                                                   tags$tr(
                                                     tags$td(width = "10%", tags$div(style = localStyle, "Show:")),
                                                     tags$td(width = "40%", 
                                                             selectInput("LGExplore_showH", label=NULL,
                                                                         showChoices,selectize = FALSE)
                                                     ),
                                                     tags$td(width = "25%", 
                                                             conditionalPanel(condition="input.IV2choice != 'none'",
                                                                              selectInput("LGExplore_whichShowH", label=NULL,
                                                                                          whichShowChoices, selected="Main 1",selectize = FALSE)
                                                             )),
                                                     tags$td(width = "25%", 
                                                             conditionalPanel(condition="input.IV2choice != 'none'",
                                                                              selectInput("LGExplore_typeShowH", label=NULL,
                                                                                          extraShowChoices, selected="direct",selectize = FALSE)
                                                             ))
                                                   )
                                        ),
                                        tags$table(width = "100%",class="myTable",
                                                   tags$tr(
                                                     tags$td(width = "10%", tags$div(style = localStyle, "Runs:")),
                                                     tags$td(width = "30%", 
                                                             selectInput("LGExplore_lengthH", label=NULL,
                                                                         exploreLengthChoices,selectize=FALSE)
                                                     ),
                                                     tags$td(width = "10%", tags$div(style = localStyle, "Append:")),
                                                     tags$td(width = "10%", checkboxInput("LGExploreAppendH", label=NULL)),
                                                     tags$td(width = "20%", actionButton("LGexploreRunH", "Run"))
                                                   )
                                        )
                       ),
                       conditionalPanel("input.LGExploreShow=='Design'",
                                        tags$table(width = "100%",class="myTable",
                                                   tags$tr(
                                                     tags$td(width = "10%", tags$div(style = localStyle, "Vary:")),
                                                     tags$td(width = "40%", 
                                                             selectInput("LGExplore_typeD",label=NULL,
                                                                         designChoices,selectize=FALSE)
                                                     ),
                                                     tags$td(id="LGExplore_nRangeLabel",width = "15%", 
                                                             conditionalPanel(condition="input.LGExplore_typeD == 'SampleSize' || input.LGExplore_typeD == 'Repeats'",
                                                                              tags$div(style = localStyle, "max:")
                                                             )),
                                                     tags$td(width = "15%", 
                                                             conditionalPanel(condition="input.LGExplore_typeD == 'SampleSize' || input.LGExplore_typeD == 'Repeats'",
                                                                              numericInput("LGExplore_nRange", label=NULL,value=250,min=10,step=50)
                                                             )),
                                                     tags$td(width = "10%", 
                                                             conditionalPanel(condition="input.LGExplore_typeD == 'SampleSize'",
                                                                              tags$div(style = localStyle, "log")
                                                                              )),
                                                     tags$td(width = "10%", 
                                                             conditionalPanel(condition="input.LGExplore_typeD == 'SampleSize'",
                                                                              checkboxInput("LGExplore_xlog",label="",value=FALSE)
                                                                              )),
                                                   ),
                                                   tags$tr(
                                                     tags$td(width = "10%", tags$div(style = localStyle, "Show:")),
                                                     tags$td(width = "40%", 
                                                             selectInput("LGExplore_showD", label=NULL,
                                                                         showChoices,width="100%",selectize = FALSE)
                                                     ),
                                                     tags$td(width = "15%", 
                                                             conditionalPanel(condition="input.IV2choice != 'none'",
                                                                              selectInput("LGExplore_whichShowD", label=NULL,
                                                                                          whichShowChoices, selected="Main 1",selectize = FALSE)
                                                             )),
                                                     tags$td(width = "15%", 
                                                             conditionalPanel(condition="input.IV2choice != 'none'",
                                                                              selectInput("LGExplore_typeShowD", label=NULL,
                                                                                          extraShowChoices, selected="direct",selectize = FALSE)
                                                             )),
                                                     tags$td(width = "10%", tags$div(style = localStyle, "")),
                                                     tags$td(width = "10%", tags$div(style = localStyle, ""))
                                                   )
                                        ),
                                        tags$table(width = "100%",class="myTable",
                                                   tags$tr(
                                                     tags$td(width = "10%", tags$div(style = localStyle, "Runs:")),
                                                     tags$td(width = "30%", 
                                                             selectInput("LGExplore_lengthD", label=NULL,
                                                                         exploreLengthChoices,selectize=FALSE)
                                                     ),
                                                     tags$td(width = "10%", tags$div(style = localStyle, "Append:")),
                                                     tags$td(width = "10%", checkboxInput("LGExploreAppendD", label=NULL)),
                                                     tags$td(width = "20%", actionButton("LGexploreRunD", "Run"))
                                                   )
                                        )
                       ),
                     ),
                     wellPanel(
                       style = paste("background: ",subpanelcolours$exploreC,";"),
                       tags$table(width = "100%",class="myTable",
                                  tags$tr(
                                    tags$td(width = "25%", tags$div(style = labelStyle, "Options:")),
                                    tags$td(width = "15%"),
                                    tags$td(width = "30%"),
                                    tags$td(width = "20%"),
                                    tags$td(width="5%"),
                                    tags$td(width="5%"),
                                  ),
                                  tags$tr(
                                    tags$td(width = "25%", tags$div(style = localPlainStyle, "no points:")),
                                    tags$td(width = "15%", 
                                            numericInput("LGExplore_npoints", label=NULL,value=13)
                                    ),
                                    tags$td(width = "30%", id="Explore_esRangeLabel", tags$div(style = localPlainStyle, "r-range:")),
                                    tags$td(width = "20%", 
                                            numericInput("LGExplore_esRange", label=NULL,value=0.8)
                                    ),
                                    tags$td(width="5%"),
                                    tags$td(width="5%"),
                                  ),
                                  tags$tr(
                                    tags$td(width = "25%", tags$div(style = localPlainStyle, "quantiles:")),
                                    tags$td(width = "15%", 
                                            numericInput("LGExplore_quants", label=NULL,value=0.95, step = 0.01,min=0.01,max=0.99)
                                    ),
                                    tags$td(width = "30%"),
                                    tags$td(width = "20%"),
                                    tags$td(width="5%"),
                                    tags$td(width="5%")
                                  ),
                                  tags$tr(
                                    tags$td(width = "25%", tags$div(style = localPlainStyle, "full y-lim:")),
                                    tags$td(width = "15%", checkboxInput("LGExploreFull_ylim", label=NULL,value=FALSE)),
                                    tags$td(width = "30%", tags$div(style = localPlainStyle, "anom-range:")),
                                    tags$td(width = "20%", 
                                            numericInput("LGExplore_anomRange", label=NULL,value=0.9)
                                    ),
                                    tags$td(width="5%"),
                                    tags$td(width="5%")
                                  )
                       )
                     ),
                     wellPanel(
                       style = paste("background: ",subpanelcolours$hypothesisC,";"),
                       effectPanel("LGExplore",asTable = TRUE),
                       worldPanel("LGExplore",asTable = TRUE),
                     ),
                     designPanel("LGExplore"),
                     wellPanel(
                       style = paste("background: ",subpanelcolours$exploreC,";"),
                       tags$table(width = "100%",class="myTable",
                                  tags$tr(
                                    tags$td(width = "15%", tags$div(style = localStyle, " ")),
                                    tags$td(width = "20%",actionButton("LGExploreClose","Close")),
                                  )
                       )
                     )
              ),# close of column
              column(width=9,plotOutput("LGExploreShowOutput",height=LGGraphHeight))
            ) 
          )
  )
LGmodalExplore$attribs$`data-backdrop` <- "static"
# LGmodalPossible[[2]]$`data-backdrop` = "static"
# LGmodalPossible[[2]]$`data-keyboard` = "false"

