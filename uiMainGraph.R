source("uiMetaGraph.R")


MainGraphs <-
  
  wellPanel(
    style=paste("min-width:", graphWidth, ";"),
    style = paste("background: ",maincolours$panelC), 
    tabsetPanel(type="tabs",
                id="Graphs",
                tabPanel("Sample",class="Graphs",
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "97%",plotOutput("SamplePlot",height=graphHeight,width="100%")),
                                      tags$td(width = "3%",valign="top",actionButton("LGEvidenceStart",label=expandLabel)),
                                    )
                         ),
                         style =paste("background:", maincolours$graphC, ";")
                ),
                tabPanel("Describe",class="Graphs",
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "97%",plotOutput("DescriptivePlot",height=graphHeight,width="100%")),
                                      tags$td(width = "3%",valign="top",actionButton("LGEvidenceStart1",label=expandLabel)),
                                    )
                         ),
                         style =paste("background:", maincolours$graphC, ";")
                ),
                tabPanel("Infer",
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "97%",plotOutput("InferentialPlot",height=graphHeight,width="100%")),
                                      tags$td(width = "3%",valign="top",actionButton("LGEvidenceStart2",label=expandLabel)),
                                    )
                         ),
                         style =paste("background:", maincolours$graphC, ";"),
                ),
                tabPanel("Expect",value="Expect", 
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "97%",plotOutput("ExpectedPlot",height=graphHeight,width="100%")),
                                              tags$td(width = "3%",valign="top",actionButton("LGEvidenceStart3",label=expandLabel)),
                                    )
                         ),
                         style =paste("background:", maincolours$graphC, ";")
                ),
                metaGraphPanel
                ,tabPanel("Explore",value="Explore",
                          tags$table(width = "100%",class="myTable",
                                     tags$tr(
                                       tags$td(width = "97%",plotOutput("ExplorePlot",height=graphHeight,width="100%")),
                                       tags$td(width = "3%",valign="top",actionButton("LGExploreStart",label=expandLabel)),
                                     )
                          ),
                         style =paste("background:", maincolours$graphC, ";")
                )
                ,tabPanel("Possible",value="Possible",
                          tags$table(width = "100%",class="myTable",
                                     tags$tr(
                                       tags$td(width = "97%",plotOutput("LikelihoodPlot",height=graphHeight,width="100%")),
                                       tags$td(width = "3%",valign="top",actionButton("LGPossibleStart",label=expandLabel)),
                                     )
                          ),
                         style =paste("background:", maincolours$graphC, ";")
                )
    ),
    width=fullPanelWidth
  )
