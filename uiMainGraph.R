
panelWidth="14.5cm"
halfWidth="7cm"
halfWidth="7cm"
panelHeight="8.1cm"


MainGraphs <-
  
  wellPanel(
    style=paste("min-width:", panelWidth, ";"),
    style = paste("background: ",maincolours$panelC), 
    tabsetPanel(type="tabs",
                id="Graphs",
                tabPanel("Sample",class="Graphs",
                         plotOutput("SamplePlot",height=panelHeight,width=panelWidth),
                         style =paste("background:", maincolours$graphC, ";")
                ),
                tabPanel("Describe",class="Graphs",
                         plotOutput("DescriptivePlot",height=panelHeight,width=panelWidth),
                         style =paste("background:", maincolours$graphC, ";")
                ),
                tabPanel("Infer",
                         plotOutput("InferentialPlot",height=panelHeight,width=panelWidth),
                         style =paste("background:", maincolours$graphC, ";"),
                ),
                tabPanel("Expect",value="Expect", 
                         plotOutput("ExpectedPlot",height=panelHeight,width=panelWidth),
                         style =paste("background:", maincolours$graphC, ";")
                )
                ,tabPanel("Explore",value="Explore",
                         plotOutput("ExplorePlot",height=panelHeight,width=panelWidth),
                         style =paste("background:", maincolours$graphC, ";")
                )
                ,tabPanel("Possible",value="Possible",
                         plotOutput("LikelihoodPlot",height=panelHeight,width=panelWidth),
                         style =paste("background:", maincolours$graphC, ";")
                )
    ),
    width="16cm"
  )
