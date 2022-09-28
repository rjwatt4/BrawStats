MainReports <-
  wellPanel(
    style=paste("min-width:", graphWidth, ";"),
    style = paste("background: ",maincolours$panelC), 
    tabsetPanel(type="tabs",
                id="Reports",
                tabPanel("Sample",     
                         plotOutput("SampleReport",height=reportHeight,width=graphWidth),
                         style =paste("background:", maincolours$graphC, ";")),
                tabPanel("Describe",   
                         plotOutput("DescriptiveReport",height=reportHeight,width=graphWidth),
                         style =paste("background:", maincolours$graphC, ";")),
                tabPanel("Infer",      
                         plotOutput("InferentialReport",height=reportHeight,width=graphWidth),
                         style =paste("background:", maincolours$graphC, ";")),
                metaReportPanel,
                tabPanel("Expect",value="Expect",   
                         plotOutput("ExpectedReport",height=reportHeight,width=graphWidth),
                         style =paste("background:", maincolours$graphC, ";"))
                ,tabPanel("Explore",value="Explore",
                          plotOutput("ExploreReport",height=reportHeight,width=graphWidth),
                          style =paste("background:", maincolours$graphC, ";"))
                ,tabPanel("Possible",value="Possible",
                          plotOutput("LikelihoodReport",height=reportHeight,width=graphWidth),
                          style =paste("background:", maincolours$graphC, ";"))
    ),
    width=fullPanelWidth
  )


