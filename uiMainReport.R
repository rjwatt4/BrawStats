MainReports <-
  wellPanel(
    style=paste("min-width:", panelWidth, ";"),
    style = paste("background: ",maincolours$panelC), 
    tabsetPanel(type="tabs",
                id="Reports",
                tabPanel("Sample",     
                         plotOutput("SampleReport",height="5cm",width=panelWidth),
                         style =paste("background:", maincolours$graphC, ";")),
                tabPanel("Describe",   
                         plotOutput("DescriptiveReport",height="5cm",width=panelWidth),
                         style =paste("background:", maincolours$graphC, ";")),
                tabPanel("Infer",      
                         plotOutput("InferentialReport",height="5cm",width=panelWidth),
                         style =paste("background:", maincolours$graphC, ";")),
                tabPanel("Expect",value="Expect",   
                         plotOutput("ExpectedReport",height="5cm",width=panelWidth),
                         style =paste("background:", maincolours$graphC, ";"))
                ,tabPanel("Explore",value="Explore",
                          plotOutput("ExploreReport",height="5cm",width=panelWidth),
                          style =paste("background:", maincolours$graphC, ";"))
                ,tabPanel("Possible",value="Possible",
                          plotOutput("LikelihoodReport",height="5cm",width=panelWidth),
                          style =paste("background:", maincolours$graphC, ";"))
    ),
    width="16cm"
  )


