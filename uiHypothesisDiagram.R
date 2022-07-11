panelHeight="8.1cm"


if (switches$doWorlds) {
  HypothesisDiagram <-
    
    wellPanel(
      style = paste("background: ",maincolours$panelC), 
      tabsetPanel(type="tabs",
                  id="HypothesisDiagram",
                  tabPanel("Hypothesis",
                           style = paste("background: ",maincolours$graphC), 
                           plotOutput("HypothesisPlot",height=panelHeight,width="100%")
                  ),
                  tabPanel("World",
                           style = paste("background: ",maincolours$graphC), 
                           plotOutput("WorldPlot",height=panelHeight,width="100%")
                  )
      )
    )
  
} else {
HypothesisDiagram <-
  
  wellPanel(
    style = paste("background: ",maincolours$panelC), 
    tabsetPanel(type="tabs",
                id="HypothesisDiagram",
                tabPanel("Hypothesis",
                         style = paste("background: ",maincolours$graphC), 
                         plotOutput("HypothesisPlot",height=panelHeight,width="100%")
                )
    )
  )
}
