panelHeight="8.1cm"
panelWidth="6.9cm"


if (switches$doWorlds) {
  HypothesisDiagram <-
    
    wellPanel(
      style=paste("min-width:", panelWidth, ";"),
      style = paste("background: ",maincolours$panelC), 
      tabsetPanel(type="tabs",
                  id="HypothesisDiagram",
                  tabPanel("Hypothesis",
                           style = paste("background: ",maincolours$graphC), 
                           plotOutput("HypothesisPlot",height=panelHeight,width=panelWidth)
                  ),
                  tabPanel("World",
                           style = paste("background: ",maincolours$graphC), 
                           plotOutput("WorldPlot",height=panelHeight,width=panelWidth)
                  )
      ),
      width="8cm"
    )
  
} else {
HypothesisDiagram <-
  
  wellPanel(
    style=paste("min-width:", panelWidth, ";"),
    style = paste("background: ",maincolours$panelC), 
    tabsetPanel(type="tabs",
                id="HypothesisDiagram",
                tabPanel("Hypothesis",
                         style = paste("background: ",maincolours$graphC), 
                         plotOutput("HypothesisPlot",height=panelHeight,width=panelWidth)
                )
    ),
    width="8cm"
  )

worldDiagramReserve<-tabPanel("World",
         style = paste("background: ",maincolours$graphC), 
         plotOutput("WorldPlot",height=panelHeight,width="100%")
)

}
