
if (switches$doWorlds) {
  HypothesisDiagram <-
    
    wellPanel(
      style = paste("background: ",maincolours$panelC), 
      tabsetPanel(type="tabs",
                  id="HypothesisDiagram",
                  tabPanel("Hypothesis",
                           style = paste("background: ",maincolours$graphC), 
                           plotOutput("HypothesisPlot")
                  ),
                  tabPanel("World",
                           style = paste("background: ",maincolours$graphC), 
                           plotOutput("WorldPlot")
                  )
      ),
      width="8cm"
    )
  
} else {
HypothesisDiagram <-
  
  wellPanel(
    style = paste("background: ",maincolours$panelC), 
    tabsetPanel(type="tabs",
                id="HypothesisDiagram",
                tabPanel("Hypothesis",
                         style = paste("background: ",maincolours$graphC), 
                         plotOutput("HypothesisPlot")
                )
    ),
    width="8cm"
  )

worldDiagramReserve<-tabPanel("World",
         style = paste("background: ",maincolours$graphC), 
         plotOutput("WorldPlot")
)

}
