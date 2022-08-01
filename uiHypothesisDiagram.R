

if (switches$doWorlds) {
  HypothesisDiagram <-
    
    wellPanel(
      style=paste("min-width:", diagramPanelWidth, ";"),
      style = paste("background: ",maincolours$panelC), 
      tabsetPanel(type="tabs",
                  id="HypothesisDiagram",
                  tabPanel("Hypothesis",
                           style = paste("background: ",maincolours$graphC), 
                           plotOutput("HypothesisPlot",height=diagramUpperPanelHeight,width=diagramPanelWidth)
                  ),
                  tabPanel("World",
                           style = paste("background: ",maincolours$graphC), 
                           plotOutput("WorldPlot",height=diagramUpperPanelHeight,width=diagramPanelWidth)
                  )
      ),
      width="8cm"
    )
  
} else {
HypothesisDiagram <-
  
  wellPanel(
    style=paste("min-width:", diagramPanelWidth, ";"),
    style = paste("background: ",maincolours$panelC), 
    tabsetPanel(type="tabs",
                id="HypothesisDiagram",
                tabPanel("Hypothesis",
                         style = paste("background: ",maincolours$graphC), 
                         plotOutput("HypothesisPlot",height=diagramUpperPanelHeight,width=diagramPanelWidth)
                )
    ),
    width="8cm"
  )

worldDiagramReserve<-tabPanel("World",
         style = paste("background: ",maincolours$graphC), 
         plotOutput("WorldPlot",height=diagramUpperPanelHeight,width="100%")
)

}
