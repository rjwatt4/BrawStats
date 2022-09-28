
PopulationDiagram <-
  
  wellPanel(
    style = paste("background: ",maincolours$panelC), 
    tabsetPanel(type="tabs",
                id="Theory",
                tabPanel("Population",
                         style = paste("background: ",maincolours$graphC), 
                         plotOutput("PopulationPlot",height=diagramLowerPanelHeight,width=diagramPanelWidth)
                         ),
                tabPanel("Prediction",
                         style = paste("background: ",maincolours$graphC), 
                         plotOutput("PredictionPlot",height=diagramLowerPanelHeight,width=diagramPanelWidth)
                         )
    ),
    width="8cm"
  )
    