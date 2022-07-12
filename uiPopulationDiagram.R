panelWidth="6.9cm"

PopulationDiagram <-
  
  wellPanel(
    style = paste("background: ",maincolours$panelC), 
    tabsetPanel(type="tabs",
                id="Theory",
                tabPanel("Population",
                         style = paste("background: ",maincolours$graphC), 
                         plotOutput("PopulationPlot",height="5.0cm",width=panelWidth)
                         ),
                tabPanel("Prediction",
                         style = paste("background: ",maincolours$graphC), 
                         plotOutput("PredictionPlot",height="5.0cm",width=panelWidth)
                         )
    ),
    width="8cm"
  )
    