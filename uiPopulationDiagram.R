PopulationDiagram <-
  
  wellPanel(
    style = paste("background: ",maincolours$panelC), 
    tabsetPanel(type="tabs",
                id="Theory",
                tabPanel("Population",plotOutput("PopulationPlot",height="5.0cm",width="100%")),
                tabPanel("Prediction",plotOutput("PredictionPlot",height="5.0cm",width="100%"))
    )
  )
    