panelHeight="8.1cm"

HypothesisDiagram <-
  
  wellPanel(
    style = paste("background: ",maincolours$panelC), 
    tabsetPanel(type="tabs",
                id="Hypothesis",
                tabPanel("Hypothesis",
                         style = paste("background: ",maincolours$graphC), 
                         plotOutput("HypothesisPlot",height=panelHeight,width="100%")
                )
    )
  )