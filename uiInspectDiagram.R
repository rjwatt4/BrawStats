

inspectDiagram <-
  bsModal(id="inspectOutput", title="Inspect Variable", trigger="inspectButton", size = "large",
          wellPanel(
          style = paste("background: ",maincolours$graphC,";"),
          fluidRow(
            column(offset=1,width=1, 
                   fluidRow(
                     selectInput("inspectOrder","Display:",choices=c("unsorted","sorted","piled"),selected="unsorted",selectize=FALSE)
                   ),
                   fluidRow(
                     checkboxInput("showMean","Mean")
                   ),
                   fluidRow(
                     checkboxInput("showSD","Sd")
                   ),
            ),
            column(width=6,plotOutput("mainInspect")),
            column(width=2,plotOutput("penaltyInspect")),
            column(width=2,plotOutput("penalty2Inspect"))
          ),
          fluidRow(
            column(width=2,checkboxInput("showResiduals","Show Residuals")
            ),
            column(width=6,conditionalPanel(condition="input.showResiduals",
                           sliderInput("ResidVal",label=NULL,value=-0.8,min=-1,max=1, step=0.01, ticks=FALSE, width="100%")
                           )
                   ),
          ),
          fluidRow(
            column(width=3, 
                 actionButton("inspectNewSample",label="New Sample")
            ),
          )
          )
  )

