
if (switches$doLikelihood) {
  possibleGraphPanel<-
    tabPanel("Possible",class="Graphs",
             tags$table(width = "100%",class="myTable",
                        tags$tr(
                          tags$td(width = "97%",plotOutput("LikelihoodPlot",height=graphHeight,width="100%")),
                          tags$td(width = "3%",valign="top",actionButton("LGPossibleStart",label=expandLabel)),
                        )
             ),
             style =paste("background:", maincolours$graphC, ";")
    )
  possibleReportPanel<-
    tabPanel("MetaAnalysis",class="Graphs",
             tags$table(width = "100%",class="myTable",
                        tags$tr(
                          tags$td(width = "97%",plotOutput("LikelihoodReport",height=reportHeight,width="100%")),
                        )
             ),
             style =paste("background:", maincolours$graphC, ";")
    )
  
} else {
  possibleGraphPanel<-c()
  possibleReportPanel<-c()
}
