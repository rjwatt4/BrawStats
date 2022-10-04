
if (switches$doMetaAnalysis) {
  metaGraphPanel<-
    tabPanel("MetaAnalysis",class="Graphs",
             tags$table(width = "100%",class="myTable",
                        tags$tr(
                          tags$td(width = "97%",plotOutput("MetaAnalysisPlot",height=graphHeight,width="100%")),
                          tags$td(width = "3%",valign="top",actionButton("LGMetaStart",label=expandLabel)),
                        )
             ),
             style =paste("background:", maincolours$graphC, ";")
    )
  metaReportPanel<-
    tabPanel("MetaAnalysis",class="Graphs",
             tags$table(width = "100%",class="myTable",
                        tags$tr(
                          tags$td(width = "97%",plotOutput("MetaAnalysisReport",height=reportHeight,width="100%")),
                        )
             ),
             style =paste("background:", maincolours$graphC, ";")
    )
  
} else {
  metaGraphPanel<-c()
  metaReportPanel<-c()
}
