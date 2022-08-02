
  replicationTabReserve<-tabPanel("Replicate",
         style = paste("background: ",subpanelcolours$designC), 
         wellPanel(
           style = paste("background: ",subpanelcolours$designC,";"),
           tags$table(width = "100%",class="myTable",
                      tags$tr(
                        tags$td(width = "40%", tags$div(style = localStyle, "Replication:")),
                        tags$td(width = "10%", 
                                checkboxInput("sReplicationOn",label=NULL,value=0)
                        ),
                        tags$td(width = "30%", tags$div(style = localStyle, "Power:")),
                        tags$td(width = "20%", 
                                numericInput("sReplPower",label=NULL,value=0.8,min=0, max=1, step=0.1)
                        )
                      ),
                      tags$tr(
                        tags$td(width = "40%", tags$div(style = localStyle, "Sig Original:")),
                        tags$td(width = "10%", 
                                checkboxInput("sReplSigOnly",label=NULL,value=0)
                        ),
                        tags$td(width = "30%", tags$div(style = localStyle, "Repeats:")),
                        tags$td(width = "20%", 
                                numericInput("sReplRepeats",label=NULL,value=1,min=1, max=10, step=1)
                        )
                      )
           )
         )
)
  
  if (switches$doReplications){
    replicationTab<-replicationTabReserve
  } else {
    replicationTab<-c()
  }
  