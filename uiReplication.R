
if (switches$doReplications){
  replicationTab<-tabPanel("Replicate",
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
                          tags$td(width = "40%", tags$div(style = localStyle, "Sig Only:")),
                          tags$td(width = "10%", 
                                  checkboxInput("sReplSigOnly",label=NULL,value=0)
                          )
                        )
           )
         )
)
} else {
  replicationTab<-c()
}