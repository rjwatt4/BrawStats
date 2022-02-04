DesignTab <-
  wellPanel(
    style = paste("background: ",panelcolours$designC), 
    # h5("Design"),
    fluidRow(headerText("Design the sample: size & method")),
    tabsetPanel(id="Design", type="tabs",
                # sampling tab
                tabPanel("Design:",value="Design",
                         style = paste("background: ",subpanelcolours$designC)
                         ),
                tabPanel("Sampling",value="Sampling",
                         style = paste("background: ",subpanelcolours$designC), 
                         wellPanel(
                           style = paste("background: ",subpanelcolours$designC,";"),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(
                                        tags$td(width = "30%", tags$div(style = localStyle, "Sample Size:")),
                                        tags$td(width = "70%", 
                                                numericInput("sN",label=NULL,value=design$sN)
                                        )
                                      ),
                                      tags$tr(
                                        tags$td(width = "30%", tags$div(style = localStyle, "Method:")),
                                        tags$td(width = "70%", 
                                                selectInput("sMethod",label=NULL,c("Random","Stratified","Cluster","Convenience","Snowball"),
                                                            selected=design$sMethod,
                                                            selectize=FALSE)
                                        )
                                      ),
                                      tags$tr(
                                        tags$td(width = "30%", tags$div(style = localStyle, "Usage (IV):")),
                                        tags$td(width = "70%", 
                                                selectInput("sIV1Use",label=NULL,c("Between","Within"),
                                                            selected=design$sIV1Use,
                                                            selectize=FALSE)
                                        )
                                      ),
                                      tags$tr(id="IV2Design",
                                              tags$td(width = "30%", 
                                                      conditionalPanel(condition="input.IV2choice != 'none'",
                                                                       tags$div(style = localStyle, "Usage (IV2):"))
                                                      ),
                                              tags$td(width = "70%", 
                                                      conditionalPanel(condition="input.IV2choice != 'none'",
                                                                       selectInput("sIV2Use",label=NULL,c("Between","Within"),
                                                                  selected=design$sIV2Use,
                                                                  selectize=FALSE)
                                              )
                                      ))
                           ))
                ),
                tabPanel("Anomalies",
                         style = paste("background: ",subpanelcolours$designC), 
                         wellPanel(
                           style = paste("background: ",subpanelcolours$designC,";"),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(
                                        tags$td(width = "40%", tags$div(style = localStyle, "Dependence:")),
                                        tags$td(width = "60%", 
                                                numericInput("sDependence",label=NULL,value=design$sDependence,min=0, max=1, step=0.1)
                                        )
                                      ),
                                      tags$tr(
                                        tags$td(width = "40%", tags$div(style = localStyle, "Outliers:")),
                                        tags$td(width = "60%", 
                                                numericInput("sOutliers",label=NULL,value=design$sOutliers,min=0, max=1, step=0.1)
                                        )
                                      ),
                                      tags$tr(
                                        tags$td(width = "40%", tags$div(style = localStyle, "Limited Ranges:")),
                                        tags$td(width = "60%", 
                                                checkboxInput("sRangeOn",label=NULL,value=design$sRangeOn)
                                        )
                                      )
                           )
                         ),
                conditionalPanel(condition  = "input.sRangeOn",
                           fluidRow(
                           column(width=6,offset=0,
                                  sliderInput("sDVRange",
                                              label="DV:",
                                              min = -fullRange,
                                              max = fullRange,
                                              step = 0.1,
                                              value = design$sDVRange
                                  )
                           ),
                           column(width=6,offset=0,
                                  sliderInput("sIVRange",
                                              label="IV:",
                                              min = -fullRange,
                                              max = fullRange,
                                              step = 0.1,
                                              value = design$sIVRange
                                  )
                           )
                         )
                )
                )
                # options tab
                ,tabPanel("#",id="DesignOptions",
                          style = paste("background: ",subpanelcolours$designC),
                          wellPanel(
                            style = paste("background: ",subpanelcolours$designC,";"),
                            conditionalPanel(condition="input.sMethod == 'Random'",
                                             tags$table(width = "100%",class="myTable",
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = paste(localStyle,"text-align: left"), "Random:")),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "100%", tags$div(style = localPlainStyle, "no options")),
                                                        ))),
                            conditionalPanel(condition="input.sMethod == 'Stratified'",
                                             tags$table(width = "100%",class="myTable",
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = paste(localStyle,"text-align: left"), "Stratified:")),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "number:")),
                                                          tags$td(width = "50%",numericInput("sNStrata",label=NULL,value=design$sNStrata)),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "range:")),
                                                          tags$td(width = "50%",numericInput("sRStrata",label=NULL,value=design$sRStrata)),
                                                        ))),
                            conditionalPanel(condition="input.sMethod == 'Cluster'",
                                             tags$table(width = "100%",class="myTable",
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = paste(localStyle,"text-align: left"), "Cluster:")),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "clusters number:")),
                                                          tags$td(width = "50%",numericInput("sNCluster",label=NULL,value=design$sNCluster)),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "cluster range:")),
                                                          tags$td(width = "50%",numericInput("sRCluster",label=NULL,value=design$sRCluster)),
                                                        ))),
                            conditionalPanel(condition="input.sMethod == 'Convenience'",
                                             tags$table(width = "100%",class="myTable",
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = paste(localStyle,"text-align: left"), "Convenience:")),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "clusters number:")),
                                                          tags$td(width = "50%",numericInput("sNConvenience",label=NULL,value=design$sNConvenience)),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "cluster range:")),
                                                          tags$td(width = "50%",numericInput("sRConvenience",label=NULL,value=design$sRConvenience)),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "contacts number:")),
                                                          tags$td(width = "50%",numericInput("sNCConvenience",label=NULL,value=design$sNCConvenience)),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "contact range:")),
                                                          tags$td(width = "50%",numericInput("sRCConvenience",label=NULL,value=design$sRCConvenience)),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "spread range:")),
                                                          tags$td(width = "50%",numericInput("sRCSConvenience",label=NULL,value=design$sRCSConvenience)),
                                                        ))),
                            conditionalPanel(condition="input.sMethod == 'Snowball'",
                                             tags$table(width = "100%",class="myTable",
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = paste(localStyle,"text-align: left"), "Snowball:")),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "cluster number:")),
                                                          tags$td(width = "50%",numericInput("sNCSnowball",label=NULL,value=design$sNCSnowball)),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "clusters range:")),
                                                          tags$td(width = "50%",numericInput("sRCSnowball",label=NULL,value=design$sRCSnowball)),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "contacts number:")),
                                                          tags$td(width = "50%",numericInput("sNSSnowball",label=NULL,value=design$sNSSnowball)),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "contact range:")),
                                                          tags$td(width = "50%",numericInput("sRSSnowball",label=NULL,value=design$sRSSnowball)),
                                                        ),
                                                        tags$tr(
                                                          tags$td(width = "30%", tags$div(style = localPlainStyle, "spread:")),
                                                          tags$td(width = "50%",numericInput("sRCSSnowball",label=NULL,value=design$sRCSSnowball)),
                                                        ))),
                          )
                )
                # help tab
                ,tabPanel(helpChar,value="?",
                          style = paste("background: ",subpanelcolours$designC,";"),
                          wellPanel(
                            style = paste("background: ",subpanelcolours$designC,";"),
                            tags$table(width = "100%",class="myTable",
                                       tags$tr(
                                         tags$div(style = helpStyle, 
                                                  tags$br(HTML("<b>"),"Sampling:",HTML("</b>")),
                                                  tags$br(HTML('&emsp;'), '1. choose the sampling method'),
                                                  tags$br(HTML('&emsp;'), '2. set the sample size'),
                                                  tags$br(HTML('&emsp;'),HTML('&emsp;'), '(see the Prediction diagram change)'),
                                                  tags$br(HTML('&emsp;'), '3. choose a between/within design'),
                                                  tags$br(HTML('&emsp;'),HTML('&emsp;'), '(Categorical IVs only)'),
                                                  tags$br(HTML("<b>"),"Anomalies: ",HTML("</b>")),
                                                  tags$br(HTML('&emsp;'), '1. add in outliers'),
                                                  tags$br(HTML('&emsp;'), '2. sample with non-independence'),
                                                  tags$br(HTML('&emsp;'), '3. apply heteroscedasticity (unequal variance)'),
                                                  tags$br(HTML('&emsp;'), '4. apply limited range to IV or DV'),
                                                  tags$br(HTML('&emsp;'),HTML('&emsp;'), 'a. set the range of IV sampling'),
                                                  tags$br(HTML('&emsp;'),HTML('&emsp;'), 'b. set the range of DV values retained'),
                                         ),
                                       )
                            )
                          )
                )
    )
  )
