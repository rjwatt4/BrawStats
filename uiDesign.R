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
                                                selectInput("sMethod",label=NULL,c("Random","Opportunity","Resample"),
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
                                        tags$td(width = "30%", tags$div(style = localStyle, "Dependence:")),
                                        tags$td(width = "70%", 
                                                numericInput("sDependence",label=NULL,value=design$sDependence,min=0, max=1, step=0.1)
                                        )
                                      ),
                                      tags$tr(
                                        tags$td(width = "30%", tags$div(style = localStyle, "Outliers:")),
                                        tags$td(width = "70%", 
                                                numericInput("sOutliers",label=NULL,value=design$sOutliers,min=0, max=1, step=0.1)
                                        )
                                      ),
                                      tags$tr(
                                        tags$td(width = "30%", tags$div(style = localStyle, "Heteroscedasticity:")),
                                        tags$td(width = "70%", 
                                                numericInput("sHeteroscedasticity",label=NULL,value=design$sHeteroscedasticity,min=-1, max=1, step=0.1)
                                        )
                                      ),
                                      tags$tr(
                                        tags$td(width = "30%", tags$div(style = localStyle, "Limited Ranges:")),
                                        tags$td(width = "70%", 
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
