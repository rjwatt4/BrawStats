EvidenceTab <-
  
wellPanel(
  style = paste("background: ",panelcolours$simulateC), 
  fluidRow(headerText("Make a simulated sample; run multiple samples")),
  # h5("Evidence"),
  tabsetPanel(id="Evidence", type="tabs",
              tabPanel("Evidence:",value="Evidence",
              ),
              # single tab
              tabPanel("Single",value="Single",
                       style = paste("background: ",subpanelcolours$simulateC), 
                       # fluidRow(h1("  ")),
                       tags$table(width = "100%",class="myTable",
                                  tags$tr(
                                    tags$td(width = "15%", tags$div(style = localStyle, "Show:")),
                                    tags$td(width = "50%", 
                                            selectInput("Infer_type",label=NULL,
                                                        c("Basic" = "EffectSize",
                                                          "Power" = "Power"),
                                                        selectize=FALSE)
                                    ),
                                    tags$td(width = "10%", tags$div(style = localStyle, "")),
                                    tags$td(width = "25%", actionButton("newSample", "New Sample"))
                                  )
                       )
              ),
              # multiple tab
              tabPanel("Multiple",id="Multiple",
                       style = paste("background: ",subpanelcolours$simulateC), 
                       wellPanel(
                         style = paste("background: ",subpanelcolours$simulateC,";"),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "15%", tags$div(style = localStyle, "Show:")),
                                      tags$td(width = "50%", 
                                              selectInput("Expected_type",label=NULL,
                                                          c("Basic" = "EffectSize",
                                                            "Power" = "Power",
                                                            "NHST errors" = "NHSTErrors",
                                                            "CI limits" = "CILimits"),
                                                      selectize=FALSE)
                                      ),
                                      tags$td(width = "35%", 
                                              conditionalPanel(condition="input.IV2choice != 'none'",
                                              selectInput("Effect_type",label=NULL,
                                                          c("direct" = "direct",
                                                            "unique" = "unique",
                                                            "total" = "total",
                                                            "all" = "all",
                                                            "coefficients" = "coefficients"),
                                                          selectize=FALSE)
                                      )
                                    )),
                                    tags$tr(
                                      tags$td(width = "15%", tags$div(style = localStyle, "Runs:")),
                                      tags$td(width = "50%", 
                                              selectInput("Expected_length",label=NULL,
                                                          c("10" = "10",
                                                            "50" = "50",
                                                            "100" = "100",
                                                            "250" = "250",
                                                            "500" = "500",
                                                            "1000" = "1000"),
                                              selected = "10",
                                                  selectize=FALSE)
                                      ),
                                      tags$td(width = "10%", tags$div(style = localStyle, "Append:")),
                                      tags$td(width = "5%", checkboxInput("expectedAppend", label=NULL)),
                                      tags$td(width = "20%",actionButton("expectedRun", "Run")
                                      )
                                    )
                         )
                       )
              )
              # options tab
              ,tabPanel("#",id="EvidenceOptions",
                        style = paste("background: ",subpanelcolours$simulateC),
                        wellPanel(
                          style = paste("background: ",subpanelcolours$simulateC,";"),
                          tags$table(width = "100%",class="myTable",
                                     tags$tr(
                                       tags$td(width = "30%", tags$div(style = paste(localStyle,"text-align: left"), "Analysis")),
                                     ),
                                     tags$tr(
                                       tags$td(width = "30%", tags$div(style = localPlainStyle, "Welch")),
                                       tags$td(width = "25%", 
                                               checkboxInput("Welch",label=NULL,value=effect$Welch),
                                       ),
                                       tags$td(width = "45%", tags$div(style = localPlainStyle, " ")),
                                     )
                                     ),
                          conditionalPanel(condition="input.IV2choice != 'none'",
                                           tags$table(width = "100%",class="myTable",
                                                      tags$tr(
                                                        tags$td(width = "30%", tags$div(style = localPlainStyle, "Interaction:")),
                                                        tags$td(width = "25%", tags$div(style = localPlainStyle, "analyse")),
                                                        tags$td(width = "10%",checkboxInput("rInteractionOn",label=NULL,value=evidence$rInteractionOn)),
                                                        tags$td(width = "25%", tags$div(style = localPlainStyle, "show only")),
                                                        tags$td(width = "10%", checkboxInput("evidenceInteractionOnly", value=TRUE, label=NULL)),
                                                      ))),
                          conditionalPanel(condition="input.IV2choice != 'none'",
                                           tags$table(width = "100%",class="myTable",
                                                      tags$tr(
                                                        tags$td(width = "30%", tags$div(style = localPlainStyle, "SSQ Type:")),
                                                        tags$td(width = "25%", selectInput("ssqType", label=NULL, c("Type1"="Type1","Type2"="Type2","Type3"="Type3","Type3wrong"="Type3w"), selected="Type3", selectize=FALSE)),
                                                        tags$td(width = "45%", tags$div(style = localStyle, " ")),
                                                      ))),
                          tags$table(width = "100%",class="myTable",
                                     tags$tr(
                                       tags$td(width = "30%", tags$div(style = paste(localStyle,"text-align: left"), "Display")),
                                     ),
                                     tags$tr(
                                       tags$td(width = "30%", tags$div(style = localPlainStyle, "case order:")),
                                       tags$td(width = "40%", selectInput("evidenceCaseOrder", choices = c("Alphabetic"="Alphabetic","As Found"="AsFound","Frequency"="Frequency"),selected="Alphabetic", label=NULL, selectize=FALSE)),
                                       tags$td(width = "30%", tags$div(style = localStyle, " ")),
                                     )),
                          tags$table(width = "100%",class="myTable",
                                     # tags$tr(
                                     #   tags$td(width = "30%", tags$div(style = paste(localStyle,"text-align: left"), "Describe")),
                                     # ),
                                     tags$tr(
                                       tags$td(width = "30%", tags$div(style = localPlainStyle, "scatter plots:")),
                                       tags$td(width = "40%", selectInput("allScatter", label=NULL, c("none"="none","all"="all","corr only"="corr"), selected="all", selectize=FALSE)),
                                       tags$td(width = "30%", tags$div(style = localStyle, "")),
                                     )),
                          tags$table(width = "100%",class="myTable",
                                     # tags$tr(
                                     #   tags$td(width = "30%", tags$div(style = paste(localStyle,"text-align: left"), "Infer")),
                                     # ),
                                     tags$tr(
                                       tags$td(width = "30%", tags$div(style = localPlainStyle, "animations:")),
                                       tags$td(width = "40%", checkboxInput("showAnimation", label=NULL, value=TRUE)),
                                       tags$td(width = "30%", tags$div(style = localStyle, " ")),
                                     ),
                                     tags$tr(
                                       tags$td(width = "30%", tags$div(style = localPlainStyle, "p-scale:")),
                                       tags$td(width = "40%", selectInput("pScale", label=NULL, c("linear"="linear","log10"="log10"), selected="log10", selectize=FALSE)),
                                       tags$td(width = "30%", tags$div(style = localStyle, " ")),
                                     ),
                                     tags$tr(
                                       tags$td(width = "30%", tags$div(style = localPlainStyle, "w-scale:")),
                                       tags$td(width = "25%", selectInput("wScale", label=NULL, c("linear"="linear","log10"="log10"), selected="linear", selectize=FALSE)),
                                       tags$td(width = "45%", tags$div(style = localStyle, " ")),
                                     ),
                                     tags$tr(
                                       tags$td(width = "30%", tags$div(style = localPlainStyle, "show sig/ns:")),
                                       tags$td(width = "40%", checkboxInput("sig_ns", label=NULL, value=TRUE)),
                                       tags$td(width = "30%", tags$div(style = localStyle, " ")),
                                     )),
                          tags$table(width = "100%",class="myTable",
                                     # tags$tr(
                                     #   tags$td(width = "30%", tags$div(style = paste(localStyle,"text-align: left"), "Infer")),
                                     # ),
                                     tags$tr(
                                       tags$td(width = "30%", tags$div(style = localPlainStyle, "animations:")),
                                       tags$td(width = "40%", checkboxInput("showAnimation", label=NULL, value=TRUE)),
                                       tags$td(width = "30%", tags$div(style = localStyle, " ")),
                                     )),
                          )
              )
              # help tab
              ,tabPanel(helpChar,value="?",
                        style = paste("background: ",subpanelcolours$simulateC),
                        wellPanel(
                          style = paste("background: ",subpanelcolours$simulateC,";"),
                          tags$table(width = "100%",class="myTable",
                                     tags$tr(
                                       tags$div(style = helpStyle, 
                                                tags$br(HTML('<b>'),"Single simulation:",HTML('</b>')),
                                                tags$br(HTML('&emsp;'), '1. press "New Sample", nothing else required'),
                                                tags$br(HTML('&emsp;'), '2. results are found in:'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'), 'Sample: raw data'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'), 'Description: effects'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'), 'Inference: null hypothesis tests'),
                                                tags$br(HTML('&emsp;'), '3. choose to see:'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'), 'Basic: effect-size & p-value'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'), 'Power: post-hoc power & n80'),
                                                tags$br(HTML('<b>'),"Multiple simulations: ",HTML('</b>')),
                                                tags$br(HTML('&emsp;'), '1. choose desired output then press "Run"'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'), '(use the append option to add further simulations)'),
                                                tags$br(HTML('&emsp;'), '2. results are found in:'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'), 'Expected:')
                                       ),
                                     )
                          )
                        )
              )
  )
)
