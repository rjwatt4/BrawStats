source("uiEffectPart.R")
source("uiDesignPart.R")

LGmodalEvidence <-
  bsModal(id="LGmodalEvidence", title=" ", trigger="trig", size = "large", 
          wellPanel(
            style = paste("background: ",maincolours$panelC,";"),
            fluidRow(
              style=paste0("height: ",LGPanelHeight),
              column(offset=0,width=3, 
                     wellPanel(
                       style = paste("background: ",subpanelcolours$simulateC,";"),
                       tags$table(width = "100%",class="myTable",style=paste("margin:0px;padding:0px;margin-left:-20px;margin-right:-20px;"),
                                  tags$tr(
                                    tags$td(width = "30%", tags$div(style = localStyle, "Evidence:")),
                                    tags$td(width = "30%", 
                                            selectInput("LGEvidenceShow", label=NULL,
                                                        c("Single" = "Single",
                                                          "Multiple" = "Multiple"
                                                        ),selected="Multiple",selectize=FALSE)
                                    ),
                                    tags$td(width = "25%"),
                                    tags$td(width = "15%"),
                                  ),
                       )
                       ),
                     wellPanel(
                       style = paste("background: ",subpanelcolours$simulateC,";"),
                       conditionalPanel("input.LGEvidenceShow=='Single'",
                                        tags$table(width = "100%",class="myTable",
                                                   tags$tr(
                                                     tags$td(width = "15%", tags$div(style = localStyle, "Graph:")),
                                                     tags$td(width = "35%", 
                                                             selectInput("LGEvidenceDisplayType",label=NULL,
                                                                         c("Sample" = "Sample",
                                                                           "Describe" = "Describe",
                                                                           "Infer" = "Infer"),
                                                                         selected="Describe",
                                                                         selectize=FALSE)
                                                     ),
                                                     tags$td(width = "25%"),
                                                     tags$td(width = "25%")
                                                   ),
                                                   tags$tr(
                                                     tags$td(width = "15%", tags$div(style = localStyle, "Show:")),
                                                     tags$td(width = "35%", 
                                                             selectInput("LGEvidenceInfer_type",label=NULL,
                                                                         c("Basic" = "EffectSize",
                                                                           "Power" = "Power",
                                                                           "log(lr)" = "log(lr)"),
                                                                         selectize=FALSE)
                                                     ),
                                                     tags$td(width = "25%"), # actionButton("LGEvidenceEvidenceHypothesisApply", "Analyze")),
                                                     tags$td(width = "25%", actionButton("LGEvidencenewSample", "New Sample"))
                                                   ),
                                        )
                       ),
                       conditionalPanel("input.LGEvidenceShow=='Multiple'",
                                        tags$table(width = "100%",class="myTable",
                                                   tags$tr(
                                                     tags$td(width = "15%", tags$div(style = localStyle, "Show:")),
                                                     tags$td(width = "40%", 
                                                             selectInput("LGEvidenceExpected_type",label=NULL,
                                                                         c("Basic" = "EffectSize",
                                                                           "Power" = "Power",
                                                                           "NHST errors" = "NHSTErrors",
                                                                           "CI limits" = "CILimits",
                                                                           "log(lr)" = "log(lr)",
                                                                           "2D"="2D"
                                                                         ),
                                                                         selectize=FALSE),
                                                     ),
                                                     tags$td(width = "15%",
                                                             conditionalPanel(condition="input.LGEvidenceExpected_type=='2D'",
                                                                              selectInput("LGEvidenceExpected_par1", label=NULL, 
                                                                                          c("r"="r","p"="p","n"="n","w"="w","R"="R","s"="s"), 
                                                                                          selected="p", selectize=FALSE)),
                                                     ),
                                                     tags$td(width = "15%",
                                                             conditionalPanel(condition="input.LGEvidenceExpected_type=='2D'",
                                                                              selectInput("LGEvidenceExpected_par2", label=NULL, 
                                                                                          c("r"="r","p"="p","n"="n","w"="w","R"="R","s"="s"), 
                                                                                          selected="r", selectize=FALSE)),
                                                     ),
                                                     tags$td(width = "35%", 
                                                             conditionalPanel(condition="input.IV2choice != 'none'",
                                                                              selectInput("LGEvidenceEffect_type",label=NULL,
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
                                                             selectInput("LGEvidenceExpected_length",label=NULL,
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
                                                     tags$td(width = "5%", checkboxInput("LGEvidenceExpected_append", label=NULL)),
                                                     tags$td(width = "20%",actionButton("LGEvidenceExpectedRun", "Run")
                                                     )
                                                   )
                                        )
                       )
                     ),
                     wellPanel(
                       style = paste("background: ",subpanelcolours$hypothesisC,";"),
                       effectPanel("LGEvidence",asTable = TRUE),
                       worldPanel("LGEvidence",asTable = TRUE),
                     ),
                     designPanel("LGEvidence"),
                     wellPanel(
                       style = paste("background: ",subpanelcolours$simulateC,";"),
                       tags$table(width = "100%",class="myTable",
                                  tags$tr(
                                    tags$td(width = "15%", tags$div(style = localStyle, " ")),
                                    tags$td(width = "20%",actionButton("LGEvidenceClose","Close")),
                                  )
                       )
                     )
              ),# close of column
              column(width=9,plotOutput("LGshowEvidenceOutput",height=LGGraphHeight))
            )
          )
  )
LGmodalEvidence$attribs$`data-backdrop` <- "static"
# LGmodalPossible[[2]]$`data-backdrop` = "static"
# LGmodalPossible[[2]]$`data-keyboard` = "false"

