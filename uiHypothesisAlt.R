HypothesisTab <-
  
  wellPanel(id="HypothesisTabset",
    style = paste("background: ",panelcolours$hypothesisC), 
    # h5("Hypothesis"),
    fluidRow(headerText("Build a hypothesis: variables & effect-size")),
    tabsetPanel(id="Hypothesis",
                # Hypothesis tab
                tabPanel("Hypothesis:",
                         style = paste("background: ",subpanelcolours$hypothesisC)
                ),
                
                # variables tab
                tabPanel("Variables",id="Variables",
                         style = paste("background: ",subpanelcolours$hypothesisC), 
                         wellPanel(
                           style = paste("background: ",subpanelcolours$hypothesisC,";"),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(
                                        tags$td(width = "5%", tags$div(style = localStyle, "IV:")),
                                        tags$td(width = "45%", selectInput("IVchoice", label = NULL,
                                                                           choices=IV$name,
                                                                           selected=IV$name,
                                                                           selectize=FALSE
                                        )),
                                        tags$td(width = "30%", tags$div(style = localStyle, " "))
                                      ),
                                      tags$tr(
                                        tags$td(width = "5%", tags$div(style = localStyle, "IV2:")),
                                        tags$td(width = "45%", selectInput("IV2choice", label = NULL,
                                                                           choices=IV2$name,
                                                                           selected=IV2$name,
                                                                           selectize=FALSE
                                        )),
                                        tags$td(width = "30%", tags$div(style = localStyle, " "))
                                      ),
                                      tags$tr(
                                        tags$td(width = "5%", tags$div(style = localStyle, "DV:")),
                                        tags$td(width = "45%", selectInput("DVchoice", label = NULL,
                                                                           choices=DV$name,
                                                                           selected=DV$name,
                                                                           selectize=FALSE
                                        )),
                                        tags$td(width = "5%", tags$div(style = localStyle, " ")),
                                        tags$td(width = "25%", actionButton("hypothesisApply","Apply"))
                                        )
                           ),
                           width="100%"
                         )
                ),

                tabPanel("IV",
                         style = paste("background: ",subpanelcolours$hypothesisC,";"),
                         wellPanel(
                           style = paste("background: ",subpanelcolours$hypothesisC,";"),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(
                                        tags$td(width = "30%", tags$div(style = localStyle, "Name:")),
                                        tags$td(width = "70%", textInput("IVname", value = IV$name, label = NULL))
                                      ),
                                      tags$tr(
                                        tags$td(width = "30%", div(style = localStyle, "Type:")),
                                        tags$td(width = "70%",
                                                selectInput("IVtype", label= NULL,
                                                            c("Interval" = "Interval",
                                                              "Categorical" = "Categorical"
                                                            ),selected=IV$type,
                                                            selectize=FALSE
                                                )
                                        )
                                      )
                           ),
                           conditionalPanel(condition="input.IVtype == 'Interval",
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(id="IVIntVal1",
                                              tags$td(width = "30%",  tags$div(style = localStyle, "Mean:")),
                                              tags$td(width = "20%", numericInput("IVmu", value = IV$mu, label = NULL)),
                                              tags$td(width = "10%",  tags$div(style = localStyle, "Sd:")),
                                              tags$td(width = "20%", numericInput("IVsd", value = IV$sd, label = NULL)),
                                              tags$td(width = "20%",  tags$div(style = localStyle, " "))
                                      ),
                                      tags$tr(id="IVIntVal2",
                                              tags$td(width = "30%",  tags$div(style = localStyle, " ")),
                                              tags$td(width = "20%",  tags$div(style = localStyle, " ")),
                                              tags$td(width = "10%",  tags$div(style = localStyle, " ")),
                                              tags$td(width = "20%",  tags$div(style = localStyle, " ")),
                                              tags$td(width = "20%",  tags$div(style = localStyle, " "))
                                      ),
                           )),
                           conditionalPanel(condition="input.IVtype == 'Categorical",
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(id="IVCatVal1",
                                              tags$td(width = "30%",  tags$div(style = localStyle, "No cases:")),
                                              tags$td(width = "20%", numericInput("IVncats", value = IV$ncats, label = NULL,step=1,min=2)),
                                              tags$td(width = "30%",  tags$div(style = localStyle, "Proportions:")),
                                              tags$td(width = "20%", textInput("IVprop", value = IV$proportions, label = NULL)),
                                              # tags$td(width = "10%",  tags$div(style = localStyle, " "))
                                      ),
                           ),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(id="IVCatVal2",
                                              tags$td(width = "30%",  tags$div(style = localStyle, "Cases:")),
                                              tags$td(width = "60%", textInput("IVcases", value = IV$cases, label = NULL)),
                                              tags$td(width = "10%",  tags$div(style = localStyle, " "))
                                      )
                           )),
                           width="100%")
                ),

                tabPanel("IV2",
                         style = paste("background: ",subpanelcolours$hypothesisC,";"),
                         wellPanel(
                           style = paste("background: ",subpanelcolours$hypothesisC,";"),
                           conditionalPanel(condition="input.IV2choice != 'none",
                                            tags$table(width = "100%",class="myTable",
                                      tags$tr(id="IV2Name1",
                                        tags$td(width = "30%", tags$div(style = localStyle, "Name:")),
                                        tags$td(width = "70%", textInput("IV2name", value = IV2$name, label = NULL))
                                      ),
                                      tags$tr(id="IV2Type1",
                                        tags$td(width = "30%", div(style = localStyle, "Type:")),
                                        tags$td(width = "70%",
                                                selectInput("IV2type", label= NULL,
                                                            c("Interval" = "Interval",
                                                              "Categorical" = "Categorical"
                                                            ),selected=IV2$type,selectize=FALSE
                                                )
                                        )
                                      )
                           )),
                           conditionalPanel(condition="input.IV2type == 'Interval",
                                            tags$table(width = "100%",class="myTable",
                                      tags$tr(id="IV2IntVal1",
                                              tags$td(width = "30%",  tags$div(style = localStyle, "Mean:")),
                                              tags$td(width = "20%", numericInput("IV2mu", value = IV2$mu, label = NULL)),
                                              tags$td(width = "10%",  tags$div(style = localStyle, "Sd:")),
                                              tags$td(width = "20%", numericInput("IV2sd", value = IV2$sd, label = NULL)),
                                              tags$td(width = "20%",  tags$div(style = localStyle, " "))
                                      ),
                                      tags$tr(id="IV2IntVal2",
                                              tags$td(width = "30%",  tags$div(style = localStyle, " ")),
                                              tags$td(width = "20%",  tags$div(style = localStyle, " ")),
                                              tags$td(width = "10%",  tags$div(style = localStyle, " ")),
                                              tags$td(width = "20%",  tags$div(style = localStyle, " ")),
                                              tags$td(width = "20%",  tags$div(style = localStyle, " "))
                                      ),
                           )),
                           conditionalPanel(condition="input.IV2type == 'Categorical",
                                            tags$table(width = "100%",class="myTable",
                                      tags$tr(id="IV2CatVal1",
                                              tags$td(width = "30%",  tags$div(style = localStyle, "No cases:")),
                                              tags$td(width = "20%", numericInput("IV2ncats", value = IV2$ncats, label = NULL,step=1,min=2)),
                                              tags$td(width = "30%",  tags$div(style = localStyle, "Proportions:")),
                                              tags$td(width = "20%", textInput("IV2prop", value = IV2$proportions, label = NULL)),
                                              # tags$td(width = "10%",  tags$div(style = localStyle, " "))
                                      ),
                           ),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(id="IV2CatVal2",
                                              tags$td(width = "30%",  tags$div(style = localStyle, "Cases:")),
                                              tags$td(width = "60%", textInput("IV2cases", value = IV2$cases, label = NULL)),
                                              tags$td(width = "10%",  tags$div(style = localStyle, " "))
                                      )
                         )),
                           width="100%")
                ),

                tabPanel("DV",
                         style = paste("background: ",subpanelcolours$hypothesisC,";"),
                         wellPanel(
                           style = paste("background: ",subpanelcolours$hypothesisC,";"),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(
                                        tags$td(width = "30%", tags$div(style = localStyle, "Name:")),
                                        tags$td(width = "70%", textInput("DVname", value = DV$name, label = NULL))
                                      ),
                                      tags$tr(
                                        tags$td(width = "30%", div(style = localStyle, "Type:")),
                                        tags$td(width = "70%",
                                                selectInput("DVtype", label= NULL,
                                                            c("Interval" = "Interval",
                                                              "Categorical" = "Categorical"
                                                            ),selected=DV$type,
                                                            selectize=FALSE
                                                )
                                        )
                                      )
                           ),
                           conditionalPanel(condition="input.DVtype == 'Interval",
                                            tags$table(width = "100%",class="myTable",
                                      tags$tr(id="DVIntVal1",
                                              tags$td(width = "30%",  tags$div(style = localStyle, "Mean:")),
                                              tags$td(width = "20%", numericInput("DVmu", value = DV$mu, label = NULL)),
                                              tags$td(width = "10%",  tags$div(style = localStyle, "Sd:")),
                                              tags$td(width = "20%", numericInput("DVsd", value = DV$sd, label = NULL)),
                                              tags$td(width = "20%",  tags$div(style = localStyle, " "))
                                      ),
                                      tags$tr(id="DVIntVal2",
                                              tags$td(width = "30%",  tags$div(style = localStyle, " ")),
                                              tags$td(width = "20%",  tags$div(style = localStyle, " ")),
                                              tags$td(width = "10%",  tags$div(style = localStyle, " ")),
                                              tags$td(width = "20%",  tags$div(style = localStyle, " ")),
                                              tags$td(width = "20%",  tags$div(style = localStyle, " "))
                                      ),
                           )),
                           conditionalPanel(condition="input.IVtype == 'Categorical",
                                            tags$table(width = "100%",class="myTable",
                                      tags$tr(id="DVCatVal1",
                                              tags$td(width = "30%",  tags$div(style = localStyle, "No cases:")),
                                              tags$td(width = "20%", numericInput("DVncats", value = DV$ncats, label = NULL,step=1,min=2)),
                                              tags$td(width = "30%",  tags$div(style = localStyle, "Proportions:")),
                                              tags$td(width = "20%", textInput("DVprop", value = DV$proportions, label = NULL)),
                                              # tags$td(width = "5%",  tags$div(style = localStyle, " "))
                                      ),
                           ),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(id="DVCatVal2",
                                              tags$td(width = "30%",  tags$div(style = localStyle, "Cases:")),
                                              tags$td(width = "60%", textInput("DVcases", value = DV$cases, label = NULL)),
                                              tags$td(width = "10%",  tags$div(style = localStyle, " "))
                                      )
                         )),
                           width="100%")
                ),
                
                # prediction tab
                tabPanel("Effects",id="Prediction",
                         style = paste("background: ",subpanelcolours$hypothesisC), 
                         wellPanel(
                           style = paste("background: ",subpanelcolours$hypothesisC,";"),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(
                                        tags$td(width = "35%", tags$div(style = localStyle, "IV",HTML("&rarr;"),"DV :")),
                                        tags$td(width = "20%", numericInput("rIV", label = NULL,
                                                                            min = -1,
                                                                            max = 1,
                                                                            step = 0.01,
                                                                            value = effect$rIV
                                        )),
                                        tags$td(width = "45%", tags$div(style = localStyle, " "))
                                        
                                      ),
                                      tags$tr(id="IV2E2",
                                        tags$td(width = "35%", tags$div(style = localStyle, "IV2",HTML("&rarr;"),"DV :")),
                                        tags$td(width = "20%", numericInput("rIV2", label = NULL,
                                                                            min = -1,
                                                                            max = 1,
                                                                            step = 0.01,
                                                                            value = effect$rIV2
                                        )),
                                        tags$td(width = "45%", tags$div(style = localStyle, " "))
                                        
                                      ),
                                      tags$tr(id="IV2E3",
                                        tags$td(width = "35%", tags$div(style = localStyle, "IV",HTML("&rarr;"),"IV2 :")),
                                        tags$td(width = "20%", numericInput("rIVIV2", label = NULL,
                                                                            min = -1,
                                                                            max = 1,
                                                                            step = 0.01,
                                                                            value = effect$rIVIV2
                                        )),
                                        tags$td(width = "45%", tags$div(style = localStyle, " "))
                                        
                                      ),
                                      tags$tr(id="IV2E4",
                                        tags$td(width = "35%", tags$div(style = localStyle, "IV*IV2",HTML("&rarr;"),"DV :")),
                                        tags$td(width = "20%", numericInput("rIVIV2DV", label = NULL,
                                                                            min = -1,
                                                                            max = 1,
                                                                            step = 0.01,
                                                                            value = effect$rIVIV2DV
                                        )),
                                        tags$td(width = "45%", tags$div(style = localStyle, " "))
                                      )
                           ),
                           width="100%"
                         )
                )
                # help tab
                ,tabPanel("?",
                          style = paste("background: ",subpanelcolours$hypothesisC),
                          wellPanel(
                            style = paste("background: ",subpanelcolours$hypothesisC,";"),
                            tags$table(width = "100%",class="myTable",
                                       tags$tr(
                                         tags$div(style = helpStyle, 
                                                  tags$br(HTML("<b>"),"Variables:",HTML("</b>")),
                                                  tags$br(HTML('&emsp;'), '1. choose one or two IVs and a DV by name'),
                                                  tags$br(HTML('&emsp;'), '2. edit the variable details in the separate tabs'),
                                                  tags$br(HTML('&emsp;'),HTML('&emsp;'), '(watch the Hypothesis diagram)'),
                                                  tags$br(HTML("<b>"),"Effects: ",HTML("</b>")),
                                                  tags$br(HTML('&emsp;'), '3. select effect size or sizes (for 2 IVs)'),
                                                  tags$br(HTML('&emsp;'),HTML('&emsp;'), 'these are normalized and range from -1 to +1'),
                                                  tags$br(HTML('&emsp;'),HTML('&emsp;'), '(watch the Population or Prediction diagram)')
                                         ),
                                       )
                            )
                          )
                )
    )
  )
