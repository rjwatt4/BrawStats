hypothesisFullChoices=list("Variables"=list("IV type" = "IVType",
                                        "DV type" = "DVType",
                                        "IV/DV type" = "IVDVType",
                                        "IV skew" = "IVskew",
                                        "DV skew" = "DVskew",
                                        "IV kurtosis" = "IVkurtosis",
                                        "DV kurtosis" = "DVkurtosis",
                                        "IV proptn" = "IVprop",
                                        "DV proptn" = "DVprop"),
                       "Effects"=list("Effect Size1" = "EffectSize1",
                                      "Effect Size2" = "EffectSize2",
                                      "Interaction" = "Interaction",
                                      "Covariation" = "Covariation")
)

hypothesisChoices=list("Variables"=list("IV" = "IV",
                                        "IV2" = "IV2",
                                        "DV" = "DV",
                                        "IV/DV Types" = "Types"),
                       "Effects"=list("Effect Size1" = "EffectSize1",
                                      "Effect Size2" = "EffectSize2",
                                      "Interaction" = "Interaction",
                                      "Covariation" = "Covariation")
)

variableChoices=list("& Type"="Type",
                     "& Skew"="skew",
                     "& Kurtosis"="kurtosis",
                     "& proptn"="prop"
                     )

designChoices=list("Sampling"=list("Sample Size" = "SampleSize",
                                   "Sampling Method" = "Method",
                                   "Sample Usage" = "Usage"),
                   "Anomalies"=list("Dependence" = "Dependence",
                                    "Outliers" = "Outliers",
                                    "Heteroscedasticity" = "Heteroscedasticity",
                                    "IV Range" = "IVRange",
                                    "DV Range" = "DVRange")
)

anomChoices=list("Anomalies"=list("Dependence" = "Dependence",
                                  "Outliers" = "Outliers",
                                  "Heteroscedasticity" = "Heteroscedasticity"),
                 "Range"=list("IV Range" = "IVRange",
                              "DV Range" = "DVRange")
)

effectChoices=list("IV1-DV"="MainEffectIV",
                   "IV2-DV"="MainEffectIV2",
                   "IV1xIV2-DV"="InteractionEffect")

showChoices=list("Describe" = list("Effect Size" = "EffectSize"),
              "Infer" = list("p-value" = "p",
                             "Power" = "w",
                             "NHST errors" = "NHSTErrors",
                             "p(sig)" = "p(sig)"
              )
)
extraShowChoices=c("direct"="direct",
                   "unique"="unique",
                   "total"="total",
                   "all"="all")

whichShowChoices=c("Main 1" = "Main 1",
                   "Main 2" = "Main 2",
                   "Interaction" = "Interaction")

exploreLengthChoices=c("10" = "10",
                       "50" = "50",
                       "100" = "100",
                       "500" = "500",
                       "1000" = "1000"
                       )

ExploreTab <-
    wellPanel(id="uiExplore",
            style = paste("background: ",panelcolours$exploreC), 
            fluidRow(headerText("Explore design decisions")),
            tabsetPanel(type="tabs",id="ExploreTab",
                        # sampling tab
                        tabPanel("Explore:",
                                 style = paste("background: ",subpanelcolours$exploreC)
                        ),
                        tabPanel("Hypothesis",id="ExH",
                                 style = paste("background: ",subpanelcolours$exploreC), 
                                 wellPanel(
                                   style = paste("background: ",subpanelcolours$exploreC,";"),
                                   tags$table(width = "100%",class="myTable",
                                              tags$tr(
                                                tags$td(width = "10%", tags$div(style = localStyle, "Vary:")),
                                                tags$td(width = "40%", 
                                                        selectInput("Explore_typeH",label=NULL,
                                                                    hypothesisChoices,selectize=FALSE)
                                                ),
                                                tags$td(width = "25%", 
                                                        conditionalPanel(condition="input.Explore_typeH == 'IV' || input.Explore_typeH == 'DV' || input.Explore_typeH == 'IV2'",
                                                                         selectInput("Explore_VtypeH",label=NULL,
                                                                    variableChoices,selectize=FALSE)
                                                        )
                                                ),
                                                # tags$td(id="Explore_esRangeLabel",width = "25%", tags$div(style = localStyle, "range:")),
                                                # tags$td(width = "25%", 
                                                #         conditionalPanel(condition="input.Explore_typeH != 'IV'",
                                                #                          numericInput("Explore_esRange", label=NULL,value=0.8)
                                                #         )
                                                # ),
                                              ),
                                              tags$tr(
                                                tags$td(width = "10%", tags$div(style = localStyle, "Show:")),
                                                tags$td(width = "40%", 
                                                        selectInput("Explore_showH", label=NULL,
                                                                    showChoices,selectize = FALSE)
                                                ),
                                                tags$td(width = "25%", 
                                                        conditionalPanel(condition="input.IV2choice != 'none'",
                                                                         selectInput("Explore_whichShowH", label=NULL,
                                                                    whichShowChoices, selected="Main 1",selectize = FALSE)
                                                )),
                                                tags$td(width = "25%", 
                                                        conditionalPanel(condition="input.IV2choice != 'none'",
                                                                         selectInput("Explore_extraShowH", label=NULL,
                                                                    extraShowChoices, selected="direct",selectize = FALSE)
                                                ))
                                              )),
                                   tags$table(width = "100%",class="myTable",
                                              tags$tr(
                                                tags$td(width = "10%", tags$div(style = localStyle, "Runs:")),
                                                tags$td(width = "30%", 
                                                        selectInput("Explore_lengthH", label=NULL,
                                                                    exploreLengthChoices,selectize=FALSE)
                                                ),
                                                tags$td(width = "10%", tags$div(style = localStyle, "Append:")),
                                                tags$td(width = "10%", checkboxInput("exploreAppendH", label=NULL)),
                                                tags$td(width = "20%", actionButton("exploreRunH", "Run"))
                                              )
                                   ))
                        ),
                        tabPanel("Design",id="ExD",
                                 style = paste("background: ",subpanelcolours$exploreC), 
                                 wellPanel(
                                   style = paste("background: ",subpanelcolours$exploreC,";"),
                                   tags$table(width = "100%",class="myTable",
                                              tags$tr(
                                                tags$td(width = "10%", tags$div(style = localStyle, "Vary:")),
                                                tags$td(width = "40%", 
                                                        selectInput("Explore_typeD",label=NULL,
                                                                    designChoices,selectize=FALSE)
                                                ),
                                                tags$td(id="Explore_nRangeLabel",width = "25%", tags$div(style = localStyle, "range:")),
                                                tags$td(width = "25%", 
                                                        numericInput("Explore_nRange", label=NULL,value=250)
                                                )
                                              ),
                                              tags$tr(
                                                tags$td(width = "10%", tags$div(style = localStyle, "Show:")),
                                                tags$td(width = "40%", 
                                                        selectInput("Explore_showD", label=NULL,
                                                                    showChoices,width="100%",selectize = FALSE)
                                                ),
                                                tags$td(width = "25%", 
                                                        conditionalPanel(condition="input.IV2choice != 'none'",
                                                                         selectInput("Explore_whichShowD", label=NULL,
                                                                    whichShowChoices, selected="Main 1",selectize = FALSE)
                                                )),
                                                tags$td(width = "25%", 
                                                        conditionalPanel(condition="input.IV2choice != 'none'",
                                                                         selectInput("Explore_extraShowD", label=NULL,
                                                                    extraShowChoices, selected="direct",selectize = FALSE)
                                                ))
                                              )),
                                   tags$table(width = "100%",class="myTable",
                                              tags$tr(
                                                tags$td(width = "10%", tags$div(style = localStyle, "Runs:")),
                                                tags$td(width = "30%", 
                                                        selectInput("Explore_lengthD", label=NULL,
                                                                    exploreLengthChoices,selectize=FALSE)
                                                ),
                                                tags$td(width = "10%", tags$div(style = localStyle, "Append:")),
                                                tags$td(width = "10%", checkboxInput("exploreAppendD", label=NULL)),
                                                tags$td(width = "20%", actionButton("exploreRunD", "Run"))
                                              )
                                   ))
                        ),                        
                        # tabPanel("Anomalies",id="ExA",
                        #          style = paste("background: ",subpanelcolours$exploreC), 
                        #          wellPanel(
                        #            style = paste("background: ",subpanelcolours$exploreC,";"),
                        #            tags$table(width = "100%",class="myTable",
                        #                       tags$tr(
                        #                         tags$td(width = "10%", tags$div(style = localStyle, "Vary:")),
                        #                         tags$td(width = "40%", 
                        #                                 selectInput("Explore_typeA",label=NULL,
                        #                                             anomChoices,selectize=FALSE)
                        #                         ),
                        #                         tags$td(id="Explore_anomRangeLabel",width = "25%", tags$div(style = localStyle, "an-range:")),
                        #                         tags$td(width = "25%", 
                        #                                 numericInput("Explore_anomRange", label=NULL,value=0.9)
                        #                         ),
                        #                       ),
                        #                       tags$tr(
                        #                         tags$td(width = "10%", tags$div(style = localStyle, "Show:")),
                        #                         tags$td(width = "40%", 
                        #                                 selectInput("Explore_showA", label=NULL,
                        #                                             showChoices,width="100%",selectize = FALSE)
                        #                         ),
                        #                         conditionalPanel(condition="input.IV2choice != 'none'",
                        #                                          tags$td(width = "25%", 
                        #                                 selectInput("Explore_whichShowA", label=NULL,
                        #                                             whichShowChoices, selected="Main 1",selectize = FALSE)
                        #                         )),
                        #                         tags$td(width = "25%", 
                        #                                 conditionalPanel(condition="input.IV2choice != 'none'",
                        #                                                  selectInput("Explore_extraShowA", label=NULL,
                        #                                             extraShowChoices, selected="direct",selectize = FALSE)
                        #                         ))
                        #                       )),
                        #            tags$table(width = "100%",class="myTable",
                        #                       tags$tr(
                        #                         tags$td(width = "10%", tags$div(style = localStyle, "Runs:")),
                        #                         tags$td(width = "30%", 
                        #                                 selectInput("Explore_lengthA", label=NULL,
                        #                                             exploreLengthChoices,width="100%",selectize=FALSE)
                        #                         ),
                        #                         tags$td(width = "10%", tags$div(style = localStyle, "Append:")),
                        #                         tags$td(width = "10%", checkboxInput("exploreAppendA", label=NULL)),
                        #                         tags$td(width = "20%", actionButton("exploreRunA", "Run"))
                        #                       )
                        #            ))
                        # ),                        
                        tabPanel("#",
                                 style = paste("background: ",subpanelcolours$exploreC), 
                                 wellPanel(
                                   style = paste("background: ",subpanelcolours$exploreC,";"),
                                   tags$table(width = "100%",class="myTable",
                                              tags$tr(
                                                tags$td(width = "25%", tags$div(style = localStyle, "no points:")),
                                                tags$td(width = "15%", 
                                                        numericInput("Explore_npoints", label=NULL,value=13)
                                                ),
                                                tags$td(width = "35%", tags$div(style = localStyle, "r-range:")),
                                                tags$td(width = "25%", 
                                                        numericInput("Explore_esRange2", label=NULL,value=0.8)
                                                ),
                                              ),
                                              tags$tr(
                                                tags$td(width = "25%", tags$div(style = localStyle, "quantiles:")),
                                                tags$td(width = "15%", 
                                                        numericInput("Explore_quants", label=NULL,value=0.95, step = 0.01,min=0.01,max=0.99)
                                                ),
                                                tags$td(width = "35%", tags$div(style = localStyle, "n-range:")),
                                                tags$td(width = "25%", 
                                                        numericInput("Explore_nRange2", label=NULL,value=250)
                                                ),
                                              ),
                                              tags$tr(
                                                tags$td(width = "25%", tags$div(style = localStyle, "full y-lim:")),
                                                tags$td(width = "15%", checkboxInput("full_ylim", label=NULL,value=FALSE)),
                                                tags$td(width = "35%", tags$div(style = localStyle, "anom-range:")),
                                                tags$td(width = "25%", 
                                                        numericInput("Explore_anomRange2", label=NULL,value=0.9)
                                                )                                              )
                                   ))
                        )
                        # help tab
                        ,tabPanel("?",
                                  style = paste("background: ",subpanelcolours$exploreC),
                                  wellPanel(
                                    style = paste("background: ",subpanelcolours$exploreC,";"),
                                    tags$table(width = "100%",class="myTable",
                                               tags$tr(
                                                 tags$div(style = helpStyle, 
                                                          tags$br(HTML('<b>'),"Before starting:",HTML('</b>')),
                                                          tags$br(HTML('&emsp;'), '1. set up a basic hypothesis with other panels'),
                                                          tags$br(HTML('<b>'),"Set up:",HTML('</b>')),
                                                          tags$br(HTML('&emsp;'), '2. choose the decision to explore'),
                                                          tags$br(HTML('&emsp;'),HTML('&emsp;'), '(these are split into separate groups)'),
                                                          tags$br(HTML('&emsp;'), '3. choose the outcome to examine'),
                                                          tags$br(HTML('<b>'),"Run: ",HTML('</b>')),
                                                          tags$br(HTML('&emsp;'), '4. press the "Run" button'),
                                                          tags$br(HTML('&emsp;'),HTML('&emsp;'), '(can be slow - it is working hard!)'),
                                                 ),
                                               )
                                    )
                                  )
                        )
                        
            )
                                                      
)