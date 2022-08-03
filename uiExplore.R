hypothesisFullChoices=list("Variables"=list("IV type" = "IVType",
                                        "DV type" = "DVType",
                                        "IV/DV type" = "IVDVType",
                                        "IV skew" = "IVskew",
                                        "DV skew" = "DVskew",
                                        "IV kurtosis" = "IVkurtosis",
                                        "DV kurtosis" = "DVkurtosis",
                                        "DV levels" = "DVlevels",
                                        "IV proptn" = "IVprop",
                                        "DV proptn" = "DVprop"),
                       "Effects"=list("Effect Size1" = "EffectSize1",
                                      "Effect Size2" = "EffectSize2",
                                      "Interaction" = "Interaction",
                                      "Covariation" = "Covariation")
)

hypothesisChoices3Plain=list("Variables"=list("IV" = "IV",
                                         "IV2" = "IV2",
                                         "DV" = "DV",
                                         "IV/DV Types" = "IVDVType")
)

hypothesisChoices3=list("Variables"=list("IV" = "IV",
                                         "IV2" = "IV2",
                                         "DV" = "DV",
                                         "IV/DV Types" = "IVDVType"),
                        "Effects"=list("Effect Size1" = "EffectSize1",
                                       "Effect Size2" = "EffectSize2",
                                       "Interaction" = "Interaction",
                                       "Covariation" = "Covariation")
)

hypothesisChoices2Plain=list("Variables"=list("IV" = "IV",
                                         "DV" = "DV",
                                         "IV/DV Types" = "IVDVType")
)

if (switches$doWorlds) {
  hypothesisChoices2=list("Variables"=list("IV" = "IV",
                                           "DV" = "DV",
                                           "IV/DV Types" = "IVDVType"),
                          "Effects"=list("Effect Size" = "EffectSize1"),
                          "Worlds"=list("k"="k","pnull"="pnull")
  )
  
} else {
hypothesisChoices2=list("Variables"=list("IV" = "IV",
                                         "DV" = "DV",
                                         "IV/DV Types" = "IVDVType"),
                        "Effects"=list("Effect Size" = "EffectSize1")
)
}

variableChoices=list("& type"="Type",
                     "& skew"="skew",
                     "& kurtosis"="kurtosis",
                     "& cats"="cats",
                     "& levels"="levels",
                     "& proptn"="prop"
)

if (switches$doReplications) {
designChoices=list("Sampling"=list("Sample Size" = "SampleSize",
                                   "Sampling Method" = "Method",
                                   "Sample Usage" = "Usage"),
                   "Anomalies"=list("Dependence" = "Dependence",
                                    "Outliers" = "Outliers",
                                    "Heteroscedasticity" = "Heteroscedasticity",
                                    "IV Range" = "IVRange",
                                    "DV Range" = "DVRange"),
                   "Replication"=list("SigOnly"="SigOnly",
                                      "Power"="Power",
                                      "Repeats" = "Repeats")
)
} else {
  designChoices=list("Sampling"=list("Sample Size" = "SampleSize",
                                     "Sampling Method" = "Method",
                                     "Sample Usage" = "Usage"),
                     "Anomalies"=list("Dependence" = "Dependence",
                                      "Outliers" = "Outliers",
                                      "Heteroscedasticity" = "Heteroscedasticity",
                                      "IV Range" = "IVRange",
                                      "DV Range" = "DVRange")
  )
}
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
                             "p(sig)" = "p(sig)",
                             "Power" = "w",
                             "NHST errors" = "NHSTErrors",
                             "log(lr)" = "log(lr)",
                             "p(str)" = "p(str)"
              )
)
extraShowChoices=c("direct"="direct",
                   "unique"="unique",
                   "total"="total",
                   "all"="all")

whichShowChoices=c("Main 1" = "Main 1",
                   "Main 2" = "Main 2",
                   "Interaction" = "Interaction",
                   "Mains" = "Mains",
                   "All" = "All")

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
                        tabPanel("Explore:",value="Explore",
                                 style = paste("background: ",subpanelcolours$exploreC)
                        ),
                        tabPanel("Hypothesis",id="ExH",
                                 style = paste("background: ",subpanelcolours$exploreC), 
                                 wellPanel(id="ExploreHypothesis",
                                   style = paste("background: ",subpanelcolours$exploreC,";"),
                                   tags$table(width = "100%",class="myTable",
                                              tags$tr(
                                                tags$td(width = "10%", tags$div(style = localStyle, "Vary:")),
                                                tags$td(width = "40%", 
                                                        selectInput("Explore_typeH",label=NULL,
                                                                    hypothesisChoices3,selectize=FALSE)
                                                ),
                                                tags$td(width = "25%", 
                                                        conditionalPanel(condition="input.Explore_typeH == 'IV' || input.Explore_typeH == 'DV' || input.Explore_typeH == 'IV2'",
                                                                         selectInput("Explore_VtypeH",label=NULL,
                                                                    variableChoices,selectize=FALSE)
                                                        )
                                                ),
                                                tags$td(width = "25%")
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
                                                                         selectInput("Explore_typeShowH", label=NULL,
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
                                                tags$td(width = "10%", checkboxInput("ExploreAppendH", label=NULL)),
                                                tags$td(width = "20%", actionButton("exploreRunH", "Run"))
                                              )
                                   ))
                        ),
                        tabPanel("Design",id="ExD",
                                 style = paste("background: ",subpanelcolours$exploreC), 
                                 wellPanel(id="ExploreDesign",
                                   style = paste("background: ",subpanelcolours$exploreC,";"),
                                   tags$table(width = "100%",class="myTable",
                                              tags$tr(
                                                tags$td(width = "10%", tags$div(style = localStyle, "Vary:")),
                                                tags$td(width = "40%", 
                                                        selectInput("Explore_typeD",label=NULL,
                                                                    designChoices,selectize=FALSE)
                                                ),
                                                tags$td(width = "15%", 
                                                        conditionalPanel(condition="input.Explore_typeD == 'SampleSize' || input.Explore_typeD == 'Repeats'",
                                                                         tags$div(style = localStyle, "max:")
                                                        )),
                                                tags$td(width = "25%", 
                                                        conditionalPanel(condition="input.Explore_typeD == 'SampleSize' || input.Explore_typeD == 'Repeats'",
                                                                         numericInput("Explore_nRange", label=NULL,value=250,min=10,step=50)
                                                )),
                                                tags$td(width = "5%", 
                                                        conditionalPanel(condition="input.Explore_typeD == 'SampleSize'",
                                                                         tags$div(style = localStyle, "log")
                                                        )),
                                                tags$td(width = "5%", 
                                                        conditionalPanel(condition="input.Explore_typeD == 'SampleSize'",
                                                                         checkboxInput("Explore_xlog",label="",value=FALSE)
                                                        )),
                                              ),
                                              tags$tr(
                                                tags$td(width = "10%", tags$div(style = localStyle, "Show:")),
                                                tags$td(width = "40%", 
                                                        selectInput("Explore_showD", label=NULL,
                                                                    showChoices,width="100%",selectize = FALSE)
                                                ),
                                                tags$td(width = "15%", 
                                                        conditionalPanel(condition="input.IV2choice != 'none'",
                                                                         selectInput("Explore_whichShowD", label=NULL,
                                                                    whichShowChoices, selected="Main 1",selectize = FALSE)
                                                )),
                                                tags$td(width = "25%", 
                                                        conditionalPanel(condition="input.IV2choice != 'none'",
                                                                         selectInput("Explore_typeShowD", label=NULL,
                                                                    extraShowChoices, selected="direct",selectize = FALSE)
                                                )),
                                                tags$td(width = "5%", tags$div(style = localStyle, "")),
                                                tags$td(width = "5%", tags$div(style = localStyle, "")),
                                              )),
                                   tags$table(width = "100%",class="myTable",
                                              tags$tr(
                                                tags$td(width = "10%", tags$div(style = localStyle, "Runs:")),
                                                tags$td(width = "30%", 
                                                        selectInput("Explore_lengthD", label=NULL,
                                                                    exploreLengthChoices,selectize=FALSE)
                                                ),
                                                tags$td(width = "10%", tags$div(style = localStyle, "Append:")),
                                                tags$td(width = "10%", checkboxInput("ExploreAppendD", label=NULL)),
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
                        #                                                  selectInput("Explore_typeShowA", label=NULL,
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
                        #                         tags$td(width = "10%", checkboxInput("ExploreAppendA", label=NULL)),
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
                                                tags$td(width = "25%", tags$div(style = localPlainStyle, "no points:")),
                                                tags$td(width = "15%", 
                                                        numericInput("Explore_npoints", label=NULL,value=13)
                                                ),
                                                tags$td(width = "30%", tags$div(style = localPlainStyle, "quantiles:")),
                                                tags$td(width = "20%", 
                                                        numericInput("Explore_quants", label=NULL,value=0.95, step = 0.01,min=0.01,max=0.99)
                                                ),
                                                tags$td(width="5%"),
                                                tags$td(width="5%"),
                                              ),
                                              tags$tr(
                                                tags$td(width = "25%", id="Explore_esRangeLabel", tags$div(style = localPlainStyle, "r-range:")),
                                                tags$td(width = "15%", 
                                                        numericInput("Explore_esRange", label=NULL,value=0.8)
                                                ),
                                                tags$td(width = "30%", tags$div(style = localPlainStyle, "anom-range:")),
                                                tags$td(width = "20%", 
                                                        numericInput("Explore_anomRange", label=NULL,value=0.9)
                                                ),
                                                tags$td(width="5%"),
                                                tags$td(width="5%")
                                                ),
                                              tags$tr(
                                                tags$td(width = "25%", tags$div(style = localPlainStyle, "full y-lim:")),
                                                tags$td(width = "15%", checkboxInput("ExploreFull_ylim", label=NULL,value=FALSE)),
                                                tags$td(width = "30%"),
                                                tags$td(width = "20%"),
                                                tags$td(width="5%"),
                                                tags$td(width="5%")
                                              )
                                   )
                                   )
                        )
                        # help tab
                        ,tabPanel(helpChar,value="?",
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
