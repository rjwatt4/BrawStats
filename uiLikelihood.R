likelihoodLengthChoices=c("100" = "100",
                       "250" = "250",
                       "500" = "500",
                       "1000" = "1000"
)
LikelihoodTab <-

wellPanel(
  style = paste("background: ",panelcolours$likelihoodC), 
  # h5("Evidence"),
  fluidRow(headerText("Likelihood functions based on sample or population")),
  tabsetPanel(type="tabs",id="Likelihood",
              # single tab
              tabPanel("Possible:",
                       style = paste("background: ",subpanelcolours$likelihoodC), 
                       fluidRow(
                       )
              ),
              tabPanel("Samples",
                       style = paste("background: ",subpanelcolours$likelihoodC), 
                       wellPanel(
                         style = paste("background: ",subpanelcolours$likelihoodC,";"),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "50%", tags$div(style = localStyle, "Population Distrib:")),
                                      tags$td(width = "30%", 
                                              selectInput("Population_distrS", label=NULL,
                                                          c("Uniform" = "Uniform",
                                                            "Exp" = "Exp"),selectize=FALSE)
                                      ),
                                      tags$td(width = "20%", 
                                              numericInput("Population_distr_kS",label=NULL,
                                                           min = 0,
                                                           max = 1,
                                                           step = 0.05,
                                                           value = 0.2)
                                      )
                                    ),
                                    tags$tr(
                                      tags$td(width = "50%", tags$div(style = localStyle, "Source Population:")),
                                      tags$td(width = "30%", numericInput("likelihoodPopRho", label=NULL,min=-1,max=1, step=0.1,value=0)),
                                      tags$td(width = "20%")
                                    ),
                         ),
                         conditionalPanel(condition="1==2",
                                          tags$table(width = "100%",class="myTable",
                                                     tags$tr(
                                      tags$td(width = "50%", tags$div(style = localStyle, "Show theory:")),
                                      tags$td(width = "30%", checkboxInput("likelihoodTheoryS", value=TRUE, label=NULL)),
                                      tags$td(width = "20%")
                                    ),
                         )),
                         conditionalPanel(condition="1==2",
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "20%", tags$div(style = localStyle, "Runs:")),
                                      tags$td(width = "20%", 
                                              selectInput("likelihood_lengthS", label=NULL,
                                                          likelihoodLengthChoices,selectize=FALSE)
                                      ),
                                      tags$td(width = "20%", tags$div(style = localStyle, "Append:")),
                                      tags$td(width = "10%", checkboxInput("likelihoodAppendS", label=NULL)),
                                      tags$td(width = "10%", actionButton("likelihoodRunS", "Run"))
                                    )
                         )
                         )
                       )
              ),
              tabPanel("Populations",
                       style = paste("background: ",subpanelcolours$likelihoodC), 
                       wellPanel(
                         style = paste("background: ",subpanelcolours$likelihoodC,";"),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "50%", tags$div(style = localStyle, "Population Distrib:")),
                                      tags$td(width = "30%", 
                                              selectInput("Population_distrP", label=NULL,
                                                          c("Uniform" = "Uniform",
                                                            "Exp" = "Exp"),selectize=FALSE)
                                      ),
                                      tags$td(width = "20%", 
                                              numericInput("Population_distr_kP",label=NULL,
                                                           min = 0,
                                                           max = 1,
                                                           step = 0.05,
                                                           value = 0.2)
                                      )
                                    ),
                                    tags$tr(
                                      tags$td(width = "50%", tags$div(style = localStyle, "Target Sample:")),
                                      tags$td(width = "30%", numericInput("likelihoodSampRho", label=NULL,min=-1,max=1, step=0.1,value=0)),
                                      tags$td(width = "20%")
                                    ),
                         ),
                         conditionalPanel(condition="1==2",
                                          tags$table(width = "100%",class="myTable",
                                                     tags$tr(
                                                       tags$td(width = "50%", tags$div(style = localStyle, "Show theory:")),
                                                       tags$td(width = "30%", checkboxInput("likelihoodTheoryP", value=TRUE, label=NULL)),
                                                       tags$td(width = "20%")
                                                     )
                                          )),
                         conditionalPanel(condition="1==2",
                                          tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "20%", tags$div(style = localStyle, "Runs:")),
                                      tags$td(width = "20%", 
                                              selectInput("likelihood_lengthP", label=NULL,
                                                          likelihoodLengthChoices,selectize=FALSE)
                                      ),
                                      tags$td(width = "20%", tags$div(style = localStyle, "Append:")),
                                      tags$td(width = "10%", checkboxInput("likelihoodAppendP", label=NULL)),
                                      tags$td(width = "10%", actionButton("likelihoodRunP", "Run")),
                                    )
                         )
                         )
                       )
              ),
              tabPanel("#",
                       style = paste("background: ",subpanelcolours$likelihoodC), 
                       wellPanel(
                         style = paste("background: ",subpanelcolours$likelihoodC,";"),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "5%", tags$div(style = localStyle, "view:")),
                                      tags$td(width = "25%", 
                                              selectInput("LikelihoodView", label=NULL,
                                                          c("3D" = "3D",
                                                            "2D" = "2D"),selectize=FALSE)
                                      )
                                      ),
                                    tags$tr(
                                      tags$td(width = "5%", tags$div(style = localStyle, "az:")),
                                      tags$td(width = "25%", 
                                              numericInput("LikelihoodAzimuth",label=NULL,
                                                           min = -180,
                                                           max = 180,
                                                           step = 5,
                                                           value = 35)
                                      ),
                                      tags$td(width = "5%", tags$div(style = localStyle, "elev:")),
                                      tags$td(width = "25%", 
                                              numericInput("LikelihoodElevation",label=NULL,
                                                           min = 0,
                                                           max = 90,
                                                           step = 5,
                                                           value = 15)
                                      ),
                                      tags$td(width = "5%", tags$div(style = localStyle, "r:")),
                                      tags$td(width = "25%", 
                                              numericInput("LikelihoodRange",label=NULL,
                                                           min = 0,
                                                           max = 10000,
                                                           step = 100,
                                                           value = 1000)
                                      )
                                    )
                         )
                       )
              )
              # help tab
              ,tabPanel("?",
                        style = paste("background: ",subpanelcolours$likelihoodC),
                        wellPanel(
                          style = paste("background: ",subpanelcolours$likelihoodC,";"),
                          tags$table(width = "100%",class="myTable",
                                     tags$tr(
                                       tags$div(style = helpStyle, 
                                                tags$br(HTML('<b>'),"Samples:",HTML('</b>')),
                                                tags$br("Visualize the samples produced by a given population"),
                                                tags$br(" "),
                                                tags$br(HTML('<b>'),"Populations:",HTML('</b>')),
                                                tags$br("Visualize the populations that produce a given sample"),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'), '(slower process)'),
                                                tags$br(" "),
                                                tags$br(HTML('<b>'),"Steps:",HTML('</b>')),
                                                tags$br(HTML('&emsp;'), '1. choose the distribution of populations'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'), 'uniform - the common assumption'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'),HTML('&emsp;'), '(extremely unlikely in practice)'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'), 'exponential - much more likely'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'),HTML('&emsp;'), '(high effect sizes are rare)'),
                                                tags$br(HTML('&emsp;'), '2. choose whether to see theoretical distributions'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'),HTML('&emsp;'), '(these are idealized)'),
                                                tags$br(HTML('&emsp;'), '3. press "Run"'),
                                       ),
                                     )
                          )
                        )
              )
  )
)
