SlikelihoodLengthChoices=c("100" = "100",
                          "250" = "250",
                          "500" = "500",
                          "1000" = "1000"
)
PlikelihoodLengthChoices=c("1000" = "1000",
                          "2500" = "2500",
                          "5000" = "5000",
                          "10000" = "10000"
)

if (switches$doLikelihood) {
LikelihoodTab <-

  wellPanel(ID="MainLikelihood",
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
                         tags$table(width = "100%",class="myTable",style=paste("margin:0px;padding:0px;margin-left:-20px;margin-right:-20px;"),
                                    tags$tr(
                                      tags$td(width = "45%", tags$div(style = localStyle, "Source Population:")),
                                      tags$td(width = "30%", numericInput("likelihoodPopRho", label=NULL,min=-1,max=1, step=0.1,value=0)),
                                      tags$td(width = "15%"),
                                      tags$td(width = "10%"),
                                    ),
                                    tags$tr(
                                      tags$td(width = "45%", tags$div(style = localStyle, "Target Sample:")),
                                      tags$td(width = "30%", numericInput("likelihoodSampRhoS", label=NULL,min=-1,max=1, step=0.1,value=0)),
                                      tags$td(width = "15%"),
                                      tags$td(width = "10%"),
                                    ),
                         ),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "25%", tags$div(style = localStyle, "Runs:")),
                                      tags$td(width = "20%", 
                                              selectInput("likelihood_lengthS", label=NULL,
                                                          SlikelihoodLengthChoices,selectize=FALSE)
                                      ),
                                      tags$td(width = "20%", tags$div(style = localStyle, "Append:")),
                                      tags$td(width = "10%", checkboxInput("likelihoodAppendS", label=NULL)),
                                      tags$td(width = "10%", actionButton("likelihoodRunS", "Run"))
                                    )
                         )
                       )
              ),
              tabPanel("Populations",
                       style = paste("background: ",subpanelcolours$likelihoodC), 
                       wellPanel(
                         style = paste("background: ",subpanelcolours$likelihoodC,";"),
                         tags$table(width = "100%",class="myTable",style=paste("margin:0px;padding:0px;margin-left:-20px;margin-right:-20px;"),
                                    tags$tr(
                                      tags$td(width = "45%", tags$div(style = localStyle, "Population PDF:")),
                                      tags$td(width = "30%", 
                                              selectInput("likelihood_distrP", label=NULL,
                                                          c("Uniform" = "Uniform",
                                                            "Gauss"="Gauss",
                                                            "Exp" = "Exp"),selectize=FALSE)
                                      ),
                                      tags$td(width = "15%", 
                                              selectInput("likelihood_RZ", label=NULL,
                                                          c("r" = "r",
                                                            "z" = "z"),selectize=FALSE)
                                      ),
                                      tags$td(width = "10%", 
                                              conditionalPanel(condition="input.likelihood_distrP!=='Uniform'",
                                                               numericInput("likelihood_distr_kP",label=NULL,
                                                                            min = 0,
                                                                            max = 1,
                                                                            step = 0.05,
                                                                            value = 0.2)
                                              )
                                      )
                                    ),
                                    ),
                                    conditionalPanel(condition="input.includeNulls",
                                                     tags$table(width = "100%",class="myTable",style=paste("margin:0px;padding:0px;margin-left:-20px;margin-right:-20px;"),
                                                                tags$tr(
                                                       tags$td(width = "45%", tags$div(style = localStyle, "p(null):")),
                                                       tags$td(width = "30%", numericInput("likelihoodNullp", label=NULL,min=0,max=1, step=0.025,value=0)),
                                                       tags$td(width = "15%"),
                                                       tags$td(width = "10%")
                                                     ),
                                                     )
                                    ),
                         tags$table(width = "100%",class="myTable",style=paste("margin:0px;padding:0px;margin-left:-20px;margin-right:-20px;"),
                                    tags$tr(
                                      tags$td(width = "45%", tags$div(style = localStyle, "Target Sample:")),
                                      tags$td(width = "30%", numericInput("likelihoodSampRho", label=NULL,min=-1,max=1, step=0.1,value=0)),
                                      tags$td(width = "15%"),
                                      tags$td(width = "10%"),
                                    )
                                    ),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "25%", tags$div(style = localStyle, "Runs:")),
                                      tags$td(width = "20%", 
                                              selectInput("likelihood_lengthP", label=NULL,
                                                          PlikelihoodLengthChoices,selectize=FALSE)
                                      ),
                                      tags$td(width = "20%", tags$div(style = localStyle, "Append:")),
                                      tags$td(width = "10%", checkboxInput("likelihoodAppendP", label=NULL)),
                                      tags$td(width = "10%", actionButton("likelihoodRunP", "Run")),
                                    )
                         ),
                         width="100%"
                       )
              ),
              tabPanel("#",
                       style = paste("background: ",subpanelcolours$likelihoodC), 
                       wellPanel(
                         style = paste("background: ",subpanelcolours$likelihoodC,";"),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "50%", tags$div(style = localStyle, "include nulls:")),
                                      tags$td(width = "30%", checkboxInput("includeNulls", value=FALSE, label=NULL)),
                                      tags$td(width = "20%")
                                    ),
                                    tags$tr(
                                      tags$td(width = "50%", tags$div(style = localStyle, "show theory:")),
                                      tags$td(width = "30%", checkboxInput("likelihoodTheory", value=FALSE, label=NULL)),
                                      tags$td(width = "20%")
                                    )
                         ),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "15%", tags$div(style = localStyle, "view:")),
                                      tags$td(width = "20%", 
                                              selectInput("LikelihoodView", label=NULL,
                                                          c("3D" = "3D",
                                                            "2D" = "2D"),selectize=FALSE)
                                      ),
                                      tags$td(width = "5%", tags$div(style = localStyle, "az:")),
                                      tags$td(width = "15%", 
                                              numericInput("LikelihoodAzimuth",label=NULL,
                                                           min = -180,
                                                           max = 180,
                                                           step = 5,
                                                           value = 35)
                                      ),
                                      tags$td(width = "5%", tags$div(style = localStyle, "elev:")),
                                      tags$td(width = "15%", 
                                              numericInput("LikelihoodElevation",label=NULL,
                                                           min = 0,
                                                           max = 90,
                                                           step = 5,
                                                           value = 15)
                                      ),
                                      tags$td(width = "5%", tags$div(style = localStyle, "r:")),
                                      tags$td(width = "20%", 
                                              numericInput("LikelihoodRange",label=NULL,
                                                           min = 0,
                                                           max = 10000,
                                                           step = 100,
                                                           value = 1000)
                                      ),
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
} else
{
  LikelihoodTab <- c()
}