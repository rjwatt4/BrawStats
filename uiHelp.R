HelpTab <-
  
  wellPanel(id="HelpTabset",
    style = paste("background: ",maincolours$graphC), 
    h5("Follow these 4 steps:",style=paste("font-weight:bold;font-style:italic;font-size=10px;margin:0px;padding:0px;margin-bottom:8px;","color:", maincolours$panelC,";"),sep=""),
#    fluidRow(helpHeaderText("Follow these 4 steps:")),
    tabsetPanel(id="Help",
                # Help tab
                tabPanel("Help:",
                         style = paste("background: ",maincolours$graphC)
                ),
                
                # Step 1 tab
                tabPanel("Step 1",id="Step1",
                         style = paste("background: ",maincolours$graphC), 
                         wellPanel(
                           style = paste("background: ",subpanelcolours$hypothesisC,";"),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(
                                        tags$div(style = helpStyle, 
                                                 tags$br(HTML("<b>"),"Make a Hypothesis:",HTML("</b>")),
                                                 tags$br(HTML('&emsp;'), '1. choose variables'),
                                                 tags$br(HTML('&emsp;'), '2. set expected effect-sizes'),
                                        )
                                      )
                           )
                         )
                ),
                
                # Step 2 tab
                tabPanel("Step 2",id="Step2",
                         style = paste("background: ",maincolours$graphC), 
                         wellPanel(
                           style = paste("background: ",subpanelcolours$designC,";"),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(
                                        tags$div(style = helpStyle, 
                                                 tags$br(HTML("<b>"),"Set a Design:",HTML("</b>")),
                                                 tags$br(HTML('&emsp;'), '1. choose sample size & method'),
                                                 tags$br(HTML('&emsp;'), '2. you can build in sampling anomalies'),
                                        )
                                      )
                           )
                         )
                ),
                
                # Step 3 tab
                tabPanel("Step 3",id="Step3",
                         style = paste("background: ",maincolours$graphC), 
                         wellPanel(
                           style = paste("background: ",subpanelcolours$simulateC,";"),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(
                                        tags$div(style = helpStyle, 
                                                 tags$br(HTML("<b>"),"Evidence:",HTML("</b>")),
                                                 tags$br(HTML('&emsp;'), '1. Now simulate a sample and watch the analysis'),
                                                 tags$br(HTML('&emsp;'), '2. Then run multiple samples to see variability'),
                                        )
                                      )
                           )
                         )
                ),
                
                # Step 4 tab
                tabPanel("Step 4",id="Step4",
                         style = paste("background: ",maincolours$graphC), 
                         wellPanel(
                           style = paste("background: ",subpanelcolours$exploreC,";"),
                           tags$table(width = "100%",class="myTable",
                                      tags$tr(
                                        tags$div(style = helpStyle, 
                                                 tags$br(HTML("<b>"),"Explore:",HTML("</b>")),
                                                 tags$br(HTML('&emsp;'), '1. Look at the consequences of your decisions'),
                                                 tags$br(HTML('&emsp;'), '2. Vary any of them and look at outputs'),
                                        )
                                      )
                           )
                         )
                )
    )
  )
