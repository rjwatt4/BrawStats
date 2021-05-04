FilesTab <-

wellPanel(
  style = paste("background: ",panelcolours$filesC), 
  # h5("Evidence"),
  fluidRow(headerText("Import and export files")),
  tabsetPanel(type="tabs",
              # single tab
              tabPanel("Files:",
                       style = paste("background: ",subpanelcolours$filesC), 
                       fluidRow(
                       )
              ),
              tabPanel("Workspace",
                       style = paste("background: ",subpanelcolours$filesC), 
                       wellPanel(
                         style = paste("background: ",subpanelcolours$filesC,";"),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "5%", style="border-top: 1px solid black;border-left: 1px solid black;", tags$div(style = localStyle, "import:")),
                                      tags$td(width = "90%", style="border-top: 1px solid black;",
                                              fileInput("wsInputFile",label=NULL,accept=c(".xlsx",".xls"))
                                      ),
                                      tags$td(width = "5%", style="border-top: 1px solid black;border-right: 1px solid black;",
                                              actionButton("wsInputFileLoad","Load"))
                                    ),
                                    tags$tr(
                                      tags$td(width = "5%", style="border-bottom: 1px solid black;border-left: 1px solid black;", tags$div(style = localStyle, "sheet:")),
                                      tags$td(width = "90%", style="border-bottom: 1px solid black;",
                                              selectInput("wsInputSheet",label=NULL,choices=c("sheet name"),selectize = FALSE)
                                      ),
                                      tags$td(width = "5%",style="border-bottom: 1px solid black;border-right: 1px solid black;")
                                    ),
                                    tags$tr(
                                      tags$td(width = "5%", style="border-bottom: 1px solid black;border-top: 1px solid black;border-left: 1px solid black;",tags$div(style = localStyle, "export:")),
                                      tags$td(width = "50%", style="border-bottom: 1px solid black;border-top: 1px solid black;",
                                              textInput("wsOutputFile",label=NULL)),
                                      tags$td(width = "5%", style="border-bottom: 1px solid black;border-top: 1px solid black;border-right: 1px solid black;",
                                              actionButton("WSOutputFileSave","Save")),
                                    ),
                                    tags$tr(
                                      tags$td(width = "5%", style="border-bottom: 1px solid black;border-top: 1px solid black;border-left: 1px solid black;",tags$div(style = localStyle, "clipboard:")),
                                      tags$td(width = "50%", style="border-bottom: 1px solid black;border-top: 1px solid black;",
                                              actionButton("wsPaste","Paste")),
                                      tags$td(width = "5%", style="border-bottom: 1px solid black;border-top: 1px solid black;border-right: 1px solid black;",
                                              actionButton("wsCopy","Copy")),
                                    )
                                    
                         ))
              ),
              tabPanel("Data",
                       style = paste("background: ",subpanelcolours$filesC), 
                       wellPanel(
                         style = paste("background: ",subpanelcolours$filesC,";"),
                         tags$table(width = "100%",class="myTable",
                                    tags$tr(
                                      tags$td(width = "5%", style="border-top: 1px solid black;border-left: 1px solid black;", tags$div(style = localStyle, "import:")),
                                      tags$td(width = "90%", style="border-top: 1px solid black;",
                                              fileInput("dataInputFile",label=NULL,accept=c(".xlsx",".xls"))
                                      ),
                                      tags$td(width = "5%", style="border-top: 1px solid black;border-right: 1px solid black;",
                                              actionButton("dataInputFileLoad","Load"))
                                    ),
                                    tags$tr(
                                      tags$td(width = "5%", style="border-bottom: 1px solid black;border-left: 1px solid black;", tags$div(style = localStyle, "sheet:")),
                                      tags$td(width = "90%", style="border-bottom: 1px solid black;",
                                              selectInput("dataInputSheet",label=NULL,choices=c("sheet name"),selectize = FALSE)
                                      ),
                                      tags$td(width = "5%", style="border-bottom: 1px solid black;border-right: 1px solid black;")
                                    ),
                                    tags$tr(
                                      tags$td(width = "5%", style="border-bottom: 1px solid black;border-top: 1px solid black;border-left: 1px solid black;",tags$div(style = localStyle, "export:")),
                                      tags$td(width = "50%", style="border-bottom: 1px solid black;border-top: 1px solid black;",
                                              textInput("DataoutputFile",label=NULL)),
                                      tags$td(width = "5%", style="border-bottom: 1px solid black;border-top: 1px solid black;border-right: 1px solid black;",
                                              actionButton("dataOutputFileSave","Save")),
                                    ),
                                    tags$tr(
                                      tags$td(width = "5%", style="border-bottom: 1px solid black;border-top: 1px solid black;border-left: 1px solid black;",tags$div(style = localStyle, "clipboard:")),
                                      tags$td(width = "50%", style="border-bottom: 1px solid black;border-top: 1px solid black;",
                                              actionButton("dPaste","Paste")),
                                      tags$td(width = "5%", style="border-bottom: 1px solid black;border-top: 1px solid black;border-right: 1px solid black;",
                                              actionButton("dCopy","Copy")),
                                    )
                         ))
              )
              # help tab
              ,tabPanel("?",
                        style = paste("background: ",subpanelcolours$filesC),
                        wellPanel(
                          style = paste("background: ",subpanelcolours$filesC,";"),
                          tags$table(width = "100%",class="myTable",
                                     tags$tr(
                                       tags$div(style = helpStyle, 
                                                tags$br("Import/export workspace/sample:"),
                                                tags$br(HTML('<b>'),"Importing:",HTML('</b>')),
                                                tags$br(HTML('&emsp;'), '1. select the file (.xls .xlsx)'),
                                                tags$br(HTML('&emsp;'), '2. go to Hypothesis:Variables to set up hypothesis'),
                                                tags$br(HTML('<b>'),"Exporting:",HTML('</b>')),
                                                tags$br(HTML('&emsp;'), '1. provide a file name (.xlsx)'),
                                                tags$br(HTML('&emsp;'), '2. press "Save"'),
                                                tags$br(HTML('&emsp;'),HTML('&emsp;'), 'current workspace/sample is saved'),
                                       ),
                                     )
                          )
                        )
              )
  )
)
