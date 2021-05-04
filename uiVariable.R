variableDialog<-wellPanel(
  style = paste("background: ",subpanelcolours$hypothesisC,";"),
  tags$table(width = "100%",class="myTable",
             tags$tr(
               tags$td(width = "30%", tags$div(style = localStyle, "Name:")),
               tags$td(width = "70%", textInput("MVname", value = MV$name, label = NULL))
             ),
             tags$tr(
               tags$td(width = "30%", div(style = localStyle, "Type:")),
               tags$td(width = "70%", 
                       selectInput("MVtype", label= NULL,
                                   c("Interval" = "Interval",
                                     "Categorical" = "Categorical"
                                   ),selected=MV$type,
                                   selectize=FALSE
                       )
               )
             )
  ),
  conditionalPanel(condition="input.MVtype == 'Interval'",
                   tags$table(width = "100%",class="myTable",
                              tags$tr(id="MVIntVal1",
                                      tags$td(width = "30%",  tags$div(style = localStyle, "Mean:")),
                                      tags$td(width = "20%", numericInput("MVmu", value = MV$mu, label = NULL)),
                                      tags$td(width = "10%",  tags$div(style = localStyle, "Sd:")),
                                      tags$td(width = "20%", numericInput("MVsd", value = MV$sd, label = NULL)),
                                      tags$td(width = "20%",  tags$div(style = localStyle, " "))
                              ),
                              tags$tr(id="MVIntVal2",
                                      tags$td(width = "30%",  tags$div(style = localStyle, "Skew:")),
                                      tags$td(width = "20%",  numericInput("MVskew", value = MV$skew, step=0.1, label = NULL)),
                                      tags$td(width = "10%",  tags$div(style = localStyle, "Kurtosis:")),
                                      tags$td(width = "20%",  numericInput("MVkurt", value = MV$kurtosis, step=0.1, label = NULL)),
                                      tags$td(width = "20%",  tags$div(style = localStyle, " "))
                              ),
                   )),
  conditionalPanel(condition="input.MVtype == 'Categorical'",
                   tags$table(width = "100%",class="myTable",
                              tags$tr(id="MVCatVal1",
                                      tags$td(width = "30%",  tags$div(style = localStyle, "No cases:")),
                                      tags$td(width = "20%", numericInput("MVncats", value = MV$ncats, label = NULL,step=1,min=2)),
                                      tags$td(width = "30%",  tags$div(style = localStyle, "Proportions:")),
                                      tags$td(width = "20%", textInput("MVprop", value = MV$proportions, label = NULL)),
                                      # tags$td(width = "10%",  tags$div(style = localStyle, " "))
                              ),
                   ),
                   tags$table(width = "100%",class="myTable",
                              tags$tr(id="MVCatVal2",
                                      tags$td(width = "30%",  tags$div(style = localStyle, "Cases:")),
                                      tags$td(width = "60%", textInput("MVcases", value = MV$cases, label = NULL)),
                                      tags$td(width = "10%",  tags$div(style = localStyle, " "))
                              )
                   )),
  width="100%")


