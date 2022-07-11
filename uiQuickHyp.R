if (is_local) {
quickHypotheses<-
  tags$table(width = "100%",class="myTable",
             tags$tr(
               tags$td(width = "45%",
                       tags$div(style = localStyle, "Hypothesis:")
               ),
               tags$td(width = "30%",
                       selectInput("Hypchoice", label = NULL,
                                   choices=
                                     list("i~i"="ii","i~o"="oi","i~c2"="c2i","i~c3"="c3i",
                                          "o~i"="io","o~o"="oo","o~c2"="c2o","o~c3"="c3o",
                                          "c~i"="ic","c~o"="oc","c~c2"="c2c","c~c3"="c3c"," ",
                                          "i~i+i"="iii","i~c+i"="cii","i~i+c"="ici","i~c+c"="cci","  ",
                                          "c~i+i"="iic","c~c+i"="cic","c~i+c"="icc","c~c+c"="ccc","empty"="ee"),
                                   selected="ee",
                                   selectize=FALSE)
               ),
               tags$td(width = "25%", tags$div(style = localStyle, " ")
               )
             ),
             tags$tr(
               tags$td(width = "45%",
                       tags$div(style = localStyle, "Presets:")
                       ),
               tags$td(width = "30%", 
                                        selectInput("Effectchoice", label = NULL,
                                                    choices=
                                                      list("zeroes"="e0","interaction"="e1","opposite"="e2"),
                                                    selected="none",
                                                    selectize=FALSE)
                       ),
               tags$td(width = "25%", tags$div(style = localStyle, " ")
               )
             )
  )
} else {
  quickHypotheses<-c()
}

