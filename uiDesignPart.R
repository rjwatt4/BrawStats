designPanel <- function(prefix="") {
  wellPanel(
    style = paste("background: ",subpanelcolours$designC,";"),
    tags$table(width = "100%",class="myTable",
               tags$tr(
                 tags$td(width = "35%", tags$div(style = labelStyle, "Design:")),
                 tags$td(width = "15%"),
                 tags$td(width = "35%"),
                 tags$td(width = "15%")
               ),
               tags$tr(
                 tags$td(width = "35%", tags$div(style = localStyle, "sample size:")),
                 tags$td(width = "15%", 
                         numericInput(paste0(prefix,"sN"), label=NULL,value=42)
                 ),
                 tags$td(width = "35%"),
                 tags$td(width = "15%")
               )
    ),
    tags$table(width = "100%",class="myTable",
               tags$tr(
                       tags$td(width = "35%", tags$div(style = localStyle, "method:")),
                       tags$td(width = "65%", 
                               selectInput(paste0(prefix,"sMethod"),label=NULL,c("Random","Stratified","Cluster","Convenience","Snowball"),
                                           selected=design$sMethod,
                                           selectize=FALSE)
                       )
               ),
    )
  )
  
}