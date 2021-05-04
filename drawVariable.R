source("plotVariable.R")

drawVariable<-function(var){
  switch(var$type,
         "Interval"={drawInterval(var)},
         "Categorical"={drawCategorical(var)}
  )
  
}