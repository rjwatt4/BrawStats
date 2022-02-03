drawSample<-function(IV,DV,effect,result){
  
  # the scattered points
  x<-result$ivplot
  y<-result$dvplot
  
  result$Heteroscedasticity<-effect$Heteroscedasticity
  g<-drawPopulation(IV,DV,effect,alpha=0.5)

  dotSize<-7
  if (length(x)>100) {
    dotSize<-7*sqrt(100/length(x))
  }
  pts<-data.frame(x=x,y=y);
  g<-g+geom_point(data=pts,aes(x=x,y=y),shape=21, colour = "black", fill = plotcolours$sampleC, size = dotSize)+
  labs(x=IV$name,y=DV$name)+plotTheme
  g
  
}
