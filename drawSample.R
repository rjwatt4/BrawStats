drawSample<-function(IV,DV,effect,design,result){
  
  # the scattered points
  x<-result$ivplot
  y<-result$dvplot
  g<-drawPopulation(result$IVs,result$DVs,result,design,alpha=0.5)

  dotSize<-7
  if (length(x)>100) {
    dotSize<-7*sqrt(100/length(x))
  }
  pts<-data.frame(x=x,y=y);
  g<-g+geom_point(data=pts,aes(x=x,y=y),shape=21, colour = "black", fill = plotcolours$sampleC, size = dotSize)
  labs(x=IV$name,y=DV$name)+plotTheme
  g
  
}
