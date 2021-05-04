
varplotMargins<-margin(0.0,-0.2,0,-1,"cm");
darkYellow<-"#FFCC00"

drawVar<-function(pts,var){
  ggplot(pts,aes(x=r,y=dens))+geom_area(fill=plotcolours$sampleC)+geom_line(lwd=0.5)+
    labs(x=var$name,y="")+
    plotTheme+
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    theme(plot.margin=varplotMargins)+
    coord_cartesian(xlim = c(-1,1)*3*var$sd+var$mu, ylim = c(0, 1.2))
}


drawCategorical<-function(var){
  ng<-var$ncats
  r1<-c(-1, -1, 1, 1)*0.3
  d1<-c(0,1,1,0)
  pp<-as.numeric(unlist(strsplit(var$proportions,",")))
  pp<-pp/max(pp)
  if (length(pp)<ng) {pp<-c(pp,rep(pp[length(pp)],ng-length(pp)))}
  b<-(1:ng)*2-(ng+1)
  
  r<-c()
  dens<-c()
  for (i in 1:length(b)){
    r<-c(r,r1+b[i])
    dens<-c(dens,d1*pp[i])
  }
  r<-r/max(abs(r[1]),2)*2.5
  r<-c(-fullRange,r,fullRange)
  dens<-c(0,dens,0)
  
  l=var$cases
  
  pts=data.frame(r=r,dens=dens)
  drawVar(pts,var)+
    scale_x_continuous(breaks=b,labels=l)
  
}

drawInterval<-function(var){
  r<-seq(-fullRange,fullRange,0.1)*var$sd+var$mu
  if (var$skew!=0 || var$kurtosis!=0) {
    a<-f_johnson_M(var$mu,var$sd,var$skew,var$kurtosis)
    dens<-dJohnson(r,parms=a)
    dens[is.na(dens)]<-0
  } else {
  dens<-dnorm(r,var$mu,var$sd) # exp(-0.5*((r-var$mu)/var$sd)^2)
  }
  # dens<-dJohnson(r,list())
  dens[1]=0; dens[length(dens)]=0
  
  pts=data.frame(r=r,dens=dens/max(dens))
  drawVar(pts,var)
}
