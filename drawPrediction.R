r2CatProportions<-function(rho,ncats1,ncats2) {
  
  # find proportions in each cell
  sigma<-matrix(c(1,rho,rho,1),nrow=2)
  mu<-c(0,0)
  xbreaks<-qnorm(seq(0,1,1/ncats1))
  ybreaks<-qnorm(seq(0,1,1/ncats2))
  division<-matrix(ncol=ncats1+1,nrow=ncats2)
  for (ix in 1:ncats1+1){
    whole<-pmnorm(c(xbreaks[ix],Inf),mu,sigma)
    divis<-pmnorm(matrix(c(rep(xbreaks[ix],ncats2+1),ybreaks),ncol=2,byrow=FALSE),mu,sigma)
    division[,ix]<-diff(divis)
  }
  division[,1]<-0
  division<-t(diff(t(division)))
  division
}


drawParParPrediction<-function(g,IV,DV,rho,n,offset=1){
  if (offset==1)
  {  col<- plotcolours$descriptionC
  xoff=0}
  else
  {  off=offset-2
  col<- col2rgb(plotcolours$descriptionC1)*(1-off)+col2rgb(plotcolours$descriptionC2)*off
  col<- rgb(col[1]/255,col[2]/255,col[3]/255)
  xoff=-0.25+off*0.5}

  x<-seq(-fullRange,fullRange,0.01)
  y<-x*rho
  se<-sqrt((1+x^2)/n)*qnorm(0.975)
  y_lower<-y-se
  y_upper<-y+se
  yv_lower<-y_lower*DV$sd+DV$mu
  yv_upper<-y_upper*DV$sd+DV$mu
  
  xv<-x*IV$sd+IV$mu
  yv<-y*DV$sd+DV$mu
  xv<-c(xv,rev(xv))
  yv<-c(yv,rev(yv))
  
  pts<-data.frame(x=xv,y=yv)
    g+geom_polygon(data=pts,aes(x=xv,y=c(yv_lower,rev(yv_upper))),fill = col, alpha=0.5)+
    geom_line(data=pts,aes(x=x,y=y),colour=col,lwd=2)
    
}

drawCatParPrediction<-function(g,IV,DV,rho,n,offset= 1){
  if (offset==1)
  {  col<- plotcolours$descriptionC
  xoff=0}
  else
  {  off=offset-2
  col<- col2rgb(plotcolours$descriptionC1)*(1-off)+col2rgb(plotcolours$descriptionC2)*off
  col<- rgb(col[1]/255,col[2]/255,col[3]/255)
  xoff=-0.25+off*0.5}

  ncats<-IV$ncats
  b<-(1:ncats)*2-(ncats+1)
  b<-drawCatPositions(ncats)
  xv<-(1:ncats)*2-(ncats+1)
  
  if (length(IV$vals)==0){
    d<-rho/sqrt(1-rho^2)/2*xv/(sd(xv)*sqrt(1-1/ncats))
    d<-d*DV$sd+DV$mu
  } else{
    x<-IV$vals
    y<-DV$vals
    d<-array(0,ncats)
    for (i in 1:ncats){
      d[i]<-mean(y[x==IV$cases[i]])
    }
  }
  l<-IV$cases
  
  se<-rep(DV$sd^2*sqrt(1-rho^2)/sqrt(n/ncats),ncats)
  se<-se*2
  mn_pts<-data.frame(xm=b+xoff,ym=d,se=se)
  g<-g+
    geom_line(data=mn_pts,aes(x=xm,y=ym))+
    geom_errorbar(data=mn_pts,aes(x=xm, ymin=ym-se, ymax=ym+se),width=0.2)+
    geom_point(data=mn_pts,aes(x=xm,y=ym), shape=21, colour = "black", fill = col, size = 7)
  if (offset<=2){
    g<-g+scale_x_continuous(breaks=b,labels=l)
  }
  g
  
}

drawParCatPrediction<-function(g,IV,DV,rho,n,offset= 1){
  if (offset==1)
  {  col<- plotcolours$descriptionC
  xoff=0}
  else
  {  off=offset-2
  col<- col2rgb(plotcolours$descriptionC1)*(1-off)+col2rgb(plotcolours$descriptionC2)*off
  col<- rgb(col[1]/255,col[2]/255,col[3]/255)
  xoff=-0.25+off*0.5}
  
  ncats<-DV$ncats
  l=paste("D",1:ncats,sep="")
  # b<-(1:ncats)*2-(ncats+1)
  b<-drawCatPositions(ncats)
  b<-c(0,1)
  
  x<-seq(-fullRange,fullRange,0.01)
  y<-x*rho
  se=sqrt((1+x^2)/n)*qnorm(0.975)
  
  y_lower<-y-se
  y_upper<-y+se
  yv_lower<-pnorm(y_lower)*(b[2]-b[1])+b[1]
  yv_upper<-pnorm(y_upper)*(b[2]-b[1])+b[1]
  
  xv<-x*IV$sd+IV$mu
  yv<-pnorm(y)*(b[2]-b[1])+b[1]
  xv<-c(xv,rev(xv))
  yv<-c(yv,rev(yv))
  
  pts<-data.frame(x=xv,y=yv)
  g<-g+
    geom_polygon(data=pts,aes(x=xv,y=c(yv_lower,rev(yv_upper))),fill = col, alpha=0.5)+
    geom_line(data=pts,aes(x=x,y=y),colour=col,lwd=2)
  if (offset<=2){
    g<-g+scale_y_continuous(breaks=c(0,1),labels=l)
  }
  g

}

drawCatCatPrediction<-function(g,IV,DV,rho,n,offset= 1){
  if (offset==1)
  {  col<- plotcolours$descriptionC
  xoff=0
  barwidth=1}
  else
  {  off=offset-2
  col<- col2rgb(plotcolours$descriptionC1)*(1-off)+col2rgb(plotcolours$descriptionC2)*off
  col<- rgb(col[1]/255,col[2]/255,col[3]/255)
  xoff=-0.25+off*0.5
  barwidth=0.5}
  
  ncats1<-IV$ncats
  ncats2<-DV$ncats
  l1=IV$cases
  b<-drawCatPositions(ncats1)

  if (length(IV$vals)>0)  {
    yv<-as.numeric(DV$vals)-1
    yz<-c()
    for (i in 1:IV$ncats){ 
      yz<-c(yz,mean(yv[IV$vals==IV$cases[i]]))
      }
    y<-yz
  } else {
  y<-r2CatProportions(rho,ncats1,ncats2)
  y<-y[2,]*2
  }
  pts<-data.frame(x=b+xoff,y=y)
  g<-g+
    geom_bar(data=pts,aes(x=x,y=y),stat="identity",width=barwidth,colour="black",fill=col)
  if (offset<=2){
    g<-g+scale_x_continuous(breaks=b,labels=l1)
  }
  g

}


drawPrediction<-function(IV,IV2,DV,effect,design,offset=1,g=NULL){
  
  n<-design$sN
  hypothesisType=paste(IV$type,DV$type,sep=" ")
  if (is.null(g))  g<-ggplot()
  
  if (is.null(IV2)){
    rho<-effect$rIV
    if (is.na(rho)) {rho<-0}
    
    switch (hypothesisType,
            "Interval Interval"={
              g<-drawParParPrediction(g,IV,DV,rho,n,offset)
            },
            "Categorical Interval"={
              g<-drawCatParPrediction(g,IV,DV,rho,n,offset)
            },
            "Interval Categorical"={
              g<-drawParCatPrediction(g,IV,DV,rho,n,offset)
            },
            "Categorical Categorical"={
              g<-drawCatCatPrediction(g,IV,DV,rho,n,offset)
            }
    )
    
  } else {
    roff=0.82
    switch (IV2$type,
            "Interval"= rho<-effect$rIV+c(-1,1)*effect$rIVIV2DV,
            "Categorical"= rho<-effect$rIV+seq(-1,1,length.out=IV2$ncats)*effect$rIVIV2DV
    )
    rho[is.na(rho)] <- 0
    
    for (i in 1:length(rho)) {
      offset=2+(i-1)/(length(rho)-1)
      switch (hypothesisType,
              "Interval Interval"={
                g<-drawParParPrediction(g,IV,DV,rho[i],n,offset)
              },
              "Categorical Interval"={
                g<-drawCatParPrediction(g,IV,DV,rho[i],n,offset)
              },
              "Interval Categorical"={
                g<-drawParCatPrediction(g,IV,DV,rho[i],n,offset)
              },
              "Categorical Categorical"={
                g<-drawCatCatPrediction(g,IV,DV,rho[i],n,offset)
              }
      )
    }
  }
  if (offset<=2){
    switch (hypothesisType,
            "Interval Interval"={
              g<-g+coord_cartesian(xlim = c(-1,1)*fullRange*IV$sd+IV$mu, ylim = c(-1,1)*fullRange*DV$sd+DV$mu)
            },
            "Categorical Interval"={
              g<-g+coord_cartesian(xlim = c(-1,1)*fullRange*IV$sd+IV$mu, ylim = c(-1,1)*1.5*DV$sd+DV$mu)
            },
            "Interval Categorical"={
              g<-g+coord_cartesian(xlim = c(-1,1)*fullRange*IV$sd+IV$mu, ylim = c(0,1)*1.05)
            },
            "Categorical Categorical"={
              g<-g+coord_cartesian(xlim = c(-1,1)*fullRange*IV$sd+IV$mu, ylim = c(0,1)*1.05)
            }
    )
    }
  g+labs(x=IV$name,y=DV$name)+plotTheme+theme(plot.margin=popplotMargins)
  
}
