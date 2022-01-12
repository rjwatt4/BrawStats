plotDescriptionCols = c()
CatCatcols=c()

doLegendBars=TRUE
drawBars<-TRUE
drawBaseline<-TRUE

source("varUtilities.R")


drawParParPrediction<-function(g,IV,DV,rho,n,offset=1){
  if (offset==1) {
    col<- plotcolours$descriptionC
    xoff=0
  } else {
    off=offset-2
    col<- col2rgb(plotcolours$descriptionC1)*(1-off)+col2rgb(plotcolours$descriptionC2)*off
    col<- rgb(col[1]/255,col[2]/255,col[3]/255)
    xoff=-0.25+off*0.5
  }
  
  x<-seq(-fullRange,fullRange,0.01)
  y<-x*rho
  se<-sqrt((1+x^2)/n)*qnorm(0.975)
  y_lower<-y-se
  y_upper<-y+se
  yv_lower<-y_lower*DV$sd+DV$mu
  yv_upper<-y_upper*DV$sd+DV$mu
  
  x<-x*IV$sd+IV$mu
  y<-y*DV$sd+DV$mu
  xv<-c(x,rev(x))
  
  pts2<-data.frame(x=x,y=y)
  pts1<-data.frame(x=xv,y=c(yv_lower,rev(yv_upper)))
  g<-g+geom_polygon(data=pts1,aes(x=x,y=y),fill = col, alpha=0.5)
  if (offset==1) {
    g<-g+geom_line(data=pts2,aes(x=x,y=y),colour=col,lwd=2)
  } else {
    g<-g+geom_line(data=pts2,aes(x=x,y=y),colour=col,lwd=2)
  }
  g
  
}

drawCatParPrediction<-function(g,IV,DV,rho,n,offset= 1){
  if (offset==1) {
    col<- plotcolours$descriptionC
    xoff=0
  } else {
    off=offset-2
    col<- col2rgb(plotcolours$descriptionC1)*(1-off)+col2rgb(plotcolours$descriptionC2)*off
    col<- rgb(col[1]/255,col[2]/255,col[3]/255)
    xoff=-0.25+off*0.5
  }
  
  ncats<-IV$ncats
  b<-(1:ncats)-1
  xv<-b
  
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


drawParOrdPrediction<-function(g,IV,DV,rho,n,offset=1){
  if (offset==1) {
  col<- plotcolours$descriptionC
  xoff=0
  } else   {
    off=offset-2
    col<- col2rgb(plotcolours$descriptionC1)*(1-off)+col2rgb(plotcolours$descriptionC2)*off
    col<- rgb(col[1]/255,col[2]/255,col[3]/255)
    xoff=-0.25+off*0.5
  }
  
  x<-seq(-fullRange,fullRange,0.01)
  y<-x*rho
  se<-sqrt((1+x^2)/n)*qnorm(0.975)
  y_lower<-y-se
  y_upper<-y+se
  yv_lower<-y_lower*DV$spread+(DV$nlevs+1)/2
  yv_upper<-y_upper*DV$spread+(DV$nlevs+1)/2
  
  xv<-x*IV$sd+IV$mu
  yv<-y*DV$spread+(DV$nlevs+1)/2
  xv<-c(xv,rev(xv))
  yv<-c(yv,rev(yv))
  
  pts<-data.frame(x=xv,y=yv)
  g+geom_polygon(data=pts,aes(x=xv,y=c(yv_lower,rev(yv_upper))),fill = col, alpha=0.5)+
    geom_line(data=pts,aes(x=x,y=y),colour=col,lwd=2)
  
}

drawCatOrdPrediction<-function(g,IV,DV,rho,n,offset= 1){
  if (offset==1) {
    col<- plotcolours$descriptionC
    xoff=0
  } else {
    off=offset-2
    col<- col2rgb(plotcolours$descriptionC1)*(1-off)+col2rgb(plotcolours$descriptionC2)*off
    col<- rgb(col[1]/255,col[2]/255,col[3]/255)
    xoff=-0.25+off*0.5
  }
  
  nlevs<-DV$nlevs
  ncats<-IV$ncats
  b<-(1:ncats)-1
  xv<-b
  
  if (length(IV$vals)==0){
    d<-rho/sqrt(1-rho^2)/2*xv/(sd(xv)*sqrt(1-1/ncats))
    d<-d+(nlevs+1)/2
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
  if (offset==1) {
    col<- plotcolours$descriptionC1
    xoff=0
    barwidth=0.75
  } else {
    off=offset-2
    col<- col2rgb(plotcolours$descriptionC1)*(1-off)+col2rgb(plotcolours$descriptionC2)*off
    col<- rgb(col[1]/255,col[2]/255,col[3]/255)
    xoff=-0.25+off*0.5
    barwidth=0.25
  }
  
  ncats<-DV$ncat
  l<-DV$cases
  b<-(1:ncats)-1

  x<-seq(-fullRange,fullRange,0.01)
  y<-x*rho
  se=sqrt((1+x^2)/n)*qnorm(0.975)
  
  y_lower<-y-se
  y_upper<-y+se
  yv_lower<-pnorm(y_lower)
  yv_upper<-pnorm(y_upper)
  
  x<-x*IV$sd+IV$mu
  y<-pnorm(y)
  
  xv<-c(x,rev(x))
  
  pts2<-data.frame(x=x,y=y)
  pts1<-data.frame(x=xv,y=c(yv_lower,rev(yv_upper)))
  g<-g+
    geom_polygon(data=pts1,aes(x=x,y=y),fill = col, alpha=0.5)+
    geom_line(data=pts2,aes(x=x,y=y),colour=col,lwd=2)
  
  if (drawBars) {
    if (length(IV$vals)>0)  {
      bin_breaks<-c(-Inf,seq(-1,1,length.out=10)*fullRange*sd(IV$vals)+mean(IV$vals),Inf)
      dens2<-hist(IV$vals,breaks=bin_breaks,freq=TRUE,plot=FALSE,warn.unused = FALSE)
      bins=dens2$mids
      
      full_x<-c()
      full_y<-c()
      full_f<-c()
      for (i2 in 1:DV$ncats){
        dens1<-hist(IV$vals[DV$vals==DV$cases[i2]],breaks=bin_breaks,freq=TRUE,plot=FALSE,warn.unused = FALSE)
        densities<-dens1$counts/dens2$counts
        xoff<-(i2-1)/(DV$ncats-1)-(DV$ncats-1)/2
        full_x<-c(full_x,bins[2:(length(bins)-1)]+xoff/4)
        full_y<-c(full_y,densities[2:(length(bins)-1)])
        full_f<-c(full_f,rep(i2,length(bins)-2))
      }
      
      pts<-data.frame(x=full_x,y=full_y,fill=full_f)
      if (doLegendBars) {
        g<-g+geom_bar(data=pts,aes(x=full_x,y=full_y,fill=factor(full_f)),stat="identity",width=barwidth/(DV$ncats+1))
      } else {
        g<-g+geom_bar(data=pts,aes(x=full_x,y=full_y),stat="identity",width=barwidth/(DV$ncats+1),fill=col)
      }
    } else {
      dens2<-1
      bins<-seq(-1,1,length.out=10)*fullRange*IV$sd+IV$mu
      full_x<-c()
      full_y<-c()
      full_f<-c()
      for (i2 in 1:DV$ncats){
        dens1<-pnorm(bins*rho)
        if (i2-1==0)dens1<-1-dens1
        densities<-dens1/dens2
        xoff<-(i2-1)/(DV$ncats-1)-(DV$ncats-1)/2
        full_x<-c(full_x,bins[2:(length(bins)-1)]+xoff/4)
        full_y<-c(full_y,densities[2:(length(bins)-1)])
        full_f<-c(full_f,rep(i2,length(bins)-2))
      }
      pts<-data.frame(x=full_x,y=full_y,fill=full_f)
      if (offset==1) {
        col<-CatCatcols[i2]
      }
      if (doLegendBars) {
        g<-g+geom_bar(data=pts,aes(x=x,y=y,fill=factor(fill)),stat="identity",width=barwidth/(DV$ncats+1))
      } else {
        g<-g+geom_bar(data=pts,aes(x=full_x,y=full_y),stat="identity",width=barwidth/(DV$ncats+1),fill=col)
      }
    }
    if (doLegendBars && offset==1){
      g<-g+scale_fill_manual(name=DV$name,values=CatCatcols)
    }
  }
  if (drawBaseline) {
    pts1<-data.frame(x=c(-1,1)*fullRange*IV$sd+IV$mu,y=c(0,0))
    g<-g+geom_line(data=pts1,aes(x=x,y=y),color="black")
  }
  
  g
  
}

drawCatCatPrediction<-function(g,IV,DV,rho,n,offset= 1){
  if (offset==1) {
    col<- plotcolours$descriptionC
    xoff=0
    barwidth=0.75
  } else {
    off=offset-2
    col<- col2rgb(plotcolours$descriptionC1)*(1-off)+col2rgb(plotcolours$descriptionC2)*off
    col<- rgb(col[1]/255,col[2]/255,col[3]/255)
    xoff=-0.25+off*0.5
    barwidth=0.5
  }
  
  ncats1<-IV$ncats
  ncats2<-DV$ncats
  l1=IV$cases
  b1<-(1:ncats1)-1
  
  if (length(IV$vals)>0)  {
    pp<-matrix(NA,ncats2,ncats1)
    yv<-as.numeric(DV$vals)
    for (i1 in 1:ncats1) {
      for (i2 in 1:ncats2) {
        pp[i2,i1]<-sum(yv[IV$vals==IV$cases[i1]]==i2)/length(IV$vals)
      }
    }
  } else {
    pp<-r2CatProportions(rho,ncats1,ncats2)
  }

  full_x<-c()
  full_y<-c()
  full_f<-c()
  full_c<-c()
  for (i2 in 1:ncats2){
    full_x<-c(full_x,b1+xoff+(i2-1-(ncats2-1)/2)/(ncats2+1))
    full_y<-c(full_y,pp[i2,])
    full_f<-c(full_f,rep(i2,length(pp[i2,])))
    full_c<-c(full_f,rep(CatCatcols[i2],length(pp[i2,])))
  }
  
  pts<-data.frame(x=full_x,y=full_y,fill=full_f)
  if (offset==1) {
    if (doLegendBars) {
      g<-g+geom_bar(data=pts,aes(x=x,y=y,fill=factor(full_f)),stat="identity",width=barwidth/(ncats2+1))
    } else {
      g<-g+geom_bar(data=pts,aes(x=x,y=y),stat="identity",width=barwidth/(ncats2+1),fill=full_c)
    }
  } else{
    g<-g+geom_bar(data=pts,aes(x=x,y=y),stat="identity",width=barwidth/(ncats2+1),fill=col)
  }

  if (doLegendBars && offset==1){
    g<-g+scale_fill_manual(name=DV$name,values=CatCatcols,labels=DV$cases)
  }
  
  if (offset<=2){
    g<-g+scale_x_continuous(breaks=b1,labels=l1)
  }
  pts1<-data.frame(x=c(-1,ncats1),y=c(0,0))
  g<-g+geom_line(data=pts1,aes(x=x,y=y),color="black")
  g
  
}


drawPrediction<-function(IV,IV2,DV,effect,design,offset=1,g=NULL){
  
  n<-design$sN
  hypothesisType=paste(IV$type,DV$type,sep=" ")
  if (is.null(g))  {
    g<-ggplot()
  }
  
  if (is.null(IV2)){
    doLegendBars<<-TRUE
    if (is.null(CatCatcols) && DV$type=="Categorical") {
      CatCatcols <<- c()
      cols<-c()
      for (i2 in 1:DV$ncats) {
        off<-(i2-1)/(DV$ncats-1)
        col<-col2rgb(plotcolours$descriptionC1)*off+col2rgb(plotcolours$descriptionC2)*(1-off)
        col<- rgb(col[1]/255,col[2]/255,col[3]/255)
        cols<-c(cols,col)
      }
      # names(cols)<-DV$cases
      CatCatcols<<-cols
    }
    
    rho<-effect$rIV
    if (is.na(rho)) {rho<-0}
    
    switch (hypothesisType,
            "Interval Interval"={
              g<-drawParParPrediction(g,IV,DV,rho,n,offset)
            },
            "Categorical Interval"={
              g<-drawCatParPrediction(g,IV,DV,rho,n,offset)
            },
            "Interval Ordinal"={
              g<-drawParOrdPrediction(g,IV,DV,rho,n,offset)
            },
            "Categorical Ordinal"={
              g<-drawCatOrdPrediction(g,IV,DV,rho,n,offset)
            },
            "Interval Categorical"={
              g<-drawParCatPrediction(g,IV,DV,rho,n,offset)
            },
            "Categorical Categorical"={
              g<-drawCatCatPrediction(g,IV,DV,rho,n,offset)
            }
    )
    
  } else {
    doLegendBars<<-FALSE
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
              g<-g+coord_cartesian(xlim = c(0,IV$ncats+1)-1, ylim = c(-1,1)*1.5*DV$sd+DV$mu)
            },
            "Interval Ordinal"={
              g<-g+coord_cartesian(xlim = c(-1,1)*fullRange*IV$sd+IV$mu, ylim = c(0,DV$nlevs+1))
              l=paste(1:DV$nlevs,sep="")
              b<-1:DV$nlevs
              g<-g+scale_y_continuous(breaks=b,labels=l)
            },
            "Categorical Ordinal"={
              g<-g+coord_cartesian(xlim = c(0,IV$ncats+1)-1, ylim = c(0,DV$nlevs+1))
              l=paste(1:DV$nlevs,sep="")
              b<-1:DV$nlevs
              g<-g+scale_y_continuous(breaks=b,labels=l)
            },
            "Interval Categorical"={
              g<-g+coord_cartesian(xlim = c(-1,1)*fullRange*IV$sd+IV$mu, ylim = c(0.75,DV$ncats+0.25)-1)
              g<-g+scale_y_continuous(breaks=seq(0,1,0.2))
            },
            "Categorical Categorical"={
              g<-g+coord_cartesian(xlim = c(0,IV$ncats+1)-1, ylim = c(-0.1,1.1))
              g<-g+scale_y_continuous(breaks=seq(0,1,0.2))
            }
    )
  } else {
      if (DV$type=="Categorical") {
        g<-g+coord_cartesian(ylim = c(-0.1,1.1))
        g<-g+scale_y_continuous(breaks=seq(0,1,0.2))
      } else {
        
      }
    }
  g<-g+labs(x=IV$name,y=DV$name)+plotTheme+theme(plot.margin=popplotMargins)
  
}
