source("varUtilities.R")

mv2dens<-function(x,rho,break1,break2){
  mu=c(0,0)
  sigma=matrix(c(1,rho,rho,1),ncol=2,byrow=TRUE)
  xd=x[2]-x[1]
  xi=c(x[1]-xd/2,x+xd/2)
  xd<-matrix(c(xi,rep(break1[1],length(xi))),ncol=2,byrow=FALSE)
  z1=diff(pmnorm(xd,mu,sigma))
  xd<-matrix(c(xi,rep(break2[1],length(xi))),ncol=2,byrow=FALSE)
  z2=diff(pmnorm(xd,mu,sigma))
  z2-z1
}


erf<-function(z){
  2*pnorm(sqrt(2)*z) - 1
}

drawCatPositions<-function(ncats){
  pbreaks<-seq(0,1,1/(ncats))
  ebreaks<-exp(-qnorm(pbreaks)^2/2)
  -1/sqrt(2*pi)*diff(ebreaks)/diff(pbreaks)
}


drawParParPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha){
  theta=seq(0,2*pi,2*pi/101)
  d<-acos(rho)
  x=cos(theta+d/2)
  y=cos(theta-d/2)
  y<-(y-x*rho)*(1+x/3*Heteroscedasticity)+x*rho
  pts=data.frame(x=x*2*IV$sd+IV$mu,y=y*2*DV$sd+DV$mu)
  g<-ggplot(pts,aes(x=x, y=y))+
    geom_polygon(fill = plotcolours$sampleC,color=plotcolours$sampleC,alpha=alpha)
  
}

drawCatParPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha){
  ncats<-IV$ncats
  l<-IV$cases
  pp<-CatProportions(IV)
  b<-(1:ncats)-1

  pbreaks<-seq(0,1,1/(ncats))
  ebreaks<-qnorm(pbreaks)
  
  y<-seq(-1,1,0.01)*fullRange
  pts=data.frame(x=y*0,y=y)
  g<-ggplot(pts,aes(x=x,y=y))
  yshape<-c(y,rev(y))
  if (length(IV$vals)>0){
    # dealing with a sample
    muv<-array(0,ncats)
    sdv<-array(0,ncats)
    for (id in 1:ncats) {
      muv[id]<-mean(DV$vals[IV$vals==IV$cases[id]],na.rm=TRUE)
      sdv[id]<-sd(DV$vals[IV$vals==IV$cases[id]],na.rm=TRUE)
    }
    mu_order<-order(muv)
  } else {
    muv<-array(DV$mu,ncats)
    sdv<-array(DV$sd,ncats)
    mu_order<-1:ncats
    if (rho<0) {mu_order<-rev(mu_order)}
  }
  hsy<-1+seq(-1,1,length.out=ncats)*Heteroscedasticity
  for (id in 1:ncats) {
    use<-mu_order[id]
        x<-mv2dens(y,abs(rho),ebreaks[use],ebreaks[use+1])
    x<-x/max(x,na.rm=TRUE)*(b[2]-b[1])/2.2
    xshape<-c(-x,rev(x))*pp[id]
    pts<-data.frame(x=xshape+b[id],y=yshape*hsy[id]*sdv[id]+muv[id])
    g<-g+
      geom_polygon(data=pts,aes(x=x,y=y),fill = plotcolours$sampleC,color=plotcolours$sampleC,alpha=alpha)
  }
  g+scale_x_continuous(breaks=b,labels=l)

}

drawParOrdPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha){
  ng<-DV$nlevs
  l=paste(1:ng,sep="")
  b<-1:ng

  pbreaks<-seq(0,1,1/(ng))
  ebreaks<-(pbreaks-0.5)*2

  x<-seq(-1,1,0.01)*fullRange
  pts=data.frame(x=x,y=x*0)
  g<-ggplot(pts,aes(x=x,y=y))
  xshape<-c(x,rev(x))
  for (id in 1:ng) {
    y<-mv2dens(x,rho,ebreaks[id],ebreaks[id+1])
    y<-y/max(y)
    yshape<-c(-y,rev(y))/ng*1.75
    pts<-data.frame(x=xshape*IV$sd+IV$mu,y=yshape+b[id])
    g<-g+
      geom_polygon(data=pts,aes(x=x,y=y),fill = plotcolours$sampleC,color=plotcolours$sampleC,alpha=1)
  }
  g+scale_y_continuous(breaks=b,labels=l)
  
}

drawCatOrdPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha){
  ncats1<-IV$ncats
  pp1<-CatProportions(IV)
  l1=IV$cases
  b1<-(1:ncats1)-1
  
  ng2<-DV$nlevs
  b2<-1:ng2
  l2=1:ng2
  
  division<-r2CatProportions(rho,ncats1,ng2)  
  pp<-exp(-0.5*(((1:ng2)-DV$median)/(DV$iqr/2))^2)
  pp<-pp/max(pp)
  s<-division/max(division)
  x<-c(-1,-1,1,1)*min(diff(b1))/2*0.9
  y<-c(-1,1,1,-1)*min(diff(b2))/2*0.9
  
  pts=data.frame(x=x,y=y)
  g<-ggplot(pts,aes(x=x,y=y))
  for (ix in 1:ncats1) {
    for (iy in 1:ng2) {
      # xoff<-sign(b1[ix])*abs(x[1])*(1-s[iy,ix])
      # yoff<-sign(b2[iy])*abs(y[1])*(1-s[iy,ix])
      # pts<-data.frame(x=x*s[iy,ix]+b1[ix]-xoff, y=y*s[iy,ix]+b2[iy]-yoff)
      pts<-data.frame(x=x*pp[iy]*pp1[ix]+b1[ix], y=y*s[iy,ix]+b2[iy])
      g<-g+
        geom_polygon(data=pts,aes(x=x,y=y),fill = plotcolours$sampleC,colour=plotcolours$sampleC,alpha=alpha)
    }
  }
  g+scale_x_continuous(breaks=b1,labels=l1)+scale_y_continuous(breaks=b2,labels=l2)
}

drawParCatPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha){
  ncats<-DV$ncats
  pp<-CatProportions(DV)
  l=DV$cases
  b<-(1:ncats)-1

  pbreaks<-seq(0,1,1/(ncats))
  ebreaks<-qnorm(pbreaks)
  
  x<-seq(-1,1,0.01)*fullRange
  pts=data.frame(x=x,y=x*0)
  g<-ggplot(pts,aes(x=x,y=y))
  xshape<-c(x,rev(x))
  for (id in 1:ncats) {
    y<-mv2dens(x,rho,ebreaks[id],ebreaks[id+1])
    y<-y/max(y)/2.5
    
    yshape<-c(-y,rev(y))*pp[id]
    pts<-data.frame(x=xshape*IV$sd+IV$mu,y=yshape+b[id])
    g<-g+
      geom_polygon(data=pts,aes(x=x,y=y),fill = plotcolours$sampleC,color=plotcolours$sampleC,alpha=alpha)
  }
  g+scale_y_continuous(breaks=b,labels=l)

}

drawCatCatPopulation<-function(IV,DV,rho,Heteroscedasticity,alpha){
  ncats1<-IV$ncats
  pp1<-CatProportions(IV)
  b1<-(1:ncats1)-1
  l1=IV$cases
  
  ncats2<-DV$ncats
  pp2<-CatProportions(DV)
  b2<-(1:ncats2)-1
  l2=DV$cases
  
  division<-r2CatProportions(rho,ncats1,ncats2)
  s<-division/max(division)
  x<-c(-1,-1,1,1)*min(diff(b1))/2*0.9
  y<-c(-1,1,1,-1)*min(diff(b2))/2*0.9

  pts=data.frame(x=x,y=y)
  g<-ggplot(pts,aes(x=x,y=y))
  for (ix in 1:ncats1) {
    for (iy in 1:ncats2) {
      # xoff<-sign(b1[ix])*abs(x[1])*(1-s[iy,ix])
      # yoff<-sign(b2[iy])*abs(y[1])*(1-s[iy,ix])
      # pts<-data.frame(x=x*s[iy,ix]+b1[ix]-xoff, y=y*s[iy,ix]+b2[iy]-yoff)
      pts<-data.frame(x=x*s[iy,ix]*pp1[ix]*pp2[iy]+b1[ix], y=y*s[iy,ix]*pp1[ix]*pp2[iy]+b2[iy])
      g<-g+
        geom_polygon(data=pts,aes(x=x,y=y),fill = plotcolours$sampleC,colour=plotcolours$sampleC,alpha=alpha)
    }
  }
  g+scale_x_continuous(breaks=b1,labels=l1)+scale_y_continuous(breaks=b2,labels=l2)
}

drawPopulation<-function(IV,DV,effect,alpha=1){
  rho<-effect$rIV
  if (is.na(rho)) {rho<-0}
  
  hypothesisType=paste(IV$type,DV$type,sep=" ")
  heteroscedasticity<-effect$Heteroscedasticity[1]
  
  if (IV$type=="empty" || DV$type=="empty") {
    pts<-data.frame(x=100,y=100)
    g<-ggplot(pts,aes(x=x,y=y))
    if (IV$type!="empty") {
      switch (IV$type, 
              "Categorical"={
                ncats1<-IV$ncats
                b1<-(1:ncats1)-1
                l1=IV$cases
                g<-g+scale_x_continuous(breaks=b1,labels=l1)+scale_y_continuous(breaks=NULL)+coord_cartesian(xlim = c(0,ncats1+1)-1,ylim=c(0,1))
                },
              "Interval"={
                g<-g+scale_x_continuous()+scale_y_continuous(breaks=NULL)+coord_cartesian(xlim = c(-1,1)*fullRange*IV$sd+IV$mu,ylim=c(0,1))
                }
      )
    }
    if (DV$type!="empty") {
      switch (DV$type, 
              "Categorical"={
                ncats1<-DV$ncats
                b1<-(1:ncats1)-1
                l1=DV$cases
                g<-g+scale_y_continuous(breaks=b1,labels=l1)+scale_x_continuous(breaks=NULL)+coord_cartesian(ylim = c(0,ncats1+1)-1,xlim=c(0,1))
              },
              "Ordinal"={
                ncats1<-DV$nlevs
                b1<-(1:ncats1)
                l1=b1
                g<-g+scale_y_continuous(breaks=b1,labels=l1)+scale_x_continuous(breaks=NULL)+coord_cartesian(ylim = c(0,ncats1+1)-1,xlim=c(0,1))
              },
              "Interval"={
                g<-g+scale_y_continuous()+scale_x_continuous(breaks=NULL)+coord_cartesian(ylim = c(-1,1)*fullRange*DV$sd+DV$mu,xlim=c(0,1))
              }
      )
    }
    if (IV$type=="empty" && DV$type=="empty") {
      g<-g+scale_x_continuous(breaks=NULL)+scale_y_continuous(breaks=NULL)+coord_cartesian(xlim = c(0,1),ylim=c(0,1))
    }
  } else {
    
  switch (hypothesisType,
          "Interval Interval"={
            g<-drawParParPopulation(IV,DV,rho,heteroscedasticity,alpha)+coord_cartesian(xlim = c(-1,1)*fullRange*IV$sd+IV$mu, ylim = c(-1,1)*fullRange*DV$sd+DV$mu)
          },
          "Ordinal Interval"={
            g<-drawParParPopulation(IV,DV,rho,heteroscedasticity,alpha)+coord_cartesian(xlim = c(-1,1)*fullRange*IV$sd+IV$mu, ylim = c(-1,1)*fullRange*DV$sd+DV$mu)
          },
          "Categorical Interval"={
            g<-drawCatParPopulation(IV,DV,rho,heteroscedasticity,alpha)+coord_cartesian(xlim = c(0,IV$ncats+1)-1, ylim = c(-1,1)*fullRange*DV$sd+DV$mu)
          },
          "Interval Ordinal"={
            g<-drawParOrdPopulation(IV,DV,rho,heteroscedasticity,alpha)+coord_cartesian(xlim = c(-1,1)*fullRange*IV$sd+IV$mu, ylim = c(0,DV$nlevs+1))
          },
          "Ordinal Ordinal"={
            g<-drawParOrdPopulation(IV,DV,rho,heteroscedasticity,alpha)+coord_cartesian(xlim = c(-1,1)*fullRange*IV$sd+IV$mu, ylim = c(0,DV$nlevs+1))
          },
          "Categorical Ordinal"={
            g<-drawCatOrdPopulation(IV,DV,rho,heteroscedasticity,alpha)+coord_cartesian(xlim = c(0,IV$ncats+1)-1, ylim = c(0,DV$nlevs+1))
          },
          "Interval Categorical"={
            g<-drawParCatPopulation(IV,DV,rho,heteroscedasticity,alpha)+coord_cartesian(xlim = c(-1,1)*fullRange*IV$sd+IV$mu, ylim = c(0,DV$ncats+1)-1)
          },
          "Ordinal Categorical"={
            g<-drawParCatPopulation(IV,DV,rho,heteroscedasticity,alpha)+coord_cartesian(xlim = c(-1,1)*fullRange*IV$sd+IV$mu, ylim = c(0,DV$ncats+1)-1)
          },
          "Categorical Categorical"={
            g<-drawCatCatPopulation(IV,DV,rho,heteroscedasticity,alpha)+coord_cartesian(xlim = c(0,IV$ncats+1)-1, ylim = c(0,DV$ncats+1)-1)
          }
  )
}
  g+plotTheme+theme(plot.margin=popplotMargins)+
    labs(x=IV$name,y=DV$name)
  
}
