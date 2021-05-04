
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


erf<-function(z){
  2*pnorm(sqrt(2)*z) - 1
}

drawCatPositions<-function(ncats){
  pbreaks<-seq(0,1,1/(ncats))
  ebreaks<-exp(-qnorm(pbreaks)^2/2)
  -1/sqrt(2*pi)*diff(ebreaks)/diff(pbreaks)
}


drawParParPopulation<-function(IV,DV,rho,design,alpha){
  theta=seq(0,2*pi,2*pi/101)
  if (is.null(design)) {Heteroscedasticity<-0} 
  else {Heteroscedasticity=design$sHeteroscedasticity}
  d<-acos(rho)
  x=cos(theta+d/2)
  y=cos(theta-d/2)
  y<-(y-x*rho)*(1+x/3*Heteroscedasticity)+x*rho
  pts=data.frame(x=x*2*IV$sd+IV$mu,y=y*2*DV$sd+DV$mu)
  g<-ggplot(pts,aes(x=x, y=y))+
    geom_polygon(fill = plotcolours$sampleC,alpha=alpha)
  
}

drawCatParPopulation<-function(IV,DV,rho,design,alpha){
  ncats<-IV$ncats
  l=IV$cases
  b<-drawCatPositions(ncats)
  b<-seq(b[1],b[ncats],length.out=ncats)

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
  hsy<-1+seq(-1,1,length.out=ncats)*design$sHeteroscedasticity
  for (id in 1:ncats) {
    use<-mu_order[id]
        x<-mv2dens(y,abs(rho),ebreaks[use],ebreaks[use+1])
    x<-x/max(x,na.rm=TRUE)*(b[2]-b[1])/2.2
    xshape<-c(-x,rev(x))
    pts<-data.frame(x=xshape+b[id],y=yshape*hsy[id]*sdv[id]+muv[id])
    g<-g+
      geom_polygon(data=pts,aes(x=x,y=y),fill = plotcolours$sampleC,alpha=alpha)
  }
  g+scale_x_continuous(breaks=b,labels=l)

}

drawParCatPopulation<-function(IV,DV,rho,design,alpha){
  ncats<-DV$ncats
  l=paste("D",1:ncats,sep="")
  b<-drawCatPositions(ncats)

  pbreaks<-seq(0,1,1/(ncats))
  ebreaks<-qnorm(pbreaks)
  
  x<-seq(-1,1,0.01)*fullRange
  pts=data.frame(x=x,y=x*0)
  g<-ggplot(pts,aes(x=x,y=y))
  xshape<-c(x,rev(x))
  for (id in 1:ncats) {
    y<-mv2dens(x,rho,ebreaks[id],ebreaks[id+1])
    y<-y/max(y)/2
    
    yshape<-c(-y,rev(y))
    pts<-data.frame(x=xshape*IV$sd+IV$mu,y=yshape+b[id])
    g<-g+
      geom_polygon(data=pts,aes(x=x,y=y),fill = plotcolours$sampleC,alpha=alpha)
  }
  g+scale_y_continuous(breaks=b,labels=l)

}

drawCatCatPopulation<-function(IV,DV,rho,design,alpha){
  ncats1<-IV$ncats
  b1<-drawCatPositions(ncats1)
  l1=IV$cases
  
  ncats2<-DV$ncats
  b2<-drawCatPositions(ncats2)
  l2=paste("D",1:ncats2,sep="")
  
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
      pts<-data.frame(x=x*s[iy,ix]+b1[ix], y=y*s[iy,ix]+b2[iy])
      g<-g+
        geom_polygon(data=pts,aes(x=x,y=y),fill = plotcolours$sampleC,colour="black",alpha=alpha)
    }
  }
  g+scale_x_continuous(breaks=b1,labels=l1)+scale_y_continuous(breaks=b2,labels=l2)
}

drawPopulation<-function(IV,DV,effect,design,alpha=1){
  rho<-effect$rIV
  if (is.na(rho)) {rho<-0}
  
  hypothesisType=paste(IV$type,DV$type,sep=" ")
  
  switch (hypothesisType,
          "Interval Interval"={
            g<-drawParParPopulation(IV,DV,rho,design,alpha)
          },
          "Categorical Interval"={
            g<-drawCatParPopulation(IV,DV,rho,design,alpha)
          },
          "Interval Categorical"={
            g<-drawParCatPopulation(IV,DV,rho,design,alpha)
          },
          "Categorical Categorical"={
            g<-drawCatCatPopulation(IV,DV,rho,design,alpha)
          }
  )
  g+plotTheme+theme(plot.margin=popplotMargins)+
    labs(x=IV$name,y=DV$name)+coord_cartesian(xlim = c(-1,1)*fullRange*IV$sd+IV$mu, ylim = c(-1,1)*fullRange*DV$sd+DV$mu)
  
}