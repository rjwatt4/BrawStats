inspectMainGraph<-function(inspect) {
  
  var<-inspect$var
  data<-inspect$data

  # start
  g<-ggplot()
  
  if (!is.null(data)) {
    if (var$type=="Categorical") {data<-as.numeric(data)}
    # data points
    
    switch(inspect$inspectOrder,
           "unsorted"={y<-1:inspect$n},
           "sorted"={y<-rank(data)},
           "piled"={y<-pile(data)}
           )
    
    pts<-data.frame(x=data,y=y)
    g<-g+geom_point(data=pts,aes(x=x,y=y),shape=21, colour="black", fill=plotcolours$sampleC,size=7)
    mn<-mean(data)
    
    # showing the residuals
    if (inspect$showResiduals) {
      # vertical line
      g<-g+geom_vline(xintercept=inspect$ResidVal, colour = "black", lwd=1.5)
      # horizontal lines      
      switch (var$type,
              "Categorical"={
              },
              "Ordinal"={
                for (i in 1:inspect$n) {
                  col<-sign(inspect$ResidVal-data[i])
                  col<-col/2+0.5
                  col<-rgb(col,col,col)
                  g<-g+geom_segment(data=data.frame(x=data[i],xend=inspect$ResidVal,y=y[i],yend=y[i]),aes(x=x,y=y,xend=xend,yend=yend), colour=col, lwd=1  )
                }
              },
              "Interval"={
                for (i in 1:inspect$n) {
                  col=(inspect$ResidVal-data[i])/var$sd/2
                  col=max(min(col/2+0.5,1),0)
                  col=rgb(col,col,col)
                  g<-g+geom_segment(data=data.frame(x=data[i],xend=inspect$ResidVal,y=y[i],yend=y[i]),aes(x=x,y=y,xend=xend,yend=yend), colour=col, lwd=1  )
                }
              }
      )
    }
    
    # show mean
    if (inspect$showMean) {
      # vertical line
      switch (var$type,
              "Categorical"={
                g<-g+geom_vline(xintercept=Mode(data), colour = "red", lwd=2)
                },
              "Ordinal"={
                g<-g+geom_vline(xintercept=median(data), colour = "red", lwd=2)
                },
              "Interval"={
                g<-g+geom_vline(xintercept=mean(data), colour = "red", lwd=2)
                }
      )
    }
    # show mean
    if (inspect$showSd) {
      # vertical lines
      switch (var$type,
              "Categorical"={
              },
              "Ordinal"={
                g<-g+geom_vline(xintercept=quantile(data,0.25), colour = "red", lwd=1)
                g<-g+geom_vline(xintercept=quantile(data,0.75), colour = "red", lwd=1)
              },
              "Interval"={
                g<-g+geom_vline(xintercept=mean(data)+std(data,1), colour = "red", lwd=1)
                g<-g+geom_vline(xintercept=mean(data)-std(data,1), colour = "red", lwd=1)
              }
      )
    }
  }
  
  # wind up
  g<-g+scale_y_continuous(breaks=NULL)
  switch (var$type,
          "Categorical"+{
            g<-g+scale_x_continuous(breaks=1:var$ncats,labels=var$cases)
            g<-g+coord_cartesian(xlim=c(1,var$ncats)+c(-1,1)*(var$ncats-1)/10,ylim = c(1,inspect$n)+c(-1,1)*(inspect$n-1)/10)
          },
          "Ordinal"={
            g<-g+scale_x_continuous(breaks=1:var$nlevs)
            g<-g+coord_cartesian(xlim=c(1,var$nlevs)+c(-1,1)*(var$nlevs-1)/10,ylim = c(1,inspect$n)+c(-1,1)*(inspect$n-1)/10)
          },
          "Interval"={
            g<-g+scale_x_continuous()
            g<-g+coord_cartesian(xlim=c(-1,1)*3*var$sd+var$mu,ylim = c(1,inspect$n)+c(-1,1)*(inspect$n-1)/10)
          }
          )
  g+labs(x=var$name,y=NULL)+plotTheme
}


inspectPenaltyGraph<-function(inspect) {
  
  var<-inspect$var
  data<-inspect$data
  
  if (!is.null(data) && inspect$showResiduals) {
      # vertical line
    switch(var$type,
           "Categorical"={
             val<-mean(as.numeric(data)!=round(inspect$ResidVal))
             },
           "Ordinal"={
             if (var$discrete) {mval<-round(inspect$ResidVal)} else {mval<-inspect$ResidVal}
             val<-mean(sign(data-mval))
           },
           "Interval"={
             val<-mean(data-inspect$ResidVal)/2/var$sd
             }
           )
    
    g<-ggplot()
    if (val<0) {
      g<-g+geom_polygon(data=data.frame(x=c(-0.5,-0.5,0.5,0.5),y=c(0,val,val,0)),aes(x=x,y=y),colour="white",fill="white")
      g<-g+geom_hline(yintercept=0,colour="white",lwd=1)
    } else {
      g<-g+geom_polygon(data=data.frame(x=c(-0.5,-0.5,0.5,0.5),y=c(0,val,val,0)),aes(x=x,y=y),colour="black",fill="black")
      g<-g+geom_hline(yintercept=0,colour="black",lwd=1)
    }
  } else {
    return(ggplot()+plotBlankTheme)
  }
  
  # wind up
  g<-g+scale_x_continuous(breaks=NULL)
  g<-g+scale_y_continuous(breaks=0)
  g<-g+coord_cartesian(xlim=c(-1,1),ylim = c(-1,1))
  g+labs(x=NULL,y="Residuals")+plotTheme
  
  
}


inspectPenalty2Graph<-function(inspect) {
  return(ggplot()+plotBlankTheme)
  
  var<-inspect$var
  data<-inspect$data
  
  if (!is.null(data) && inspect$showResiduals) {
    # vertical line
    switch(var$type,
           "Categorical"={
             val<-mean(as.numeric(data)!=round(inspect$ResidVal))
             val<-(sum(data!=round(inspect$ResidVal))+(length(data)-sum(data==round(inspect$ResidVal))))/length(data)
           },
           "Ordinal"={
             # sum(outside iqr)-sum(inside iqr)
             if (var$discrete) {mval<-round(inspect$ResidVal)} else {mval<-inspect$ResidVal}
             q1<-median(data[data<mval])
              q2<-median(data[data>mval])
             r1=sum(data<q1)+sum(data>q2)
             r2<-sum(data>q1 & data<q2)
             val<-r1-r2
           },
           "Interval"={
             # squared residual
             val<-mean((data-inspect$ResidVal)^2)/2/var$sd/var$sd
           }
    )
    
    g<-ggplot()
    if (val<0) {
      g<-g+geom_polygon(data=data.frame(x=c(-0.5,-0.5,0.5,0.5),y=c(0,val,val,0)),aes(x=x,y=y),colour="white",fill="white")
      g<-g+geom_hline(yintercept=0,colour="white",lwd=1)
    } else {
      g<-g+geom_polygon(data=data.frame(x=c(-0.5,-0.5,0.5,0.5),y=c(0,val,val,0)),aes(x=x,y=y),colour="black",fill="black")
      g<-g+geom_hline(yintercept=0,colour="black",lwd=1)
    }
  } else {
    return(ggplot()+plotBlankTheme)
  }
  
  # wind up
  g<-g+scale_x_continuous(breaks=NULL)
  g<-g+scale_y_continuous(breaks=0)
  g<-g+coord_cartesian(xlim=c(-1,1),ylim = c(0,1))
  g+labs(x=NULL,y="Residuals^2")+plotTheme
  
  
}

pile<-function(data) {
  x<-c()
  y<-c()
  xspacing<-(max(data)-min(data))/20
  yspacing<-1.5
  for (i in 1:length(data)){
    found<-sum((x-data[i])^2<xspacing^2)
    y<-c(y,(found-1)*yspacing)
    x<-c(x,data[i])
  }
  return(y)
}