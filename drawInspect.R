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
           "sorted"={y<-rank(data,ties.method="first")},
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

    g<-showMean(g,inspect)
    g<-showSD(g,inspect)    
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
    if (inspect$inspectHistory[length(inspect$inspectHistory)]!=inspect$ResidVal)
    {inspect$inspectHistory<-c(inspect$inspectHistory,inspect$ResidVal)}
    
    g<-ggplot()
    y=c()
    x<-sort(inspect$inspectHistory)
    
    for (i in 1:length(x)) {
      if (inspect$whichResiduals=="1") {
        switch(var$type,
               "Categorical"={
                 val<-mean(as.numeric(data)!=round(x[i]))
               },
               "Ordinal"={
                 if (var$discrete) {mval<-round(x[i])} else {mval<-x[i]}
                 val<-mean(sign(data-mval))
               },
               "Interval"={
                 val<-mean(data-x[i])/2/var$sd
               }
        )
      } else {
        switch(var$type,
               "Categorical"={
                 val<-(sum(data!=round(x[i]))+(length(data)-sum(data==round(x[i]))))/length(data)/var$ncats
               },
               "Ordinal"={
                 # sum(outside iqr)-sum(inside iqr)
                 if (var$discrete) {mval<-round(x[i])} else {mval<-x[i]}
                 q1<-median(data[data<mval])
                 q2<-median(data[data>mval])
                 r1=sum(data<q1)+sum(data>q2)
                 r2<-sum(data>q1 & data<q2)
                 val<-r1-r2
               },
               "Interval"={
                 # squared residual
                 val<-mean((data-x[i])^2)/2/2/var$sd/var$sd
               }
        )
      }
    if (val<0) {
      g<-g+geom_polygon(data=data.frame(x=c(-1,-1,1,1)/50+x[i],y=c(0,val,val,0)),aes(x=x,y=y),colour="white",fill="white")
      g<-g+geom_hline(yintercept=0,colour="white",lwd=1)
    } else {
      g<-g+geom_polygon(data=data.frame(x=c(-1,-1,1,1)/50+x[i],y=c(0,val,val,0)),aes(x=x,y=y),colour="black",fill="black")
      g<-g+geom_hline(yintercept=0,colour="black",lwd=1)
    }
      y<-c(y,val)
      if (i>1) {
        pts<-data.frame(x=x[i-1],y=y[i-1],xend=x[i],yend=y[i])
        g<-g+geom_segment(data=pts,aes(x=x,y=y,xend=xend,yend=yend),colour="yellow",lwd=0.5)
      }
    }
    g<-showMean(g,inspect)
    g<-showSD(g,inspect)    
    
  } else {
    return(ggplot()+plotBlankTheme)
  }
  
  # wind up
  g<-g+scale_y_continuous(breaks=0)
  switch (var$type,
          "Categorical"+{
            g<-g+scale_x_continuous(breaks=1:var$ncats,labels=var$cases)
            g<-g+coord_cartesian(xlim=c(1,var$ncats)+c(-1,1)*(var$ncats-1)/10,ylim=c(-1,1))
          },
          "Ordinal"={
            g<-g+scale_x_continuous(breaks=1:var$nlevs)
            g<-g+coord_cartesian(xlim=c(1,var$nlevs)+c(-1,1)*(var$nlevs-1)/10,ylim = c(-1,1))
          },
          "Interval"={
            g<-g+scale_x_continuous()
            g<-g+coord_cartesian(xlim=c(-1,1)*3*var$sd+var$mu,ylim = c(-1,1))
          }
  )
  # g+labs(x=var$name,y=paste("Residuals^(",inspect$whichResiduals,")",sep=""))+plotTheme
  switch (inspect$whichResidual,
          "1"={g<-g+labs(x=var$name,y=bquote(Residuals^1))},
          "2"={g<-g+labs(x=var$name,y=bquote(Residuals^2))}
          )
  g+plotTheme

  
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


showMean<-function(g,inspect) {
  var<-inspect$var
  data<-inspect$data
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
  return(g)
}

showSD<-function(g,inspect) {
  var<-inspect$var
  data<-inspect$data
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
  return(g)
  
}