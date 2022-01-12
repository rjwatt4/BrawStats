doLegendPoints=FALSE

drawPoints<-function(g,IV,DV,result,colindex=1,off=0){
  
  showRawData<-(allScatter=="all")
  if (colindex==1)
          {  col<- plotcolours$descriptionC
          xoff=0
          barwidth=1
          } else { 
          col <-plotDescriptionCols[[colindex-1]]
          xoff=-0.25+off*0.5
          barwidth=0.5
          }

  x<-result$ivplot
  y<-result$dvplot
  
  hypothesisType=paste(IV$type,DV$type,sep=" ")
  
  dotSize<-3.5
  if (length(x)>100) {
    dotSize<-3.5*sqrt(100/length(x))
  }
  switch (hypothesisType,
          "Interval Interval"={
            pts<-data.frame(x=x,y=y);
            if (colindex>=2) {
              g<-g+geom_point(data=pts,aes(x=x,y=y,fill=names(plotDescriptionCols)[colindex-1]),shape=21, colour = "black", alpha=0.95, size =dotSize)
            }
            else
              g<-g+geom_point(data=pts,aes(x=x,y=y),shape=21, size =dotSize, colour="black", fill=col)
          },
          
          "Categorical Interval"={
            pts<-data.frame(IV=x+xoff,DV=y);
            if (showRawData) {
              if (colindex>=2) 
                g<-g+geom_point(data=pts,aes(x=IV,y=DV,fill=names(plotDescriptionCols)[colindex-1]),shape=21, colour = "black", alpha=0.95, size =dotSize)
              else
                g<-g+geom_point(data=pts,aes(x=IV,y=DV),shape=21, colour = "black", fill=col, alpha=0.95, size =dotSize)
            }
          },
          
          "Interval Ordinal"={
            pts<-data.frame(IV=x,DV=y);
            if (colindex>=2)
              g<-g+geom_point(data=pts,aes(x=IV,y=DV,fill=names(plotDescriptionCols)[colindex-1]),shape=21, colour = "black", alpha=0.95, size =dotSize)
            else
              g<-g+geom_point(data=pts,aes(x=IV,y=DV),shape=21, size =dotSize, colour="black", fill=col)
          },
          
          "Categorical Ordinal"={
            pts<-data.frame(IV=x,DV=y);
            if (showRawData) {
              if (colindex>=2)
                g<-g+geom_point(data=pts,aes(x=IV,y=DV,fill=names(plotDescriptionCols)[colindex-1]),shape=21, colour = "black", alpha=0.95, size =dotSize)
              else
                g<-g+geom_point(data=pts,aes(x=IV,y=DV),shape=21, colour = "black", fill=col, alpha=0.95, size =dotSize)
            }
          },
          
          "Interval Categorical"={
            bin_breaks<-c(-Inf,seq(-1,1,length.out=10)*fullRange*sd(result$iv)+mean(result$iv),Inf)
            dens2<-hist(result$iv,breaks=bin_breaks,freq=TRUE,plot=FALSE,warn.unused = FALSE)
            bins=dens2$mids
            full_x<-c()
            full_y<-c()
            full_f<-c()
            full_c<-c()
            for (i2 in 1:DV$ncats){
              xv<-c()
              yv<-c()
              dens1<-hist(result$iv[result$dv==DV$cases[i2]],breaks=bin_breaks,freq=TRUE,plot=FALSE,warn.unused = FALSE)
              densities<-dens1$counts/dens2$counts
              for (i in 1:(length(dens1$counts)-1)){
                y<-dens1$counts[i]
                if (y>0){
                  xv<-c(xv,rep(bins[i],y)+runif(y,min=-0.08,max=0.08))
                  yv<-c(yv,runif(y,min=0.05,max=0.9)*densities[i])
                }
              }
              xoff<-(i2-1)/(DV$ncats-1)-(DV$ncats-1)/2
              full_x<-c(full_x,xv+xoff/4)
              full_y<-c(full_y,yv)
              full_f<-c(full_f,rep(i2,length(xv)))
              if (colindex==1) {
              full_c<-c(full_c,rep(CatCatcols[i2],length(xv)))
              }
            }
            pts<-data.frame(x=full_x,y=full_y,fill=full_f)
            if (showRawData) {
              if (colindex>=2) {
                # g<-g+geom_point(data=pts,aes(x=x,y=y,fill=names(plotDescriptionCols)[colindex-1]),shape=21, size =dotSize, alpha=0.95, colour="black")
                g<-g+geom_point(data=pts,aes(x=full_x,y=full_y),shape=21, size =dotSize, alpha=0.95, colour="black",fill="white")
                } else {
                  if (doLegendPoints) {
                    g<-g+geom_point(data=pts,aes(x=full_x,y=full_y,fill=factor(full_f)),shape=21, size =dotSize, alpha=0.95)
                  } else {
                    g<-g+geom_point(data=pts,aes(x=x,y=y),shape=21, size =dotSize, alpha=0.95, colour="black",fill=full_c)
                  }
                }
            }
          },
          
          "Categorical Categorical"={
            b<-(1:IV$ncats)-1
            xv<-as.numeric(result$iv)
            yv<-as.numeric(result$dv)
            
            pp<-matrix(NA,DV$ncats,IV$ncats)
            for (i1 in 1:IV$ncats) {
              for (i2 in 1:DV$ncats) {
                pp[i2,i1]<-sum(yv[xv==i1]==i2)/length(xv)
              }
            }
            
            for (i2 in 1:DV$ncats) {
              x<-b[xv[yv==i2]]+(i2-(DV$ncats+1)/2)/(DV$ncats+1)+runif(length(xv[yv==i2]),min=-0.1,max=0.1)
              y<-pp[i2,xv[yv==i2]]*runif(length(xv[yv==i2]),min=0.05,max=0.9)
            
            pts<-data.frame(x=x+xoff,y=y)
            if (showRawData) {
              if (colindex>=2)
                g<-g+geom_point(data=pts,aes(x=x,y=y,fill=names(plotDescriptionCols)[colindex-1]),shape=21, size =dotSize, alpha=0.95, colour="black")
              else
                if (doLegendPoints) {
                  g<-g+geom_point(data=pts,aes(x=x,y=y,fill=factor(i2)),shape=21, size =dotSize, alpha=0.95, colour="black")
                } else {
                  g<-g+geom_point(data=pts,aes(x=x,y=y),shape=21, size =dotSize, colour="black", fill=CatCatcols[i2], alpha=0.95)
                }
            }
            }
          }
  )
 g  
}

drawCatInterDescription<-function(IV,IV2,DV,effect,design,result,g=NULL){
  plotDescriptionCols <<- c()
  cols<-c()
  for (i in 1:IV2$ncats){
    off<-(i-1)/(IV2$ncats-1)
    col<- col2rgb(plotcolours$descriptionC1)*(1-off)+col2rgb(plotcolours$descriptionC2)*off
    cols<- c(cols,rgb(col[1]/255,col[2]/255,col[3]/255))
  }
  names(cols)<-IV2$cases
  plotDescriptionCols <<- cols
  
  Ivals<-IV$vals
  Dvals<-DV$vals
  rho<-result$rIV+seq(-1,1,length.out=IV2$ncats)*result$rIVIV2DV
  
  if (is.null(g)) {
    print("ggplot")
    g<-ggplot()
    }
  for (i in 1:IV2$ncats){
    use<-result$iv2==IV2$cases[i]
    
    result1<-result
    result1$iv<-result$iv[use]
    result1$dv<-result$dv[use]
    result1$ivplot<-result$ivplot[use]
    result1$dvplot<-result$dvplot[use]
    result1$rIV<-rho[i]
    
    result1$IVs$vals<-Ivals[use]
    result1$DVs$vals<-Dvals[use]
    g<-drawPrediction(result1$IVs,NULL,result1$DVs,result1,design,2+(i-1)/(IV2$ncats-1),g)
    g<-drawPoints(g,IV,DV,result1,i+1,(i-1)/(IV2$ncats-1))
  }
  
  g<-g+scale_fill_manual(name=IV2$name,values=plotDescriptionCols)
  g
}

drawParInterDescription<-function(IV,IV2,DV,effect,design,result,g=NULL){
  col<-c( plotcolours$descriptionC1, plotcolours$descriptionC2)
  names(col)<-c(paste(IV2$name,"<median",sep=""), paste(IV2$name,">median",sep=""))
  plotDescriptionCols <<- col
  
  Ivals<-IV$vals
  Dvals<-DV$vals
  rho<-result$rIV+seq(-1,1,length.out=2)*result$rIVIV2DV
  
  if (is.null(g)) {
    g<-ggplot()
  }
  for (i in 1:2){
    switch (i,
            use<-result$iv2<median(result$iv2),
            use<-result$iv2>=median(result$iv2)
    )
    result1<-result
    result1$iv<-result$iv[use]
    result1$dv<-result$dv[use]
    result1$ivplot<-result$ivplot[use]
    result1$dvplot<-result$dvplot[use]
    result1$rIV<-rho[i]
    
    result1$IVs$vals<-Ivals[use]
    result1$DVs$vals<-Dvals[use]
    g<-drawPrediction(result1$IVs,NULL,result1$DVs,result1,design,i+1,g)
    g<-drawPoints(g,result1$IVs,result1$DVs,result1,i+1,(i-1)/(2-1))
  }
  
  g<-g+scale_fill_manual(name=IV2$name,values=plotDescriptionCols)
  g
}

drawParDescription<-function(IV,IV2,DV,effect,design,result,g) {
  
  g<-drawPrediction(result$IVs,IV2,result$DVs,result,design,1,g)
  g<-drawPoints(g,IV,DV,result,1)
  g
}

drawCatDescription<-function(IV,IV2,DV,effect,design,result,g) {

  g<-drawPrediction(result$IVs,IV2,result$DVs,result,design,1,g)
  g<-drawPoints(g,IV,DV,result,1)
  
  if (!doLegendBars && doLegendPoints) {
    g<-g+scale_fill_manual(name=DV$name,values=CatCatcols,labels=DV$cases)
  }
  
  g
}

drawDescription<-function(IV,IV2,DV,effect,design,result) {

  g<-ggplot()
  if (is.null(IV2)){
    switch (DV$type,
            "Interval"=g<-drawParDescription(IV,IV2,DV,effect,design,result,g),
            "Ordinal"=g<-drawParDescription(IV,IV2,DV,effect,design,result,g),
            "Categorical"=g<-drawCatDescription(IV,IV2,DV,effect,design,result,g)
    )
  } else{
    switch (IV2$type,
            "Interval"=g<-drawParInterDescription(IV,IV2,DV,effect,design,result,g),
            "Categorical"=g<-drawCatInterDescription(IV,IV2,DV,effect,design,result,g)
    )
  }
  g
}
