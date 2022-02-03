min_p=0.0001
min_nw=10
max_nw=10000

se_colour="#BBBBBB"
se_size=0.75
se_arrow=0.3
CI=0.95

get_target<-function(nsvals,vals){
  target1<-max(nsvals,na.rm=TRUE)
  if (any(vals>target1)){
    target2<-min(vals[vals>target1],na.rm=TRUE)
    target<-(target1+target2)/2
  } else target<-target1+0.001
}

expected_hist<-function(vals,nsvals,valType){

  nv=max(length(nsvals),length(vals))
  nb<-round(sqrt(nv)*0.75)
  switch (valType,
          "r"=  { # ns is small
            target<-get_target(abs(nsvals),abs(vals))

            low_p<-min(vals,na.rm=TRUE)-0.001
            high_p<-max(vals,na.rm=TRUE)+0.001
            if (all(is.na(nsvals)) || all(!is.na(nsvals))){
              bins<-seq(low_p,high_p,length.out=nb)
              } else {
                target_low<-max(-target,low_p)
                target_high<-min(target,high_p)
                targetRange<-target_high-target_low
                nbs<-ceiling(nb*targetRange/(high_p-low_p))
                binStep<-targetRange/nbs
                bins<-seq(target_low,target_high,binStep)
                if (target<high_p) {
                  bins<-c(bins,seq(target+binStep,high_p+binStep,binStep))
                }
                if (-target>low_p) {
                  bins<-c(rev(seq(-target-binStep,low_p-binStep,-binStep)),bins)
                }
              }
          },
          
          "p"=  { # ns is large
            target<-alpha
            if (pPlotScale=="log10"){
              vals[vals<0.000001]<-0.000001
              vals<-log10(vals)
              nsvals<-log10(nsvals)
              target<-log10(target)
              low_p<-max(log10(min_p),min(vals,na.rm=TRUE))
              high_p<-max(vals,na.rm=TRUE)
            } else {
              low_p<-max(min_p,min(vals,na.rm=TRUE))
              high_p<-max(vals,na.rm=TRUE)
            }
            if (all(is.na(nsvals)) || all(!is.na(nsvals))){
              bins<-seq(low_p,high_p,length.out=nb)
            } else {
              nbs<-ceiling(nb*(high_p-target)/(high_p-low_p))
              binStep<-(high_p-target)/nbs
              bins<-rev(seq(high_p,low_p-binStep,-binStep))
            }
          },
          "w"=  { # ns is small
            target<-get_target(abs(nsvals),abs(vals))

            if (wPlotScale=="log10"){
              vals[val<0.000001]<-0.000001
              vals<-log10(vals)
              nsvals<-log10(nsvals)
              target<-log10(target)
              low_p<-max(log10(min_p),min(vals,na.rm=TRUE))
              high_p<-max(vals,na.rm=TRUE)
            } else {
              low_p<-max(min_p,min(vals,na.rm=TRUE))
              high_p<-max(vals,na.rm=TRUE)
            }
            if (all(is.na(nsvals)) || all(!is.na(nsvals))){
              bins<-seq(low_p,high_p,length.out=nb)
            } else {
              nbs<-ceiling(nb*(high_p-target)/(high_p-low_p))
              binStep<-(high_p-target)/nbs
              bins<-rev(seq(high_p,low_p-binStep,-binStep))
            }
          },
          "nw"= { # ns is large
            target1<-min(nsvals,na.rm=TRUE)
            if (any(vals<target1)){
              target2<-max(vals[vals<target1],na.rm=TRUE)
              if (target2==-Inf) target2=target1-0.5
              target<-(target1+target2)/2
            } else {target<-target1-0.5}
            
            low_p<-min(vals,na.rm=TRUE)
            high_p<-min(log10(10000),max(vals,na.rm=TRUE))
            
            if (all(is.na(nsvals)) || all(!is.na(nsvals))){
              bins<-seq(low_p,high_p,length.out=nb)
            } else {
              nbs<-ceiling(nb*(target-low_p)/(high_p-low_p))
              binStep<-(target-low_p)/nbs
              bins<-seq(low_p,high_p+binStep,binStep)
            }
          }
  )
  useBins<-c(-Inf,bins,Inf)
  dens<-hist(vals,breaks=useBins,plot=FALSE,warn.unused = FALSE,right=TRUE)
  dens<-dens$counts
  dens<-dens[2:(length(dens)-1)]

  nsdens<-hist(nsvals,breaks=useBins,plot=FALSE,warn.unused = FALSE,right=TRUE)
  nsdens<-nsdens$counts
  nsdens<-nsdens[2:(length(nsdens)-1)]

  nsdens<-nsdens/max(dens)/2
  dens<-dens/max(dens)/2
  x<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
  y1<-c(0,as.vector(matrix(c(dens,dens),2,byrow=TRUE)),0)
  y2<-c(0,as.vector(matrix(c(nsdens,nsdens),2,byrow=TRUE)),0)
  
  data.frame(y1=c(-y1,rev(y1)), y2=c(-y2,rev(y2)), x=c(x,rev(x)))
}

expected_plot<-function(g,pts,result,IV,DV,expType){
  xoff<-pts$x[1]
  if (useSignificanceCols){
    c1=plotcolours$infer_sigC
    c2=plotcolours$infer_nsigC
  } else {
    c1=plotcolours$descriptionC
    c2=plotcolours$descriptionC
  }
  switch (expType,
          "r"={
          },
          "p"={
            c1=plotcolours$infer_sigC
            c2=plotcolours$infer_nsigC
          },
          "w"={
            c1=plotcolours$infer_sigC
            c2=plotcolours$infer_nsigC
          },
          "nw"={
            c1=plotcolours$infer_sigC
            c2=plotcolours$infer_nsigC
          },
          "e1"={
            expType<-"p"
            c1=plotcolours$infer_err
            c2=plotcolours$infer_nsigC
          },
          "e2"={
            expType<-"p"
            c1=plotcolours$infer_sigC
            c2=plotcolours$infer_err
          },
          "ci1"={},
          "ci2"={}
  )
          
  pts1<-expected_hist(pts$y1,pts$y2,expType)

  g<-g+
    geom_polygon(data=pts1,aes(y=x,x=y1+xoff),colour=NA, fill = c1)+
    geom_polygon(data=pts1,aes(y=x,x=y2+xoff),colour=NA, fill = c2)+
    theme(legend.position = "none")+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  g
}

r_plot<-function(result,IV,IV2=NULL,DV,r=0,n=0){
  rActual<-r
  rActual[is.na(r)]<-0

    ylim<-c(-1, 1)
    ysc=1

  if (all(is.na(result$rIVIV2DV)) & is.null(IV2)){
    xoff=0
    rs<-result$rIV
    ps<-result$pIV
  } else {
    if (is.na(result$rIVIV2DV[1])){xoff=c(0,2)}else{xoff=c(0,2,4)}
        switch (result$showType,
            "direct"={
              rs<-result$r$direct
              ps<-result$p$direct
            },
            "unique"={
              rs<-result$r$unique
              ps<-result$p$unique
            },
            "total"={
              rs<-result$r$total
              ps<-result$p$total
            },
            "all"={
              rs<-c()
              ps<-c()
              ysc=1/3
              xoff=c(0,0,0,2,2,2,4,4,4)
              for (jk in 1:ncol(result$r$direct)) {
                rs<-cbind(rs,result$r$direct[,jk],result$r$unique[,jk],result$r$total[,jk])
                ps<-cbind(ps,result$p$direct[,jk],result$p$unique[,jk],result$p$total[,jk])
              }
            },
            "coefficients"={
              rs<-result$r$coefficients
              ps<-result$p$direct
            }
    )
  }

  for (i in 1:length(xoff)){
    single<-TRUE
    if (is.matrix(rs)) {
      if (nrow(rs)>points_threshold) {single<-FALSE}
    }
    if (single) {
      if (is.matrix(rs) && nrow(rs)>1){
        rvals<-rs[,i]
        pvals<-ps[,i]
        xr<-runif(nrow(rs),min=-1,max=1)*length(xoff)/2
      }
      else {
        rvals<-rs[i]
        pvals<-ps[i]
        xr=0
      }
      if (result$showType=="all") {
      pts=data.frame(x=xoff[i]+xr,y=(rvals+1)*ysc*0.9+rem(i-1,3)*ysc*2-1)
      } else {
        pts=data.frame(x=xoff[i]+xr,y=rvals)
      }
      if (i==1){g<-ggplot(pts,aes(x=x, y=y))}     
      
      if (length(rvals)==1){
        z_se<-1/sqrt(result$nval-3)
        z_ci<-atanh(rvals)+z_se*c(-1,1)*qnorm(1-(1-CI)/2)
        r_ci<-tanh(z_ci)
        pts1se<-data.frame(x=c(0,0)+xoff[i],y=r_ci)
        g<-g+geom_line(data=pts1se,aes(x=x,y=y),arrow=arrow(length=unit(se_arrow,"cm"),ends="both"),colour=se_colour,size=se_size)
      }

      dotSize=min(10,20/sqrt(length(rvals)))
      if (useSignificanceCols){
        c1=plotcolours$infer_sigC
        c2=plotcolours$infer_nsigC
      } else {
        c1=plotcolours$descriptionC
        c2=plotcolours$descriptionC
      }
      use<-(pvals>=alpha)
      pts1=pts[use,]
      g<-g+geom_point(data=pts1,aes(x=x, y=y),shape=21, colour = "black", fill = c2, size = dotSize*sqrt(ysc))
      pts2=pts[!use,]
      g<-g+geom_point(data=pts2,aes(x=x, y=y),shape=21, colour = "black", fill = c1, size = dotSize*sqrt(ysc))
    }
    else
    {
      rvals<-rs[,i]
      pvals<-ps[,i]
      rvals_sig<-rvals
      rvals_sig[pvals>=alpha]<-NA
      rvals_nsig<-rvals
      rvals_nsig[pvals<alpha]=NA
      if (result$showType=="all") {
        y1<-((rvals+1)*ysc*0.9+rem(i-1,3)*ysc*2-1)
      y2<-((rvals_nsig+1)*ysc*0.9+rem(i-1,3)*ysc*2-1)
      pts<-data.frame(x=rvals*0+xoff[i],y1=y1,y2=y2)
      } else {
        pts<-data.frame(x=rvals*0+xoff[i],y1=rvals,y2=rvals_nsig)
      }
        
      if (i==1){g<-ggplot(pts,aes(x=x, y=y))}      
      g<-expected_plot(g,pts,result,IV,DV,"r")
      lpts<-data.frame(x = xoff[i]-0.95, y = ylim[1],label=paste("actual =",format(rActual[i],digits=graph_precision)))
      g<-g+
        geom_label(data=lpts,aes(x = x, y = y, label = label), hjust=0, vjust=0, fill = "white",size=3)
    }
    if (length(xoff)>1)
      if (rem(i,3)==0)
      switch (i/3,
              {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Main Effect 1",color="white",size=3)},
              {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Main Effect 2",color="white",size=3)},
              {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Interaction",color="white",size=3)}
      )
  }
  
    if (result$showType=="all") {
      for (i in 1:3) {
        g<-g+geom_hline(yintercept=(-1+1)*ysc*0.9+(i-1)*ysc*2-1, color="black", size=1)
        g<-g+geom_hline(yintercept=(0.0+1)*ysc*0.9+(i-1)*ysc*2-1, linetype="dotted", color="black", size=0.5)
        g<-g+geom_hline(yintercept=(1+1)*ysc*0.9+(i-1)*ysc*2-1, color="black", size=1)
      }
      g<-g+coord_cartesian(xlim = c(min(xoff),max(xoff))+c(-1,1), ylim = ylim+c(0,diff(ylim)/16))+
        scale_y_continuous(breaks=(c(-1,0,1,-1,0,1,-1,0,1)+1)*ysc*0.9+(c(1,1,1,2,2,2,3,3,3)-1)*ysc*2-1,labels=c(-1,0,1,-1,0,1,-1,0,1))
    } else {
      g<-g+geom_hline(yintercept=0.0, linetype="dotted", color="black", size=0.5)+
        coord_cartesian(xlim = c(min(xoff),max(xoff))+c(-1,1), ylim = ylim+c(0,diff(ylim)/16))
    }
    g<-g+theme(legend.position = "none")+plotTheme
    g<-g+ylab("r")
  g+theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
}

p_plot<-function(result,IV,IV2=NULL,DV,r=0,n=0,ptype="p"){

  switch (ptype,
         "p"={
            pt_sigcol<-plotcolours$infer_sigC
            pt_nsigcol<-plotcolours$infer_nsigC
          },
          "e1"={
            pt_sigcol<-plotcolours$infer_err
            pt_nsigcol<-plotcolours$infer_nsigC
          },
          "e2"={
            pt_sigcol<-plotcolours$infer_sigC
            pt_nsigcol<-plotcolours$infer_err
          }
          )
  
    if (pPlotScale=="log10"){
    ylim<-c(log10(min_p), 0)
  } else{
    ylim<-c(0,1)
  }
  if (is.null(IV2) & all(is.na(result$rIVIV2DV))){
    xoff=0
    rs<-result$rIV
    ps<-result$pIV
  } else {
    if (is.na(result$rIVIV2DV[1])){xoff=c(0,2)}else{xoff=c(0,2,4)}
    switch (result$showType,
            "direct"={
              rs<-result$r$direct
              ps<-result$p$direct
            },
            "unique"={
              rs<-result$r$unique
              ps<-result$p$unique
            },
            "total"={
              rs<-result$r$total
              ps<-result$p$total
            },
            "all"={
              rs<-result$r$direct
              ps<-result$p$direct
            },
            "coefficients"={
              rs<-result$r$coefficients
              ps<-result$p$direct
            },
    )
  }

  for (i in 1:length(xoff)){
    single<-TRUE
    if (is.matrix(rs)) {
      if (nrow(rs)>points_threshold) {single<-FALSE}
    }
    if (single) {
      if (is.matrix(rs) && nrow(rs)>1){
        rvals<-rs[,i]
        pvals<-ps[,i]
        xr<-runif(nrow(rs),min=-1,max=1)*length(xoff)/2
      }
      else {
        rvals<-rs[i]
        pvals<-ps[i]
        xr=0
      }

      pvals[pvals<min_p]<-min_p
      if (pPlotScale=="log10") {
        pts=data.frame(x=xoff[i]+xr,y=log10(pvals))
      } else {
        pts=data.frame(x=xoff[i]+xr,y=pvals)
      }

      if (i==1){g<-ggplot(pts,aes(x=x, y=y))}      
      
      if (length(rvals)==1){
        z_se<-1/sqrt(result$nval-3)
        z_ci<-atanh(rvals)+z_se*c(-1,1)*qnorm(1-(1-CI)/2)
        r_ci<-tanh(z_ci)
        p_ci<-r2p(abs(r_ci),result$nval)
        if (r_ci[1]<0 && r_ci[2]>0) {
          p_ci[2]<-min(p_ci)
          p_ci[1]<-1
        }
        if (pPlotScale=="log10") p_ci<-log10(p_ci)
        pts1se<-data.frame(x=c(0,0)+xoff[i],y=p_ci)
        g<-g+geom_line(data=pts1se,aes(x=x,y=y),arrow=arrow(length=unit(se_arrow,"cm"),ends="both"),colour=se_colour,size=se_size)
      } 
      
      dotSize=min(10,20/sqrt(length(rvals)))
      pt_col<-pt_nsigcol
      use<-(pvals>=alpha)
      pts1=pts[use,]
      g<-g+geom_point(data=pts1,aes(x=x, y=y),shape=21, colour = "black", fill = pt_col, size = dotSize)
      pt_col<-pt_sigcol
      pts2=pts[!use,]
      g<-g+geom_point(data=pts2,aes(x=x, y=y),shape=21, colour = "black", fill = pt_col, size = dotSize)
    }
    else {
      rvals<-rs[,i]
      pvals<-ps[,i]
      pvals_sig<-pvals
      pvals_sig[pvals>=alpha]<-NA
      pvals_nsig<-pvals
      pvals_nsig[pvals<alpha]<-NA
      # if (pPlotScale=="log10") {
      #   pts<-data.frame(x=pvals*0+xoff[i],y1=log10(pvals),y2=log10(pvals_nsig))
      # } else {
        pts<-data.frame(x=pvals*0+xoff[i],y1=pvals,y2=pvals_nsig)
      # }
      
      if (i==1){g<-ggplot(pts,aes(x=x, y=y))}      
      g <- expected_plot(g,pts,result,IV,DV,ptype)
      
      lpts<-data.frame(x = xoff[i]-0.95, y = ylim[1], label = paste("p(sig) =",format(mean(pvals<alpha),digits=graph_precision)))
      g<-g+geom_label(data=lpts,aes(x = x, y = y, label=label), hjust=0, vjust=0, fill = "white",size=3)
    }
    if (length(xoff)>1)
      switch (i,
              {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Main Effect 1",color="white",size=3)},
              {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Main Effect 2",color="white",size=3)},
              {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Interaction",color="white",size=3)}
              )
  }
  g<-g+plotTheme+
    coord_cartesian(xlim = c(min(xoff),max(xoff))+c(-1,1), ylim = ylim+c(0,diff(ylim)/16))
    
  if (pPlotScale=="log10") {
    g<-g+geom_hline(yintercept=log10(1), linetype="dotted", color="#FF4422", size=0.5)+
      geom_hline(yintercept=log10(0.005), linetype="dotted", color="#44FF22", size=0.5)+
      geom_hline(yintercept=log10(0.01), linetype="dotted", color="#44FF22", size=0.5)+
      geom_hline(yintercept=log10(alpha), linetype="dotted", color="#44FF22", size=0.5)+
      scale_y_continuous(breaks=c(-4,-3,-2,-1,0),labels=c(0.0001,0.001,0.01,0.1,1))+ 
      ylab(bquote(log[10](p(H[0]))))
  } else
  {
    g<-g+geom_hline(yintercept=log10(alpha), linetype="dotted", color="#44FF22", size=0.5)+
      scale_y_continuous(breaks=seq(0,1,0.1),labels=seq(0,1,0.1))+ 
      ylab(bquote(p(H[0])))
  }
  g+theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
}


w_plot<-function(result,IV,IV2=NULL,DV,r=0,n=0){
  wActual<-rn2w(r,n)
  if (wPlotScale=="log10"){
    ylim<-c(log10(min_p), 0)
  } else{
    ylim<-c(0,1)
  }
  if (is.null(IV2) & all(is.na(result$rIVIV2DV))){
    xoff=0
    rs<-result$rIV
    ps<-result$pIV
  } else {
    if (is.na(result$rIVIV2DV[1])){xoff=c(0,2)}else{xoff=c(0,2,4)}
    switch (result$showType,
            "direct"={
              rs<-result$r$direct
              ps<-result$p$direct
            },
            "unique"={
              rs<-result$r$unique
              ps<-result$p$unique
            },
            "total"={
              rs<-result$r$total
              ps<-result$p$total
            },
            "coefficients"={
              rs<-result$r$coefficients
              ps<-result$p$direct
            },
    )
  }
  
  for (i in 1:length(xoff)){
    single<-TRUE
    if (is.matrix(rs)) {
      if (nrow(rs)>points_threshold) {single<-FALSE}
    }
    if (single) {
      if (is.matrix(rs) && nrow(rs)>1){
        rvals<-rs[,i]
        pvals<-ps[,i]
        xr<-runif(nrow(rs),min=-0.2,max=0.2)
      }
      else {
        rvals<-rs[i]
        pvals<-ps[i]
        xr=0
      }
      wvals<-rn2w(rvals,result$nval)
      
      if (wPlotScale=="log10") {
        pts=data.frame(x=xoff[i]+xr,y=log10(wvals))
      } else {
        pts=data.frame(x=xoff[i]+xr,y=wvals)
      }
      
      if (i==1){g<-ggplot(pts,aes(x=x, y=y))}      
      
      if (length(rvals)==1){
        z_se<-1/sqrt(result$nval-3)
        z_ci<-atanh(rvals)+z_se*c(-1,1)*qnorm(1-(1-CI)/2)
        r_ci<-tanh(z_ci)
        w_ci<-rn2w(abs(r_ci),result$nval)
        if (r_ci[1]<0 && r_ci[2]>0) {
          w_ci[2]<-max(w_ci)
          w_ci[1]<-alpha
        }
        if (wPlotScale=="log10") w_ci<-log10(w_ci)
        pts1se<-data.frame(x=c(0,0)+xoff[i],y=w_ci)
        g<-g+geom_line(data=pts1se,aes(x=x,y=y),arrow=arrow(length=unit(se_arrow,"cm"),ends="both"),colour=se_colour,size=se_size)
      } 
      
      dotSize=min(10,20/sqrt(length(rvals)))
      pt_col<-plotcolours$infer_nsigC
      use<-(pvals>=alpha)
      pts1=pts[use,]
      g<-g+geom_point(data=pts1,aes(x=x, y=y),shape=21, colour = "black", fill = pt_col, size = dotSize)
      pt_col<-plotcolours$infer_sigC
      pts2=pts[!use,]
      g<-g+geom_point(data=pts2,aes(x=x, y=y),shape=21, colour = "black", fill = pt_col, size = dotSize)
    }
    else {
      rvals<-rs[,i]
      pvals<-ps[,i]
      wvals<-rn2w(rvals,result$nval)
      wvals_sig<-wvals
      wvals_sig[pvals>=alpha]<-NA
      wvals_nsig<-wvals
      wvals_nsig[pvals<alpha]<-NA
      if (wPlotScale=="log10") {
        pts<-data.frame(x=wvals*0+xoff[i],y1=log10(wvals),y2=log10(wvals_nsig))
      } else {
        pts<-data.frame(x=wvals*0+xoff[i],y1=wvals,y2=wvals_nsig)
      }
      
      if (i==1){g<-ggplot(pts,aes(x=x, y=y))}      
      g <- expected_plot(g,pts,result,IV,DV,"w")
      
      # lpts<-data.frame(x = xoff[i]-0.95, y = ylim[1], label = paste("p(sig) =",format(mean(pvals<alpha),digits=graph_precision)))
      # g<-g+geom_label(data=lpts,aes(x = x, y = y, label=label), hjust=0, vjust=0, fill = "white",size=3)
    }
    if (length(xoff)>1)
      switch (i,
              {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Main Effect 1",color="white",size=3)},
              {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Main Effect 2",color="white",size=3)},
              {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Interaction",color="white",size=3)}
      )
  }
  g<-g+plotTheme+
    coord_cartesian(xlim = c(min(xoff),max(xoff))+c(-1,1), ylim = ylim+c(0,diff(ylim)/16))
  
  if (wPlotScale=="log10") {
    g<-g+geom_hline(yintercept=log10(alpha), linetype="dotted", color="#44FF22", size=0.5)+
      geom_hline(yintercept=log10(0.5), linetype="dotted", color="#44FF22", size=0.5)+
      geom_hline(yintercept=log10(0.8), linetype="dotted", color="#44FF22", size=0.5)+
      geom_hline(yintercept=log10(wActual), color="#FFFFFF", size=0.5)+
      scale_y_continuous(breaks=c(-4,-3,-2,-1,0),labels=c(0.0001,0.001,0.01,0.1,1))+ 
      ylab(bquote(log[10](w[est])))
  } else
  {
    g<-g+geom_hline(yintercept=alpha, linetype="dotted", color="#44FF22", size=0.5)+
         geom_hline(yintercept=0.5, linetype="dotted", color="#44FF22", size=0.5)+
         geom_hline(yintercept=0.8, linetype="dotted", color="#44FF22", size=0.5)+
         geom_hline(yintercept=wActual, color="#FFFFFF", size=0.5)+
         scale_y_continuous(breaks=seq(0,1,0.1),labels=seq(0,1,0.1))+ 
         ylab(bquote(w[est]))
  }
  g+theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
}



nw_plot<-function(result,IV,IV2=NULL,DV,r=0,n=0){
  nwActual<-rw2n(r,0.8)
  ylim<-c(log10(1), log10(10000))
  
  if (is.null(IV2) & all(is.na(result$rIVIV2DV))){
    xoff=0
    rs<-result$rIV
    ps<-result$pIV
  } else {
    if (is.na(result$rIVIV2DV[1])){xoff=c(0,2)}else{xoff=c(0,2,4)}
    switch (result$showType,
            "direct"={
              rs<-result$r$direct
              ps<-result$p$direct
            },
            "unique"={
              rs<-result$r$unique
              ps<-result$p$unique
            },
            "total"={
              rs<-result$r$total
              ps<-result$p$total
            },
            "coefficients"={
              rs<-result$r$coefficients
              ps<-result$p$direct
            },
    )
  }
  
  for (i in 1:length(xoff)){
    single<-TRUE
    if (is.matrix(rs)) {
      if (nrow(rs)>points_threshold) {single<-FALSE}
    }
    if (single) {
      if (is.matrix(rs) && nrow(rs)>1){
        rvals<-rs[,i]
        pvals<-ps[,i]
        xr<-runif(nrow(rs),min=-0.2,max=0.2)
      }
      else {
        rvals<-rs[i]
        pvals<-ps[i]
        xr=0
      }
      nwvals<-rw2n(rvals,0.8)
      
      pts=data.frame(x=xoff[i]+xr,y=log10(nwvals))

      if (i==1){g<-ggplot(pts,aes(x=x, y=y))}      
      
      if (length(rvals)==1){
        z_se<-1/sqrt(result$nval-3)
        z_ci<-atanh(rvals)+z_se*c(-1,1)*qnorm(1-(1-CI)/2)
        r_ci<-tanh(z_ci)
        nw_ci<-rw2n(abs(r_ci),0.8)
        if (r_ci[1]<0 && r_ci[2]>0) {
          nw_ci[2]<-min(nw_ci)
          nw_ci[1]<-100000
        }
        nw_ci<-log10(nw_ci)
        pts1se<-data.frame(x=c(0,0)+xoff[i],y=nw_ci)
        g<-g+geom_line(data=pts1se,aes(x=x,y=y),arrow=arrow(length=unit(se_arrow,"cm"),ends="both"),colour=se_colour,size=se_size)
      } 
      
      dotSize=min(10,20/sqrt(length(rvals)))
      pt_col<-plotcolours$infer_nsigC
      use<-(pvals>=alpha)
      pts1=pts[use,]
      g<-g+geom_point(data=pts1,aes(x=x, y=y),shape=21, colour = "black", fill = pt_col, size = dotSize)
      pt_col<-plotcolours$infer_sigC
      pts2=pts[!use,]
      g<-g+geom_point(data=pts2,aes(x=x, y=y),shape=21, colour = "black", fill = pt_col, size = dotSize)
    }
    else {
      rvals<-rs[,i]
      pvals<-ps[,i]
      nwvals<-rw2n(rvals,0.8)
      
      nwvals_sig<-nwvals
      nwvals_sig[pvals>=alpha]<-NA
      nwvals_nsig<-nwvals
      nwvals_nsig[pvals<alpha]<-NA
        pts<-data.frame(x=nwvals*0+xoff[i],y1=log10(nwvals),y2=log10(nwvals_nsig))

      if (i==1){g<-ggplot(pts,aes(x=x, y=y))}      
      g <- expected_plot(g,pts,result,IV,DV,"nw")
      
      # lpts<-data.frame(x = xoff[i]-0.95, y = ylim[1], label = paste("p(sig) =",format(mean(pvals<alpha),digits=graph_precision)))
      # g<-g+geom_label(data=lpts,aes(x = x, y = y, label=label), hjust=0, vjust=0, fill = "white",size=3)
    }
    if (length(xoff)>1)
      switch (i,
              {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Main Effect 1",color="white",size=3)},
              {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Main Effect 2",color="white",size=3)},
              {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Interaction",color="white",size=3)}
      )
  }
  g<-g+plotTheme+
    coord_cartesian(xlim = c(min(xoff),max(xoff))+c(-1,1), ylim = ylim+c(0,diff(ylim)/16))
  
  g<-g+geom_hline(yintercept=log10(50), linetype="dotted", color="#FFFF22", size=0.5)+
    geom_hline(yintercept=log10(nwActual), color="#FFFFFF", size=0.5)+
    geom_hline(yintercept=log10(500), linetype="dotted", color="#FFFF22", size=0.5)+
    scale_y_continuous(breaks=seq(0,4,1),labels=10^seq(0,4,1))+ 
    ylab(bquote(log[10](n[w=80])))
  g+theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
}


w1_plot<-function(result,IV,IV2=NULL,DV,r=0,n=0){
  wActual<-rn2w(r,n)
  if (wPlotScale=="log10") {
    ylim<-c(log10(alpha), 0)
  } else {
    ylim<-c(0, 1)
  }
  
  if (is.null(IV2) & all(is.na(result$rIVIV2DV))){
    xoff=0
  } else {
    if (is.na(result$rIVIV2DV[1])){xoff=c(0,2)}else{xoff=c(0,2,4)}
  }
  
  for (i in 1:length(xoff)){
    
    switch (i,rvals<-result$rIV,rvals<-result$rIV2,rvals<-result$rIVIV2DV)
    switch (i,pvals<-result$pIV,pvals<-result$pIV2,pvals<-result$pIVIV2DV)
    wvals<-rn2w(rvals,result$nval)
    
  if (length(wvals)==1) {
    if (wPlotScale=="log10"){
      pts=data.frame(x=0+xoff[i],y=log10(wvals))
    } else { pts=data.frame(x=0+xoff[i],y=wvals)
    }
    dotSize=min(10,20/sqrt(length(wvals)))
    pt_col<-plotcolours$infer_nsigC
    if (pvals<alpha) {pt_col<-plotcolours$infer_sigC}
    
    if (i==1){g<-ggplot(pts,aes(x=x, y=y))}      
    g<-g+geom_point(shape=21, colour = "black", fill = pt_col, size = dotSize)+
      theme(legend.position = "none")
    
    if (length(rvals)==1){
      z_se<-1/sqrt(result$nval-3)
      z_ci<-atanh(rvals)+z_se*c(-1,1)*qnorm(1-(1-CI)/2)
      r_ci<-tanh(z_ci)
      w_ci<-rn2w(abs(r_ci),result$nval)
      if (r_ci[1]<0 && r_ci[2]>0) w_ci[1]<-0.05
      if (wPlotScale=="log10") w_ci<-log10(w_ci)
      pts1se<-data.frame(x=c(0,0)+xoff[i],y=w_ci)
      g<-g+geom_line(data=pts1se,aes(x=x,y=y),colour="black")
    } 
  }
  else {
    wvals_sig<-wvals
    wvals_sig[pvals>=alpha]<-NA
    wvals_nsig<-wvals
    wvals_nsig[pvals<alpha]<-NA
    if (wPlotScale=="log10"){
      pts<-data.frame(x=wvals*0+xoff[i],y1=log10(wvals),y2=log10(wvals_nsig))
    } else {
      pts<-data.frame(x=wvals*0+xoff[i],y1=wvals,y2=wvals_nsig)
    }
    
    if (i==1){g<-ggplot(pts,aes(x=x, y=y))}      
    g <- expected_plot(g,pts,result,IV,DV,"w")
    lpts<-data.frame(x = xoff[i]-0.95, y = ylim[1],label = paste("actual =",format(wActual[i],digits=graph_precision)))
    g<-g+geom_label(data=lpts,aes(x = x, y = y, label = label), hjust=0, vjust=0, fill = "white",size=3)
    if (length(xoff)>1)
      switch (i,
              {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Main Effect 1",color="white",size=3)},
              {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Main Effect 2",color="white",size=3)},
              {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Interaction",color="white",size=3)}
      )
  }
  }
  g<-g+plotTheme
  if (wPlotScale=="log10") {
  g<-g+geom_hline(yintercept=log10(0.8), linetype="dotted", color="#FFFF22", size=0.5)+
    geom_hline(yintercept=log10(wActual), color="#FFFFFF", size=0.5)+
    geom_hline(yintercept=log10(alpha), linetype="dotted", color="#FFFF22", size=0.5)+
    scale_y_continuous(breaks=log10(c(alpha,seq(0.1,1,0.1))),labels=c(alpha,seq(0.1,1,0.1)))+
    ylab(bquote(log[10](w[est])))
  }
  else {
    g<-g+geom_hline(yintercept=0.8, linetype="dotted", color="#FFFF22", size=0.5)+
      geom_hline(yintercept=wActual, color="#FFFFFF", size=0.5)+
      geom_hline(yintercept=alpha, linetype="dotted", color="#FFFF22", size=0.5)+
      scale_y_continuous(breaks=seq(0.0,1,0.1),labels=c(seq(0.0,1,0.1)))+
      ylab(bquote(w[est]))
  }

    g<-g+coord_cartesian(xlim = c(min(xoff),max(xoff))+c(-1,1), ylim = ylim+c(-1,1)*diff(ylim)/16)
    g+theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    
  
}

nw1_plot<-function(result,IV,IV2=NULL,DV,r=0,n=0){
  nwActual<-rw2n(r,0.8)
  ylim<-c(log10(1), log10(10000))
  
  if (is.null(IV2) & all(is.na(result$rIVIV2DV))){
    xoff=0
  } else {
    if (is.na(result$rIVIV2DV[1])){xoff=c(0,2)}else{xoff=c(0,2,4)}
  }
  
  for (i in 1:length(xoff)){
    switch (i,rvals<-result$rIV,rvals<-result$rIV2,rvals<-result$rIVIV2DV)
    switch (i,pvals<-result$pIV,pvals<-result$pIV2,pvals<-result$pIVIV2DV)
    nwvals<-rw2n(rvals,0.8)
    
    if (length(nwvals)==1) {
      pts=data.frame(x=0+xoff[i],y=log10(nwvals))
      dotSize=min(10,20/sqrt(length(nwvals)))
      pt_col<-plotcolours$infer_nsigC
      if (pvals<alpha) {pt_col<-plotcolours$infer_sigC}
      
      if (i==1){g<-ggplot(pts,aes(x=x, y=y))}      
      g<-g+geom_point(shape=21, colour = "black", fill = pt_col, size = dotSize)+
        theme(legend.position = "none")
    }
    else {
      nwvals_sig<-nwvals
      nwvals_sig[pvals>=alpha]<-NA
      nwvals_nsig<-nwvals
      nwvals_nsig[pvals<alpha]<-NA
      pts<-data.frame(x=nwvals*0+xoff[i],y1=log10(nwvals),y2=log10(nwvals_nsig))
      
      if (i==1){g<-ggplot(pts,aes(x=x, y=y))}      
      g <-expected_plot(g,pts,result,IV,DV,"nw")
      lpts<-data.frame(x = xoff[i]-0.95, y = ylim[1], label=paste("actual =",format(nwActual[i],digits=graph_precision)))
      g<-g+geom_label(data=lpts,aes(x = x, y = y, label = label), hjust=0, vjust=0, fill = "white",size=3)
      if (length(xoff)>1)
        switch (i,
                {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Main Effect 1",color="white",size=3)},
                {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Main Effect 2",color="white",size=3)},
                {g<-g+annotate("text",x=xoff[i],y=ylim[2]+diff(ylim)/16,label="Interaction",color="white",size=3)}
        )
    }
  }
  
  g<-g+geom_hline(yintercept=log10(50), linetype="dotted", color="#FFFF22", size=0.5)+
    geom_hline(yintercept=log10(nwActual), color="#FFFFFF", size=0.5)+
    geom_hline(yintercept=log10(500), linetype="dotted", color="#FFFF22", size=0.5)+
    plotTheme+
    scale_y_continuous(breaks=seq(0,4,1),labels=10^seq(0,4,1))+ 
    coord_cartesian(xlim = c(min(xoff),max(xoff))+c(-1,1), ylim = ylim+c(-1,1)*diff(ylim)/16)+
    ylab(bquote(log[10](n[w=80])))
  g+theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  
}


e2_plot<-function(result,IV,IV2=NULL,DV,r=0,n=0){
  p_plot(result,IV,IV2,DV,r,n,ptype="e2")
}

e1_plot<-function(result,IV,IV2=NULL,DV,r=0,n=0){
  p_plot(result,IV,IV2,DV,r,n,ptype="e1")
}

ci1_plot<-function(result,IV=NULL,IV2=NULL,DV=NULL,r=0,n=0){
  result$rIV<-r2ci(result$rIV,result$nval[1],-1)
  if (!is.null(IV2) | !all(is.na(result$rIVIV2DV))) {
    result$r$direct<-r2ci(result$r$direct,result$nval[1],-1)
    result$r$unique<-r2ci(result$r$unique,result$nval[1],-1)
    result$r$total<-r2ci(result$r$total,result$nval[1],-1)
  }
  r_plot(result,IV,IV2,DV,r,n)
}

ci2_plot<-function(result,IV,IV2=NULL,DV,r=0,n=0){
  result$rIV<-r2ci(result$rIV,result$nval[1],+1)
  if (!is.null(IV2) | !all(is.na(result$rIVIV2DV))) {
    result$r$direct<-r2ci(result$r$direct,result$nval[1],+1)
    result$r$unique<-r2ci(result$r$unique,result$nval[1],+1)
    result$r$total<-r2ci(result$r$total,result$nval[1],+1)
  }
  r_plot(result,IV,IV2,DV,r,n)
}


