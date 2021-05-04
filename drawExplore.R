no_se_multiple<-TRUE

all_cols<-c()

drawExplore<-function(Iv,IV2,DV,effect,design,explore,exploreResult){
  rho<-effect$rIV
  
  vals<-exploreResult$vals
  if (is.character(vals[1])){
    vals<-factor(vals)
    doLine=FALSE
  } else {doLine=TRUE}
  
  g<-ggplot()
  
  if (!is.null(IV2) && is.element(explore$Explore_show,c("EffectSize","p","w","p(sig)"))) {
    switch (explore$Explore_show,
            "EffectSize"={use_cols<<-c(hsv(0.1,1,1),hsv(0.1+0.075,1,1),hsv(0.1+0.15,1,1))},
            "p"=         {use_cols<-c(hsv(0,1,1),hsv(0+0.075,1,1),hsv(0+0.15,1,1))},
            "w"=         {use_cols<-c(hsv(0.65,1,1),hsv(0.65+0.075,1,1),hsv(0.65+0.15,1,1))},
            "p(sig)"=    {use_cols<-c("#FFFFFF","#DDDDDD","#AAAAAA")},
    )
    names(use_cols)<-c("direct","unique","total")
    all_cols<<-use_cols
    g<-g+scale_fill_manual(name=explore$Explore_whichShow,values=all_cols)
    use_col_names<-TRUE
  } else {
    all_cols<-c()
    use_col_names<-FALSE
  }
  
  markersize<-7
  ni_max<-1
  if (use_col_names && explore$Explore_extraShow=="all"){
    markersize<-4
    ni_max<-3
  } 
  
  for (ni in 1:ni_max){
    if (ni_max>1) {
      switch (ni,
            {explore$Explore_extraShow<-"direct"},
            {explore$Explore_extraShow<-"unique"},
            {explore$Explore_extraShow<-"total"})
    }
    
    extra_y_label<-""
    if (is.null(IV2)){
      rVals<-exploreResult$rIVs
      pVals<-exploreResult$pIVs
    } else {
      switch (explore$Explore_whichShow,
              "Main 1"={
                rVals<-exploreResult$r1[[explore$Explore_extraShow]]
                pVals<-exploreResult$p1[[explore$Explore_extraShow]]
                extra_y_label<-paste("Main Effect 1:",explore$Explore_extraShow)
              },
              "Main 2"={
                rVals<-exploreResult$r2[[explore$Explore_extraShow]]
                pVals<-exploreResult$p2[[explore$Explore_extraShow]]
                extra_y_label<-paste("Main Effect 2:",explore$Explore_extraShow)
              },
              "Interaction"={
                rVals<-exploreResult$r3[[explore$Explore_extraShow]]
                pVals<-exploreResult$p3[[explore$Explore_extraShow]]
                extra_y_label<-paste("Interaction:",explore$Explore_extraShow)
              }
      )
    }

    extra_x_label<-""
    switch (explore$Explore_show,
            "EffectSize"={
              showVals<-rVals
              if (is.null(IV2)){
                col<-"yellow"
                colFill<-col
                lines<-c(0,effect$rIV)
              } else {
                switch (explore$Explore_whichShow,
                        "Main 1"={
                          lines<-c(0,effect$rIV)
                        },
                        "Main 2"={
                          lines<-c(0,effect$rIV2)
                        },
                        "Interaction"={
                          lines<-c(0,effect$rIVIV2DV)
                        }
                )
                col<-all_cols[[explore$Explore_extraShow]]
                colFill<-names(all_cols[explore$Explore_extraShow])
              }
            },
            "p"={
              showVals<-pVals
              lines<-c(0.05)
              if (pPlotScale=="log10"){
                showVals<-log10(showVals)
                lines<-log10(lines)
              }
              if (is.null(IV2)){
                col<-"red"
                colFill<-col
              } else {
                col<-all_cols[[explore$Explore_extraShow]]
                colFill<-names(all_cols[explore$Explore_extraShow])
              }
            },
            "w"={
              showVals<-rn2w(rVals,exploreResult$nvals)
              lines<-c(0.05,0.8)
              if (wPlotScale=="log10"){
                showVals<-log10(showVals)
                lines<-log10(lines)
              }
              if (is.null(IV2)){
                col<-"blue"
                colFill<-col
              } else {
                col<-all_cols[[explore$Explore_extraShow]]
                colFill<-names(all_cols[explore$Explore_extraShow])
              }
            },
            "p(sig)"={
              y50<-c()
              y25<-c()
              y75<-c()
              for (i in 1:length(exploreResult$vals)){
                p<-mean(pVals[,i]<alpha,na.rm=TRUE)
                y50[i]<-p
                y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
              }
              lines<-c(0.05,0.8)
              if (is.null(IV2)){
                col<-"white"
                colFill<-col
              } else {
                col<-all_cols[[explore$Explore_extraShow]]
                colFill<-names(all_cols[explore$Explore_extraShow])
              }
            },
            "NHSTErrors"={
              y50<-c()
              y25<-c()
              y75<-c()
              for (i in 1:length(exploreResult$vals)){
                p<-mean(pVals[,i]<alpha,na.rm=TRUE)
                y50[i]<-p
                y75[i]<-p+sqrt(p*(1-p)/length(pVals[,i]))
                y25[i]<-p-sqrt(p*(1-p)/length(pVals[,i]))
              }

              y50e<-c()
              y25e<-c()
              y75e<-c()
              peVals<-exploreResult$null$pIVs
              for (i in 1:length(exploreResult$vals)){
                p<-mean(peVals[,i]<alpha,na.rm=TRUE)
                y50e[i]<-p
                y75e[i]<-p+sqrt(p*(1-p)/length(peVals[,i]))
                y25e[i]<-p-sqrt(p*(1-p)/length(peVals[,i]))
              }
              col<-"red"
              cole<-"green"
              colFill<-col
              lines<-c(0.05)
            }
    )
  
    if (is.element(explore$Explore_show,c("EffectSize","p","w"))) {
      quants<-explore$Explore_quants/2
      y75<-c()
      y50<-c()
      y25<-c()
      for (i in 1:length(exploreResult$vals)) {
        y75[i]<-quantile(showVals[,i],0.50+quants,na.rm=TRUE)
        y50[i]<-quantile(showVals[,i],0.50,na.rm=TRUE)
        y25[i]<-quantile(showVals[,i],0.50-quants,na.rm=TRUE)
      }
    }

    pts1<-data.frame(vals=vals,y50=y50,y25=y25,y75=y75)
    
    if (explore$Explore_show=="NHSTErrors") {
      pts2<-data.frame(vals=vals,y50e=y50e,y25e=y25e,y75e=y75e)
      
      areaVals<-c(vals[1],vals,vals[length(vals)])
      areaData<-c(1,y50,1)
      ptsNHST<-data.frame(x=areaVals,y=areaData)
      g<-g+geom_polygon(data=ptsNHST,aes(x=x,y=y),fill=col,alpha=0.5)
      areaData<-c(0,y50e,0)
      ptsNHST<-data.frame(x=areaVals,y=areaData)
      g<-g+geom_polygon(data=ptsNHST,aes(x=x,y=y),fill=cole,alpha=0.5)
      
      if (doLine) {
        g<-g+geom_line(data=pts1,aes(x=vals,y=y50),color=col)
        g<-g+geom_line(data=pts2,aes(x=vals,y=y50e),color=cole)
      } 
      g<-g+geom_point(data=pts1,aes(x=vals,y=y50),shape=21, colour = "black", fill = col, size = 7)
      g<-g+geom_point(data=pts2,aes(x=vals,y=y50e),shape=21, colour = "black", fill = cole, size = 7)
      
    } else {
      if (doLine) {
        pts1f<-data.frame(x=c(vals,rev(vals)),y=c(y25,rev(y75)))
        if (ni_max==1 || !no_se_multiple) {
          g<-g+geom_polygon(data=pts1f,aes(x=x,y=y),fill=col,alpha=0.5)
        }
        g<-g+geom_line(data=pts1,aes(x=vals,y=y50),color="black")
      } else{
        if (ni_max==1 || !no_se_multiple){
          g<-g+geom_errorbar(data=pts1,aes(x=vals,ymin=y25,ymax=y75,width=0.2))
        }
      }
      if (use_col_names){
        pts1<-data.frame(x=vals,y=y50,fill=explore$Explore_extraShow)
        g<-g+geom_point(data=pts1,aes(x=x,y=y,fill=fill),shape=21, colour = "black", size = markersize)
      } else {
        g<-g+geom_point(data=pts1,aes(x=vals,y=y50),shape=21, colour = "black",fill=col, size = markersize)
      }
    }
    
    g<-g+geom_hline(yintercept=lines,color="white", linetype="dotted",size=0.5)    
    
    # if (is.element(explore$Explore_show,c("EffectSize","Interaction")) && is.element(explore$Explore_type,c("EffectSize","EffectSize1","EffectSize2","Covariation","Interaction"))){
    #   pts3<-data.frame(x=c(-1,1),y=c(-1,1))
    #   g<-g+geom_line(data=pts3,aes(x=x,y=y),colour="white", linetype="dotted")
    # }
    if (explore$Explore_show=="p(sig)" && explore$Explore_type=="SampleSize"){
      w<-y50
      n<-exploreResult$vals
      minrw<-function(r,w,n){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
      r_est<-optimize(minrw,c(0,0.9),w=w,n=n)
      r_est<-r_est$minimum
      nvals<-seq(min(n),max(n),length.out=101)
      yvals<-rn2w(r_est,nvals)
      ptsn<-data.frame(x=nvals,y=yvals)
      g<-g+geom_line(data=ptsn,aes(x=x,y=y),color="white")
      
      minnw<-function(n,r,w){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
      n80<-optimize(minnw,c(10,explore$Explore_nRange),w=0.8,r=r_est)
      
      if (sum(n<n80$minimum)>=2 && sum(n>n80$minimum)>=2){
        label<-paste("n80 =",format(n80$minimum,digits=2),"  r_est =", format(r_est,digits=3))
      } else {
        label<-paste("Unsafe result","  r_est =", format(r_est,digits=3))
      }
      if (ni_max>1){label<-paste(explore$Explore_extraShow,": ",label,sep="")}
      lpts<-data.frame(x=0,y=0.8+(ni-1)/10,label=label)
      g<-g+geom_label(data=lpts,aes(x = x, y = y, label = label), hjust=0, vjust=0, fill = "white",size=3.5)
    }
    if (explore$Explore_show=="p(sig)" && explore$Explore_type=="EffectSize"){
      w<-y50
      r<-exploreResult$vals
      minrw<-function(r,w,n){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
      n_est<-optimize(minrw,c(0,100),w=w,r=r)
      n_est<-n_est$minimum
      rvals<-seq(min(r),max(r),length.out=101)
      yvals<-rn2w(rvals,n_est)
      ptsn<-data.frame(x=rvals,y=yvals)
      g<-g+geom_line(data=ptsn,aes(x=x,y=y),color="white")
      
      minnw<-function(n,r,w){sum(abs(w-rn2w(r,n)),na.rm=TRUE)}
      n80<-optimize(minnw,c(0,0.8),w=0.8,n=n_est)
      
      if (sum(r<n80$minimum)>=2 && sum(r>n80$minimum)>=2){
        label<-paste("n80 =",format(n80$minimum,digits=2),"  n_est =", format(n_est,digits=3))
      } else {
        label<-paste("Unsafe result","  n_est =", format(n_est,digits=3))
      }
      if (ni_max>1){label<-paste(explore$Explore_extraShow,": ",label,sep="")}
      lpts<-data.frame(x=0,y=0.8+(ni-1)/10,label=label)
      g<-g+geom_label(data=lpts,aes(x = x, y = y, label = label), hjust=0, vjust=0, fill = "white",size=3.5)
    }
  }
  
    switch (explore$Explore_show,
            "EffectSize"={
              ylim<-c(-1,1)
              ylabel<-bquote(r[sample])
            },
            "p"={
              ylim<-c(-4,0)
              ylabel<-bquote(log[10](p))
              g<-g+scale_y_continuous(breaks=c(-4,-3,-2,-1,0),labels=c(0.0001,0.001,0.01,0.1,1))
            },
            "w"={
              if (wPlotScale=="log10"){
                ylim<-c(-2,0)
                ylabel<-bquote(log[10](w[est]))
                g<-g+scale_y_continuous(breaks=c(-2,-1,0),labels=c(0.01,0.1,1))
              } else {
                ylim<-c(0,1)
                ylabel<-bquote(w[est])
              }
            },
            "p(sig)"={
              ylim<-c(0,1)
              ylabel<-"p(sig)"
            },
            "NHSTErrors"={
              ylim<-c(0,1)
              ylabel<-"Type I"
            }
    )
  
  if (explore$full_ylim){
  g<-g+coord_cartesian(ylim = ylim*1.05)
  }
    g<-g+ylab(ylabel)
    switch (explore$Explore_type,
            "EffectSize"={g<-g+xlab(bquote(r[population]))},
            "EffectSize1"={
              g<-g+xlab(bquote(r[population]))
              g<-g+annotate("text",x=Inf,y=-Inf, hjust=1, vjust=0, angle=0, label="Main Effect 1",color="white")
            },
            "EffectSize2"={
              g<-g+xlab(bquote(r[population]))
              g<-g+annotate("text",x=Inf,y=-Inf, hjust=1, vjust=0, angle=0, label="Main Effect 2",color="white")
            },
            "Covariation"={
              g<-g+xlab(bquote(r[population]))
              g<-g+annotate("text",x=Inf,y=-Inf, hjust=1, vjust=0, angle=0, label="Covariation",color="white")
            },
            "Interaction"={
              g<-g+xlab(bquote(r[population]))
              g<-g+annotate("text",x=Inf,y=-Inf, hjust=1, vjust=0, angle=0, label="Interaction",color="white")
            },
            g<-g+xlab(explore$Explore_type)
    )
    
    if (explore$Explore_show=="NHSTErrors") {
      g<-g+scale_y_continuous(sec.axis = sec_axis(~ 1-.,name="Type II"))+
        theme(axis.title.y.left = element_text(color="darkgreen"),axis.title.y.right = element_text(color="darkred"))
    }
  
  g+plotTheme
}

