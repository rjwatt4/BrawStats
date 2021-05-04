npoints=10
min_n=10
max_n=250
min_prop=0.2
effectSizeRange=0.8
quants=0.25
n_logscale=FALSE

exploreSimulate <- function(IV,IV2,DV,effect,design,evidence,explore){
  
  npoints<-explore$Explore_npoints
  quants<-explore$Explore_quants
  max_n<-explore$Explore_nRange
  effectSizeRange<-explore$Explore_esRange
  anomaliesRange<-explore$Explore_anomRange
  kurtRange<-10^5
  
  showNotification(paste(format(0),format(npoints),sep="/"),id="counting",duration=Inf,closeButton = FALSE,type = "message")
  
  switch (explore$Explore_type,
          "IVType"={vals<-c("Interval","Cat2","Cat3")},
          "DVType"={vals<-c("Interval","Cat2")},
          "IVIV2Type"={vals<-c("IntInt","Cat2Int","Cat3Int","IntCat","Cat2Cat","Cat3Cat")},
          "IVDVType"={vals<-c("IntInt","Cat2Int","Cat3Int","IntCat","Cat2Cat","Cat3Cat")},
          "IVprop"={vals<-seq(min_prop,1,length.out=npoints)},
          "IVskew"={vals<-seq(0,1,length.out=npoints)},
          "IVkurtosis"={vals<-seq(0,log10(kurtRange),length.out=npoints)},
          "DVprop"={vals<-seq(min_prop,1,length.out=npoints)},
          "DVskew"={vals<-seq(0,1,length.out=npoints)},
          "DVkurtosis"={vals<-seq(0,log10(kurtRange),length.out=npoints)},
          "EffectSize"={vals<-seq(0,1,length.out=npoints)*effectSizeRange},
          "EffectSize1"={vals<-seq(-1,1,length.out=npoints)*effectSizeRange},
          "EffectSize2"={vals<-seq(-1,1,length.out=npoints)*effectSizeRange},
          "Covariation"={vals<-seq(-1,1,length.out=npoints)*effectSizeRange},
          "Interaction"={vals<-seq(-1,1,length.out=npoints)*effectSizeRange},
          
          "SampleSize"={
            if (n_logscale){
              vals<-round(10^seq(log10(min_n),log10(max_n),length.out=npoints))
            }else{
              vals<-round(seq(min_n,max_n,length.out=npoints))
            }
          },
          "Method"={vals<-c("Random","Opportunity")},
          "Usage"={vals<-c("Between","Within")},
          "Dependence"={vals<-seq(0,anomaliesRange,length.out=npoints)},
          "Outliers"={vals<-seq(0,anomaliesRange,length.out=npoints)},
          "Heteroscedasticity"={vals<-seq(0,1,length.out=npoints)},
          "IVRange"={vals<-seq(3,0.5,length.out=npoints)},
          "DVRange"={vals<-seq(3,0.5,length.out=npoints)}
  )

  if (explore$Append){
    exploreResult<-exploreResultHold
  } else { 
    exploreResult<-c(explore,list(rIVs=c(),pIVs=c(),nvals=c(),psig=c(),
                                  r1=list(direct=c(),unique=c(),total=c()),
                                  r2=list(direct=c(),unique=c(),total=c()),
                                  r3=list(direct=c(),unique=c(),total=c())
    )
    )
  }
  
  if (explore$Explore_length>20) {n_sims<-10} else {n_sims<-explore$Explore_length}

  for (ni in seq(n_sims,explore$Explore_length,n_sims)){
    
    main_res<-list(rval=c(),pval=c(),nval=c(),
                   r1=list(direct=c(),unique=c(),total=c()),
                   r2=list(direct=c(),unique=c(),total=c()),
                   r3=list(direct=c(),unique=c(),total=c()))
  
    for (i in 1:length(vals)){
    showNotification(paste("n=",format(ni), ":  ", format(i),"/",format(length(vals)),sep=""),id="counting",duration=Inf,closeButton = FALSE,type = "message")
    switch (explore$Explore_type,
            "IVType"={
              switch (vals[i],
                      "Cat2"={
                        IV$type<-"Categorical"
                        IV$ncats<-2
                        IV$cases<-c("C1","C2")
                      },
                      "Cat3"={
                        IV$type<-"Categorical"
                        IV$ncats<-3
                        IV$cases<-c("C1","C2","C3")
                      },
                      "Interval"={IV$type<-"Interval"}
              )
              },
            "DVType"={
              switch (vals[i],
                      "Cat2"={
                        DV$type<-"Categorical"
                        DV$ncats<-2
                        DV$cases<-c("E1","E2")
                      },
                      # "Cat3"={
                      #   DV$type<-"Categorical"
                      #   DV$ncats<-3
                      #   DV$cases<-c("D1","D2","D3")
                      # },
                      "Interval"={DV$type<-"Interval"}
              )
            },
            "IVDVType"={
              switch (vals[i],
                      "IntInt"={
                        IV$type<-"Interval"
                        DV$type<-"Interval"
                      },
                      "Cat2Int"={
                        IV$type<-"Categorical"
                        IV$ncats<-2
                        IV$cases<-c("C1","C2")
                        DV$type<-"Interval"
                      },
                      "Cat3Int"={
                        IV$type<-"Categorical"
                        IV$ncats<-3
                        IV$cases<-c("C1","C2","C3")
                        DV$type<-"Interval"
                      },
                      "IntCat"={
                        IV$type<-"Interval"
                        DV$type<-"Categorical"
                        DV$ncats<-2
                        DV$cases<-c("E1","E2")
                      },
                      "Cat2Cat"={
                        IV$type<-"Categorical"
                        IV$ncats<-2
                        IV$cases<-c("C1","C2")
                        DV$type<-"Categorical"
                        DV$ncats<-2
                        DV$cases<-c("E1","E2")
                      },
                      "Cat3Cat"={
                        IV$type<-"Categorical"
                        IV$ncats<-3
                        IV$cases<-c("C1","C2","C3")
                        DV$type<-"Categorical"
                        DV$ncats<-2
                        DV$cases<-c("E1","E2")
                      }
              )
            },
            "IVIV2Type"={
              switch (vals[i],
                      "IntInt"={
                        IV$type<-"Interval"
                        IV2$type<-"Interval"
                      },
                      "Cat2Int"={
                        IV$type<-"Categorical"
                        IV$ncats<-2
                        IV$cases<-c("C1","C2")
                        IV2$type<-"Interval"
                      },
                      "Cat3Int"={
                        IV$type<-"Categorical"
                        IV$ncats<-3
                        IV$cases<-c("C1","C2","C3")
                        IV2$type<-"Interval"
                      },
                      "IntCat"={
                        IV$type<-"Interval"
                        IV2$type<-"Categorical"
                        IV2$ncats<-2
                        IV2$cases<-c("D1","D2")
                      },
                      "Cat2Cat"={
                        IV$type<-"Categorical"
                        IV$ncats<-2
                        IV$cases<-c("C1","C2")
                        IV2$type<-"Categorical"
                        IV2$ncats<-2
                        IV2$cases<-c("D1","D2")
                      },
                      "Cat3Cat"={
                        IV$type<-"Categorical"
                        IV$ncats<-3
                        IV$cases<-c("C1","C2","C3")
                        IV2$type<-"Categorical"
                        IV2$ncats<-2
                        IV2$cases<-c("D1","D2")
                      }
              )
            },
            "IVprop"={
              IV$type<-"Categorical"
              IV$proportions<-paste(c(vals[i],1),collapse=",")
            },
            "IVskew"={
              IV$type<-"Interval"
              IV$skew<-vals[i]
            },
            "IVkurtosis"={
              IV$type<-"Interval"
              IV$kurtosis<-10^vals[i]
            },
            "DVprop"={
              DV$type<-"Categorical"
              DV$proportions<-paste(c(vals[i],1),collapse=",")
            },
            "DVskew"={
              DV$type<-"Interval"
              DV$skew<-vals[i]
            },
            "DVkurtosis"={
              DV$type<-"Interval"
              DV$kurtosis<-10^vals[i]
            },
            "EffectSize"={effect$rIV<-vals[i]},
            "EffectSize1"={effect$rIV<-vals[i]},
            "EffectSize2"={effect$rIV2<-vals[i]},
            "Covariation"={effect$rIVIV2<-vals[i]},
            "Interaction"={effect$rIVIV2DV<-vals[i]},
            "SampleSize"={design$sN<-round(vals[i])},
            "Method"={design$sMethod<-vals[i]},
            "Usage"={design$sIV1Use<-vals[i]},
            "Dependence"={design$sDependence<-vals[i]},
            "Outliers"={design$sOutliers<-vals[i]},
            "Heteroscedasticity"={design$sHeteroscedasticity<-vals[i]},
            "IVRange"={
              design$sRangeOn<-TRUE
              design$sIVRange<-vals[i]*c(-1,1)
              },
            "DVRange"={
              design$sRangeOn<-TRUE
              design$sDVRange<-vals[i]*c(-1,1)
              }
    )

    if (explore$doNull) {
      effect$rIV<-0
      effect$rIV2<-0
      effect$rIVIV2DV<-0
    }
    res<-multipleAnalysis(IV,IV2,DV,effect,design,evidence,n_sims,appendData=FALSE,showProgress=FALSE)
      
    main_res$rval<-cbind(main_res$rval,res$rIV)
    main_res$pval<-cbind(main_res$pval,res$pIV)
    main_res$nval<-cbind(main_res$nval,res$nval)

    if (!is.null(IV2)){
    main_res$r1$direct<-cbind(main_res$r1$direct,res$r$direct[,1])
    main_res$r1$unique<-cbind(main_res$r1$unique,res$r$unique[,1])
    main_res$r1$total<-cbind(main_res$r1$total,res$r$total[,1])
    
    main_res$r2$direct<-cbind(main_res$r2$direct,res$r$direct[,2])
    main_res$r2$unique<-cbind(main_res$r2$unique,res$r$unique[,2])
    main_res$r2$total<-cbind(main_res$r2$total,res$r$total[,2])
    
    main_res$r3$direct<-cbind(main_res$r3$direct,res$r$direct[,3])
    main_res$r3$unique<-cbind(main_res$r3$unique,res$r$unique[,3])
    main_res$r3$total<-cbind(main_res$r3$total,res$r$total[,3])
    
    main_res$p1$direct<-cbind(main_res$p1$direct,res$p$direct[,1])
    main_res$p1$unique<-cbind(main_res$p1$unique,res$p$unique[,1])
    main_res$p1$total<-cbind(main_res$p1$total,res$p$total[,1])
    
    main_res$p2$direct<-cbind(main_res$p2$direct,res$p$direct[,2])
    main_res$p2$unique<-cbind(main_res$p2$unique,res$p$unique[,2])
    main_res$p2$total<-cbind(main_res$p2$total,res$p$total[,2])
    
    main_res$p3$direct<-cbind(main_res$p3$direct,res$p$direct[,3])
    main_res$p3$unique<-cbind(main_res$p3$unique,res$p$unique[,3])
    main_res$p3$total<-cbind(main_res$p3$total,res$p$total[,3])
    }
    # if (i>1) {removeNotification(id = "counting")}
  }
    
    exploreResult$rIVs<-rbind(exploreResult$rIVs,main_res$rval)
    exploreResult$pIVs<-rbind(exploreResult$pIVs,main_res$pval)
    exploreResult$nvals<-rbind(exploreResult$nvals,main_res$nval)
    
    exploreResult$r1$direct<-rbind(exploreResult$r1$direct,main_res$r1$direct)
    exploreResult$r1$unique<-rbind(exploreResult$r1$unique,main_res$r1$unique)
    exploreResult$r1$total<-rbind(exploreResult$r1$total,main_res$r1$total)
    
    exploreResult$r2$direct<-rbind(exploreResult$r2$direct,main_res$r2$direct)
    exploreResult$r2$unique<-rbind(exploreResult$r2$unique,main_res$r2$unique)
    exploreResult$r2$total<-rbind(exploreResult$r2$total,main_res$r2$total)
    
    exploreResult$r3$direct<-rbind(exploreResult$r3$direct,main_res$r3$direct)
    exploreResult$r3$unique<-rbind(exploreResult$r3$unique,main_res$r3$unique)
    exploreResult$r3$total<-rbind(exploreResult$r3$total,main_res$r3$total)
    
    exploreResult$p1$direct<-rbind(exploreResult$p1$direct,main_res$p1$direct)
    exploreResult$p1$unique<-rbind(exploreResult$p1$unique,main_res$p1$unique)
    exploreResult$p1$total<-rbind(exploreResult$p1$total,main_res$p1$total)
    
    exploreResult$p2$direct<-rbind(exploreResult$p2$direct,main_res$p2$direct)
    exploreResult$p2$unique<-rbind(exploreResult$p2$unique,main_res$p2$unique)
    exploreResult$p2$total<-rbind(exploreResult$p2$total,main_res$p2$total)
    
    exploreResult$p3$direct<-rbind(exploreResult$p3$direct,main_res$p3$direct)
    exploreResult$p3$unique<-rbind(exploreResult$p3$unique,main_res$p3$unique)
    exploreResult$p3$total<-rbind(exploreResult$p3$total,main_res$p3$total)
    
  }
  removeNotification(id = "counting")
  
  exploreResult$wIVs<-rn2w(exploreResult$rIVs,exploreResult$nvals)
  for (i in 1:length(vals)) {
    p<-mean(exploreResult$pIVs[,i]<alpha)
    exploreResult$psig[i]<-p
    exploreResult$psig25[i]<-p-sqrt(p*(1-p)/length(exploreResult$pIVs[,i]))
    exploreResult$psig75[i]<-p+sqrt(p*(1-p)/length(exploreResult$pIVs[,i]))
  }
  exploreResult$vals<-vals
  
  exploreResultHold<<-exploreResult
  exploreResult
}
