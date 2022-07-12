
reportExpected<-function(IV,IV2,DV,evidence,result,nullresult,expectedType){
  
  if (is.null(IV2) | is.null(result$rIVIV2DV)){nc=3}
  else{
    if (is.na(result$rIVIV2DV[1])) {nc=6} else {nc=9}
  }
  if (expectedType=="NHSTErrors"){nc=3}
  
  # header
  outputText<-c("\bExpected",paste("nsims=",format(length(result$rIV)),sep=""),rep("",nc-2))
  outputText<-c(outputText,rep("",nc))
  if (!(is.null(IV2) | is.null(result$rIVIV2DV))){
    outputText<-c(outputText,"","\b Main Effect 1","","","\b Main Effect 2","")
    if (!is.na(result$rIVIV2DV[1])) outputText<-c(outputText,"","\bInteraction","")
    }

  if (is.null(IV2)) {
    rs<-result$rIV
    ps<-result$pIV
  } else {
    switch (result$showType,
            "direct"={rs<-result$r$direct
                      ps<-result$p$direct},
            "unique"={rs<-result$r$unique
                      ps<-result$p$unique},
            "total"={rs<-result$r$total
                     ps<-result$p$total},
            "all"={
                  rs<-c()
                  ps<-c()
                  xoff<-c()
                  for (jk in 1:ncol(result$r$direct)) {
                    rs<-cbind(rs,result$r$direct[,jk],result$r$unique[,jk],result$r$total[,jk])
                    ps<-cbind(ps,result$p$direct[,jk],result$p$unique[,jk],result$p$total[,jk])
                    xoff<-cbind(xoff,c(0,0.2,0.4)+(jk-1))
                  }
                },
            "coefficients"={rs<-result$r$coefficients
                      ps<-result$p$direct}
    )
  }
  
  # column labels
  switch (expectedType,
          "EffectSize"={outputText1<-c("   ","r","p")},
          "Power"={outputText1<-c("   ","w","nw")},
          "NHSTErrors"={outputText1<-c("   ","I","II")},
          "CILimits" ={outputText1<-c("   ","lower","upper")},
          "log(lr)"={outputText1<-c("   ","S","p")}
  )
  outputText<-c(outputText,rep(outputText1,nc/3))
  
  if (expectedType=="NHSTErrors"){
    e1=paste(format(mean(nullresult$pIV<alpha)*100,digits=report_precision),"%")
    e2=paste(format(mean(result$pIV>=alpha)*100,digits=report_precision),"%")
    outputText<-c(outputText,"",e1,e2)
    
  }else{
    
    ot1<-c()
    ot2<-c()
    ot4<-c()
    ot5<-c()
    ot6<-c()
    
    for (i in 1:(nc/3)) {
      r<-rs[,i]
      p<-ps[,i]
      switch (expectedType,
              "EffectSize"={
                a<-r
                b<-p
              },
              "Power"={
                a<-rn2w(r,result$nval)
                b<-rw2n(r,0.8)
              },
              "CILimits"={
                a<-r2ci(r,result$nval[1],-1)
                b<-r2ci(r,result$nval[1],+1)
              },
              "log(lr)"={
                a<-r2llr(r,result$nval)
                b<-p
              }
      )
      ot1<-c(ot1,
             "mean",
             format(mean(a,na.rm=TRUE),digits=report_precision),
             format(mean(b,na.rm=TRUE),digits=report_precision)
      )
      ot2<-c(ot2,
             "sd",
             format(sd(a,na.rm=TRUE),digits=report_precision),
             format(sd(b,na.rm=TRUE),digits=report_precision)
      )
      ot4<-c(ot4,
             "quant75",
             format(quantile(a,0.75,na.rm=TRUE),digits=report_precision),
             format(quantile(b,0.75,na.rm=TRUE),digits=report_precision)
      )
      ot5<-c(ot5,
             "median",
             format(quantile(a,0.5,na.rm=TRUE),digits=report_precision),
             format(quantile(b,0.5,na.rm=TRUE),digits=report_precision)
      )
      ot6<-c(ot6,
             "quant25",
             format(quantile(a,0.25,na.rm=TRUE),digits=report_precision),
             format(quantile(b,0.25,na.rm=TRUE),digits=report_precision)
      )
      if (i>1){
        ot1[length(ot1)-2]<-""
        ot2[length(ot1)-2]<-""
        ot4[length(ot1)-2]<-""
        ot5[length(ot1)-2]<-""
        ot6[length(ot1)-2]<-""
      }
    }
    outputText<-c(outputText,ot1,ot2,rep("  ",nc),ot4,ot5,ot6)
  }
  
  nr<-length(outputText)/nc
  reportPlot(outputText,nc,nr)        
  
}
