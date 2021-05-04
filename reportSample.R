reportSample<-function(IV,IV2,DV,design,result){
  
  s1<-result$iv
  s2<-result$dv
  
  nc=6
  outputText<-c("\bVariables","","","","","")
  
  outputTextI<-c()
  # Interval variables first
  done_interval<-FALSE
  if (IV$type=="Interval"){
    IV$sample_mu<-mean(s1)
    IV$sample_sd<-sd(s1,na.rm=TRUE)
    IV$sample_skew<-0
    IV$sample_kurtosis<-0
    outputTextI<-c(outputTextI,"",IV$name,
                   format(mean(s1,na.rm=TRUE),digits=report_precision),format(sd(s1,na.rm=TRUE),digits=report_precision),
                   format(skewness(s1,na.rm=TRUE),digits=report_precision),format(kurtosis(s1,na.rm=TRUE),digits=report_precision)
    )
    done_interval<-TRUE
  }
  if (no_ivs>1){
    s1a<-result$iv2
    if (IV2$type=="Interval"){
      IV2$sample_mu<-mean(s1a)
      IV2$sample_sd<-sd(s1a,na.rm=TRUE)
      outputTextI<-c(outputTextI,"",IV2$name,
                     format(mean(s1a),digits=report_precision),  format(sd(s1a),digits=report_precision),
                     format(skewness(s1a,na.rm=TRUE),digits=report_precision),format(kurtosis(s1a,na.rm=TRUE),digits=report_precision)
      )
      done_interval<-TRUE
    }
  }
  if (DV$type=="Interval"){
    DV$sample_mu<-mean(s2)
    DV$sample_sd<-sd(s2,na.rm=TRUE)
    outputTextI<-c(outputTextI,"",DV$name,
                   format(mean(s2),digits=report_precision),  format(sd(s2),digits=report_precision),
                   format(skewness(s2,na.rm=TRUE),digits=report_precision),format(kurtosis(s2,na.rm=TRUE),digits=report_precision)
    )
    done_interval<-TRUE
  }
  if (done_interval){
    outputText<-c(outputText,"","\bInterval","\bmean","\bsd","\bskew","\bkurtosis",outputTextI)
  }

  # Categorical variables
  outputTextC=c()
  done_categorical<-FALSE
  if (IV$type=="Categorical"){
    counts<-""
    for (i in 1:IV$ncats){
      counts<-paste(counts," ",  IV$cases[i],"=", sum(s1==IV$cases[i]),sep="")
      # counts<-paste(counts,sum(s1==IV$cases[i])," ")
    }
    outputTextC<-c(outputTextC,"",IV$name,counts,"","","")
    done_categorical<-TRUE
  }
  if (no_ivs>1){
    s1a<-result$iv2
    if (IV2$type=="Categorical"){
      counts<-""
      for (i in 1:IV2$ncats){
        counts<-paste(counts," ",  IV2$cases[i],"=", sum(s1a==IV2$cases[i]),sep="")
        # counts<-paste(counts,sum(s1==IV$cases[i])," ")
      }
      outputTextC<-c(outputTextC,"",IV2$name,counts,"","","")
      done_categorical<-TRUE
    }
  }
  if (DV$type=="Categorical"){
    counts<-""
    for (i in 1:DV$ncats){
      counts<-paste(counts," ",  DV$cases[i],"=", sum(s2==DV$cases[i]),sep="")
    }
    outputTextC<-c(outputTextC,"",DV$name,counts,"","","")
    done_categorical<-TRUE
  }
  if (done_categorical){
    outputText<-c(outputText,"","\bCategorical","\bcounts","","","",outputTextC)
  }
  
  outputText<-c(outputText,"  ","","","","","")
  outputText<-c(outputText,
                "\bDesign","","","","","",
                "","Sample Size: ",design$sN,"","","",
                "","Method: ",design$sMethod,"","","",
                "","Usage: ",design$sIV1Use,"","",""
  )
  nr=length(outputText)/nc
  reportPlot(outputText,nc,nr)        
  
}
