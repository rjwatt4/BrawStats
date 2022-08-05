
checkSample<-function(IV,IV2,DV,effect,design,evidence,sample,result) {
  
  if (result$pIV<alpha) return(result)
  if (design$sCheating=="None") return(result)
  
  if (design$sCheating=="Retry") {
    ntrials<-0
    while (result$pIV>alpha && ntrials<design$sCheatingK) {
      sample<-makeSample(IV,IV2,DV,effect,design)
      result<-analyseSample(IV,IV2,DV,design,evidence,sample)
      ntrials<-ntrials+1
    }
    return(result)
  }
  
  
  if (design$sCheating=="Prune") {
    ntrials<-0
    while (result$pIV>alpha && ntrials<design$sCheatingK) {
      ps<-c()
      for (i in 1:length(sample$iv)) {
        sample1<-sample
        use<-rep(TRUE,length(sample1$iv))
        use[i]<-FALSE
        sample1$participant<-sample1$participant[use]
        sample1$iv<-sample1$iv[use]
        sample1$dv<-sample1$dv[use]
        design$sN<-length(sample1$iv)
        result1<-analyseSample(IV,IV2,DV,design,evidence,sample1)
        ps<-c(ps,result1$pIV)
      }
      keep<-ps>min(ps)
      sample$participant<-sample$participant[keep]
      sample$iv<-sample$iv[keep]
      sample$dv<-sample$dv[keep]
      sample$ivplot<-sample$ivplot[keep]
      sample$dvplot<-sample$dvplot[keep]
      
      result<-analyseSample(IV,IV2,DV,design,evidence,sample)
      ntrials<-ntrials+1
    }
    return(result)
  }
  
  
  design2<-design
  design2$sNRand<-FALSE
  
  effect2<-effect
  effect2$populationRZ<-NA
  effect2$rIV<-result$rpIV
  
  sample2<-makeSample(IV,IV2,DV,effect2,design2)
  
  if (design$sCheating=="Grow") {
    ntrials<-0
    while (result$pIV>alpha && ntrials<design$sCheatingK) {
      sample$participant<-c(sample$participant,length(sample$participant)+1)
      sample$iv<-c(sample$iv,sample2$iv[ntrials+1])
      sample$dv<-c(sample$dv,sample2$dv[ntrials+1])
      sample$ivplot<-c(sample$ivplot,sample2$ivplot[ntrials+1])
      sample$dvplot<-c(sample$dvplot,sample2$dvplot[ntrials+1])
      design$sN<-design$sN+1
      
      result<-analyseSample(IV,IV2,DV,design,evidence,sample)
      ntrials<-ntrials+1
    }
    return(result)
  }
  
  
  if (design$sCheating=="Replace") {
    ntrials<-0
    while (result$pIV>alpha && ntrials<design$sCheatingK) {
      ps<-c()
      for (i in 1:length(sample$iv)) {
        sample1<-sample
        use<-rep(TRUE,length(sample1$iv))
        use[i]<-FALSE
        sample1$participant<-sample1$participant[use]
        sample1$iv<-sample1$iv[use]
        sample1$dv<-sample1$dv[use]

        result1<-analyseSample(IV,IV2,DV,design,evidence,sample1)
        ps<-c(ps,result1$pIV)
      }
      change<-which.min(ps)
      sample$iv[change]<-sample2$iv[ntrials+1]
      sample$dv[change]<-sample2$dv[ntrials+1]
      sample$ivplot[change]<-sample2$ivplot[ntrials+1]
      sample$dvplot[change]<-sample2$dvplot[ntrials+1]
      
      result<-analyseSample(IV,IV2,DV,design,evidence,sample)
      ntrials<-ntrials+1
    }
    return(result)
  }
  
}

