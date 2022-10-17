source("runLikelihood.R")

getMaxS<-function(S,kvals) {
  np<-length(kvals)
  if (S[1]==max(S)) {
    return(kvals[1])
  }
  if (S[np]==max(S)) {
    return(kvals[np])
  }
  return(approx(diff(S),(kvals[1:(np-1)]+kvals[2:np])/2,0)$y)
}

ZeroSamplingPDF<-function(z,sigma) {
  exp(-0.5*(z^2/sigma^2))/sqrt(2*pi*sigma^2)
}
ZeroSamplingCDF<-function(zcrit,sigma) {
  1-(pnorm(zcrit,0,sigma)-pnorm(-zcrit,0,sigma))
}


SingleSamplingPDF<-function(z,lambda,sigma) {
  exp(-0.5*((z-lambda)^2/sigma^2))/sqrt(2*pi*sigma^2)
}
SingleSamplingCDF<-function(zcrit,lambda,sigma) {
  1-(pnorm(zcrit,lambda,sigma)-pnorm(-zcrit,lambda,sigma))
}


UniformSamplingPDF<-function(z,lambda,sigma) {
  (1-tanh(z)^2)
}
UniformSamplingCDF<-function(zcrit,lambda,sigma) {
  1-(tanh(zcrit)-tanh(-zcrit))/2
}


GaussSamplingPDF<-function(z,lambda,sigma) {
  exp(-0.5*z^2/(sigma^2+lambda^2))/sqrt(2*pi*(sigma^2+lambda^2))
}
GaussSamplingCDF<-function(zcrit,lambda,sigma) {
  sigma<-sqrt(lambda^2+sigma^2)
  1-(pnorm(zcrit,0,sigma)-pnorm(-zcrit,0,sigma))
}


ExpSamplingPDF<-function(z,lambda,sigma) {
  lambda<-1/lambda
  0.25*(lambda*exp(-lambda*(z-sigma^2*lambda/2))*(1+erf((z-sigma^2*lambda)/sqrt(2)/sigma)) +
          lambda*exp(-lambda*(-z-sigma^2*lambda/2))*(1+erf((-z-sigma^2*lambda)/sqrt(2)/sigma)))
}
ExpSamplingCDF<-function(zcrit,lambda,sigma) {
  lambda<-1/lambda
  z<-zcrit
  p1<-0.25*(
    exp((lambda*sigma/sqrt(2))^2)*exp(z*lambda) * erfc(lambda*sigma/sqrt(2) + z/sigma/sqrt(2))
    - exp((lambda*sigma/sqrt(2))^2)/exp(z*lambda) * erfc(lambda*sigma/sqrt(2) - z/sigma/sqrt(2))
    + 2*erf(z/sigma/sqrt(2))
  )
  z<--zcrit
  p2<-0.25*(
    exp((lambda*sigma/sqrt(2))^2)*exp(z*lambda) * erfc(lambda*sigma/sqrt(2) + z/sigma/sqrt(2))
    - exp((lambda*sigma/sqrt(2))^2)/exp(z*lambda) * erfc(lambda*sigma/sqrt(2) - z/sigma/sqrt(2))
    + 2*erf(z/sigma/sqrt(2))
  )
  1-(p1-p2)
}

getLikelihood<-function(z,n,worldDistr,worldDistK,p_sig,nullP=0) {
  sigma<-1/sqrt(n-3)
  
  # get nulls ready first
  if (nullP>0) {
    nullLikelihoods<-SingleSamplingPDF(z,0,sigma)
    if (p_sig) {
      zcrit<-qnorm(1-alpha/2,0,sigma)
      gainNull<-SingleSamplingCDF(zcrit,0,sigma)
    } else {
      gainNull<-1
    }
  } else {
    nullLikelihoods<-0
    gainNull<-0
  } 
  gainMain<-1
  switch(worldDistr,
         "Single"={
           res<-c()
           for (i in 1:length(worldDistK)) {
             lambda<-worldDistK[i]
             if (p_sig) {
               zcrit<-qnorm(1-alpha/2,0,sigma)
               gainMain<-SingleSamplingCDF(zcrit,lambda,sigma)
             }
             mainLikelihoods<-SingleSamplingPDF(z,lambda,sigma)
             likelihoods<-(mainLikelihoods*(1-nullP)+nullLikelihoods*nullP)/(gainMain*(1-nullP)+gainNull*nullP)
             likelihoods[likelihoods<1e-300]<-1e-300
             res<-c(res,mean(log(likelihoods),na.rm=TRUE))
           }
         },
         "Uniform"={
           res<-c()
           for (i in 1:length(worldDistK)) {
             lambda<-worldDistK[i]
             if (p_sig) {
               zcrit<-qnorm(1-alpha/2,0,sigma)
               gainMain<-UniformSamplingCDF(zcrit,lambda,sigma)
             }
             mainLikelihoods<-UniformSamplingPDF(z,lambda,sigma)
             likelihoods<-(mainLikelihoods*(1-nullP)+nullLikelihoods*nullP)/(gainMain*(1-nullP)+gainNull*nullP)
             likelihoods[likelihoods<1e-300]<-1e-300
             res<-c(res,mean(log(likelihoods),na.rm=TRUE))
           }
         },
         "Gauss"={
           res<-c()
           for (i in 1:length(worldDistK)) {
             lambda<-worldDistK[i]
             if (p_sig) {
               zcrit<-qnorm(1-alpha/2,0,sigma)
               gainMain<-GaussSamplingCDF(zcrit,lambda,sigma)
             }
             mainLikelihoods<-GaussSamplingPDF(z,lambda,sigma)
             likelihoods<-(mainLikelihoods*(1-nullP)+nullLikelihoods*nullP)/(gainMain*(1-nullP)+gainNull*nullP)
             likelihoods[likelihoods<1e-300]<-1e-300
             res<-c(res,mean(log(likelihoods),na.rm=TRUE))
           }
         },
         "Exp"={
           res<-c()
           for (i in 1:length(worldDistK)) {
             lambda<-worldDistK[i]
             z<-abs(z)
             if (p_sig) {
               zcrit<-qnorm(1-alpha/2,0,sigma)
               gainMain<-ExpSamplingCDF(zcrit,lambda,sigma)
             }
             mainLikelihoods<-ExpSamplingPDF(z,lambda,sigma)
             likelihoods<-(mainLikelihoods*(1-nullP)+nullLikelihoods*nullP)/(gainMain*(1-nullP)+gainNull*nullP)
             likelihoods[likelihoods<1e-300]<-1e-300
             res<-c(res,mean(log(likelihoods),na.rm=TRUE))
           }
         }
  )
  res
}

# z<-seq(-100,100,length.out=1001)
# lambda<-rand(100,1)
# sigma<-rand(100,1)
# v<-c()
# for (i in 1:length(lambda)) { 
#   v<-c(v,sum(getLikelihood(z,1/sigma[i]^2+3,"Single",lambda[i]))*(z[2]-z[1]))
# }
# print(v)

# rs<-rand(10,1)-0.5
# zs<-atanh(rs)
# ns<-rand(10,1)*100+10
# kvals<-seq(0,1,length.out=21)*0.99
# S<-getLikelihood(zs,ns,"Exp",kvals)
# plot(kvals,S)
# kmax<-approx(diff(S),kvals[1:20]+kvals[2]-kvals[1],0)$y
# Smax<-approx(kvals,S,kmax)$y
# print(c(kmax,Smax))

findPmax<-function(zs,ns,distr,kvals,psigAnal,S) {
  nzp<-11
  pzvals<-seq(0,1,length.out=nzp)
  Nullmax<-0
  for (i in 1:nzp) {
    newS<-getLikelihood(zs,ns,distr,kvals,psigAnal,pzvals[i])
    if (max(newS,na.rm=TRUE)>max(S)) {
      S<-newS
      Nullmax<-pzvals[i]
    }
  }
  pzvals<-seq(-0.1,0.1,length.out=nzp)+Nullmax
  pzvals<-pzvals[pzvals>=0]
  pzvals<-pzvals[pzvals<=1]
  for (i in 1:length(pzvals)) {
    newS<-getLikelihood(zs,ns,distr,kvals,psigAnal,pzvals[i])
    if (max(newS)>max(S)) {
      S<-newS
      Nullmax<-pzvals[i]
    }
  }
  
  list(S=S,Nullmax=Nullmax)
}

runMetaAnalysis<-function(metaAnalysis,metaResult){
  rs<-metaResult$result$rIV
  zs<-atanh(rs)
  ns<-metaResult$result$nval
  np<-51
  
  
  if (metaAnalysis$meta_fixedAnal=="fixed") {
    kvals<-seq(-1,1,length.out=np)*0.95
    singleS<-getLikelihood(zs,ns,"Single",kvals,metaAnalysis$meta_psigAnal,0)
    singleKmax<-approx(diff(singleS),(kvals[1:(np-1)]+kvals[2:np])/2,0)$y
    singleSmax<-max(singleS,na.rm=TRUE)
    singleNullmax<-0
    
    bestDist<-"Single"
    bestK<-singleKmax
    bestNull<-0
    bestS<-singleSmax
    
    gaussKmax<-NA
    gaussSmax<-NA
    gaussNullmax<-NA
    expKmax<-NA
    expSmax<-NA
    expNullmax<-NA
    
  } else {
    
    # doing random effects analysis
    # find best Single
    kvals<-seq(-1,1,length.out=np)*0.95
    singleS<-getLikelihood(zs,ns,"Single",kvals,metaAnalysis$meta_psigAnal,0)
    singleNullmax<-0
    if (metaAnalysis$meta_nullAnal) {
      S<-findPmax(zs,ns,"Single",kvals,metaAnalysis$meta_psigAnal,singleS)
      singleS<-S$S
      singleNullmax<-S$Nullmax
    } 
    singleKmax<-getMaxS(singleS,kvals)
    singleSmax<-max(singleS,na.rm=TRUE)
    
    # find best Gauss
    kvals<-seq(0.01,1,length.out=np)
    gaussS<-getLikelihood(zs,ns,"Gauss",kvals,metaAnalysis$meta_psigAnal,0)
    gaussNullmax<-0
    if (metaAnalysis$meta_nullAnal) {
      S<-findPmax(zs,ns,"Gauss",kvals,metaAnalysis$meta_psigAnal,gaussS)
      gaussS<-S$S
      gaussNullmax<-S$Nullmax
    } 
    gaussKmax<-getMaxS(gaussS,kvals)
    gaussSmax<-max(gaussS,na.rm=TRUE)
    
    # find best Exp
    kvals<-seq(0.01,1,length.out=np)
    expS<-getLikelihood(zs,ns,"Exp",kvals,metaAnalysis$meta_psigAnal,0)
    expNullmax<-0
    if (metaAnalysis$meta_nullAnal) {
      S<-findPmax(zs,ns,"Exp",kvals,metaAnalysis$meta_psigAnal,expS)
      expS<-S$S
      expNullmax<-S$Nullmax
    } 
    expKmax<-getMaxS(expS,kvals)
    expSmax<-max(expS,na.rm=TRUE)
    
    use<-which.max(c(singleSmax,gaussSmax,expSmax))
    bestDist<-c("Single","Gauss","Exp")[use]
    bestK<-c(singleKmax,gaussKmax,expKmax)[use]
    bestNull<-c(singleNullmax,gaussNullmax,expNullmax)[use]
    bestS<-c(singleSmax,gaussSmax,expSmax)[use]
    
  }
  
  
  if (metaAnalysis$append) {
    bestDist<-c(metaResult$bestDist,bestDist)
    bestK<-c(metaResult$bestK,bestK)
    bestNull<-c(metaResult$bestNull,bestNull)
    bestS<-c(metaResult$bestS,bestS)
    singleKmax<-c(metaResult$single$kmax,singleKmax)
    singleSmax<-c(metaResult$single$Smax,singleSmax)
    singleNullmax<-c(metaResult$single$nullMax,singleNullmax)
    gaussKmax<-c(metaResult$gauss$kmax,gaussKmax)
    gaussSmax<-c(metaResult$gauss$Smax,gaussSmax)
    gaussNullmax<-c(metaResult$gauss$nullMax,gaussNullmax)
    expKmax<-c(metaResult$exp$kmax,expKmax)
    expSmax<-c(metaResult$exp$Smax,expSmax)
    expNullmax<-c(metaResult$exp$nullMax,expNullmax)
  }
  
  metaResult<-list(single=list(kmax=singleKmax,Smax=singleSmax,nullMax=singleNullmax),
                   gauss=list(kmax=gaussKmax,Smax=gaussSmax,nullMax=gaussNullmax),
                   exp=list(kmax=expKmax,Smax=expSmax,nullMax=expNullmax),
                   bestDist=bestDist,
                   bestK=bestK,
                   bestNull=bestNull,
                   bestS=bestS,
                   count=length(metaResult$bestDist)+1,
                   nsims=metaResult$nsims,
                   effect=effect,
                   design=design,
                   metaAnalysis=metaAnalysis,
                   result=metaResult$result
  )
  return(metaResult)
}



