model2effect<-function(mF,DVvarType,variable=NULL){
  switch (DVvarType,
          "Interval"=     {
            m2effect<-lm2effect
            predict.m<-predict.lm
            },
          "Ordinal"=      {
            m2effect<-lm2effect
            predict.m<-predict.lm
          },
          "Categorical"=  {
            m2effect<-glm2effect
            predict.m<-predict.glm
          },
  )
  r<-m2effect(mF,variable)
  if (is.null(variable)){
    r
  } else {
    # one IV ?
    if (is.null(mF$model$iv1)||is.null(mF$model$iv2)) {
      if (is.factor(mF$model[,variable])) {
        d<-data.frame(a=levels(mF$model[,variable]))
      } else {
        d<-data.frame(a=c(-1,1))
      }
      # this finds within designs
      if (!is.null(mF$model$participant)) {
        d$participant<-mF$model$participant[1]
      }
      names(d)[1]<-variable
      v<-predict.m(mF,d)
      r<-r*sign(mean(diff(v)))
    } else {
      # interaction here
      if (is.factor(mF$model$iv1)) {
        d1<-levels(mF$model$iv1)
      } else {
        d1<-c(-1,1)
      }
      if (is.factor(mF$model$iv2)) {
        d2<-levels(mF$model$iv2)
      } else {
        d2<-c(-1,1)
      }
      
      w1<-seq(-1,1,length.out=length(d1))
      w2<-seq(-1,1,length.out=length(d2))
      w<-meshgrid(w1,w2)
      w<-w$X*w$Y
      
      da<-expand.grid(d1,d2)
      d<-data.frame(iv1=da$Var1,iv2=da$Var2)
      # this finds within designs
      if (!is.null(mF$model$participant)) {
        d$participant<-mF$model$participant[1]
      }
      v<-predict.m(mF,d)
      v<-matrix(v,nrow=length(d1),ncol=length(d2))
      
      r<-r*sign(cor(c(v),c(w)))
    }
    r
  }
}

lm2effect<-function(lmNorm,variable){
  an<-anova(lmNorm)
  ssq<-an[["Sum Sq"]]
  if (!is.null(variable)) {
    use<-is.element(rownames(an),variable)
  } else {
    use<-!is.element(rownames(an),c("participant","Residuals"))
  }
  sqrt(sum(ssq[use])/sum(ssq))
}

glm2effect<-function(glmNorm,variable) {
  an<-anova(glmNorm)
  df<-an$`Resid. Df`[1]
  if (!is.null(variable)) {
    use<-is.element(rownames(an),variable)
  } else {
    use<-!is.element(rownames(an),c("participant","Residuals","NULL"))
  }
  dev<-an$Deviance[use]
  sqrt(sum(dev)/df)  
}

signEffect<-function(mF,variable){
  print(mF)
  nm<-names(mF$coefficients)
  switch (variable,
          "iv1"= {
            use<-grepl("iv1[^:]{1}",nm) | grepl("iv1$",nm)
            if (is.factor(mF$model$iv1)) {use<-c(which(use),1)}
          },
          "iv2"= {
            use<-grepl("iv2[^:]{1}",nm) | grepl("iv2$",nm)
            if (is.factor(mF$model$iv2)) {use<-c(which(use),1)}
          },
          "iv1:iv2"={
            use<-grepl("iv1[^:]*:iv2[^:]*",nm)
          }
  )
  
  cc<-mF$coefficients[use]
  if (length(cc)>1) {
    sign(mean(diff(cc),na.rm=TRUE))
  } else {
    sign(cc)
  }
}

wsd<-function(x,w=1) {
  if (length(w)==1) {
    sqrt(mean((x-mean(x))^2))
  } else {
  sqrt(sum((x-mean(x))^2 *w)/sum(w))
  }
}

model2directeffect<-function(mF){

  v1<-mF$model[,2]
  v2<-mF$model[,3]
  v3<-mF$model[,1]
  cat_vars<-c(!is.numeric(v1),!is.numeric(v2),!is.numeric(v3))
  
  switch (cat_vars[3]+1,
          {DVgain<-sd(mF$model$dv)},
          {DVgain<-sd(mF$fitted.values+mF$residuals)}
          )

  if (cat_vars[1]){
    n1<-length(levels(v1))
    h1<-as.numeric(prop.table(table(v1)))
  } else {
    m1<-mean(v1,na.rm=TRUE)
  }
  if (cat_vars[2]){
    n2<-length(levels(v2))
    h2<-as.numeric(prop.table(table(v2)))
  } else {
    m2<-mean(v2,na.rm=TRUE)
  }
  if (cat_vars[3]){
    n3<-length(levels(v3))
    h3<-as.numeric(prop.table(table(v3)))
  } else {
    m3<-mean(v3,na.rm=TRUE)
  }
  
  ncoeff<-c()
  
  # if (!cat_vars[3]) { # begin of Interval DV
    if (1==1) {
      # for Interval DV we find effect-size by 
    # looking at the change in prediction around the centre of an interval predictor
    # looking at the sd of predictions for all possible cases of categorical predictors
  # interval + interval
  if (!cat_vars[1] && !cat_vars[2])  {
    
    ncoeff1<-predict.lm(mF,data.frame(iv1=m1+0.5,iv2=m2))-predict.lm(mF,data.frame(iv1=m1-0.5,iv2=m2))
    ncoeff<-c(ncoeff,ncoeff1*sd(v2))
    
    ncoeff2<-predict.lm(mF,data.frame(iv1=m1,iv2=m2+0.5))-predict.lm(mF,data.frame(iv1=m1,iv2=m2-0.5))
    ncoeff<-c(ncoeff,ncoeff2*sd(v1))
    
    ncoeff3=predict.lm(mF,data.frame(iv1=m1+0.5,iv2=m2+0.5))+
            predict.lm(mF,data.frame(iv1=m1-0.5,iv2=m2-0.5))-
            predict.lm(mF,data.frame(iv1=m1-0.5,iv2=m2+0.5))-
            predict.lm(mF,data.frame(iv1=m1+0.5,iv2=m2-0.5))
    ncoeff<-c(ncoeff,ncoeff3*sd(v1*v2))
    
    return(ncoeff/DVgain)
  }
  
  # interval + categorical
  if (!cat_vars[1] && cat_vars[2])  {
    ncoeff1<-c()
    for (i in 1:n2) {
      v<-predict.lm(mF,data.frame(iv1=m1+0.5,iv2=levels(mF$model$iv2)[i]))-predict.lm(mF,data.frame(iv1=m1-0.5,iv2=levels(mF$model$iv2)[i]))
      ncoeff1<-c(ncoeff1,v)
    }
    nc<-sum(ncoeff1*h2)*sd(v1)
    ncoeff<-c(ncoeff,nc)
    
    ncoeff2<-c()
    for (i in 1:n2) {
      ncoeff2<-c(ncoeff2,predict.lm(mF,data.frame(iv1=m1,iv2=levels(mF$model$iv2)[i])))
    }
    nc<-wsd(ncoeff2,h2) * sign(sum(diff(ncoeff2)))
    ncoeff<-c(ncoeff, nc)
    
    nc<-wsd(ncoeff1,h1)*sign(sum(diff(ncoeff1)))*sd(v1)
    ncoeff<-c(ncoeff, nc)
    
    return(ncoeff/DVgain)
    
  }
  
  # categorical + interval
  if (cat_vars[1] && !cat_vars[2])  {
    
    ncoeff1<-c()
    for (i in 1:n1) {
      ncoeff1<-c(ncoeff1,predict.lm(mF,data.frame(iv1=levels(mF$model$iv1)[i],iv2=m2)))
    }
    nc<-wsd(ncoeff1,h1) * sign(sum(diff(ncoeff1)))
    ncoeff<-c(ncoeff, nc)

    ncoeff2<-c()
    for (i in 1:n1) {
      v<-predict.lm(mF,data.frame(iv1=levels(mF$model$iv1)[i],iv2=m2+0.5))-predict.lm(mF,data.frame(iv1=levels(mF$model$iv1)[i],iv2=m2-0.5))
      ncoeff2<-c(ncoeff2,v)
    }
    nc<-sum(ncoeff2*h1)*sd(v2)
    ncoeff<-c(ncoeff,nc)

    nc<-wsd(ncoeff2,h1)*sign(sum(diff(ncoeff2)))*sd(v2)
    ncoeff<-c(ncoeff, nc)
    
    return(ncoeff/DVgain)
    
  }
  
  # categorical + categorical
  if (cat_vars[1] && cat_vars[2])  {
    lv1<-levels(mF$model$iv1)
    lv2<-levels(mF$model$iv2)
    i<-meshgrid(1:n1,1:n2)
    v<-predict.lm(mF,data.frame(iv1=as.vector(lv1[i$X]),iv2=as.vector(lv2[i$Y])))
    dim(v)<-c(n1,n2)
    es1<-colMeans(v)
    es2<-rowMeans(v)
    
    nc<-wsd(es1,h1) * sign(sum(diff(es1)))
    ncoeff<-c(ncoeff, nc)
    
    nc<-wsd(es2,h2) * sign(sum(diff(es2)))
    ncoeff<-c(ncoeff, nc)
    
    ie<-t(t(v)-es1)-es2
    nc<-wsd(ie,1)*sign(sum(diff(-ie[1,])))
    ncoeff<-c(ncoeff, nc)
    
    return(ncoeff/DVgain)
    
  }
  } # end of Interval DV
  
  for (i in 1:3) {
    switch (i,
            variable<-"iv1",
            variable<-"iv2",
            variable<-"iv1:iv2")
    
  # old code
  n<-names(mF$coefficients)
  switch (variable,
          "iv1"={
            m<-mF$model[,variable]
            if (is.numeric(m)) { 
              vals<-m*as.numeric(mF$coefficients[variable]) 
              } else { 
              lv1<-levels(m) 
              vals<-0
              for (i in 2:length(lv1)){
                vals<-vals+(m==lv1[i])*as.numeric(mF$coefficients[paste(variable,lv1[i],sep="")])
              }
              m<-as.numeric(m)
              }
          },
          "iv2"={
            m<-mF$model[,variable]
            if (is.numeric(m)) { 
              vals<-m*as.numeric(mF$coefficients[variable])
            } else { 
              lv1<-levels(m) 
              vals<-0
              for (i in 2:length(lv1)){
                vals<-vals+(m==lv1[i])*as.numeric(mF$coefficients[paste(variable,lv1[i],sep="")])
              }
              m<-as.numeric(m)
            }
          },
          "iv1:iv2"={
            m1<-mF$model[,"iv1"]
            m2<-mF$model[,"iv2"]

            if (is.numeric(m1) && is.numeric(m2)){
              vals<-m1*m2*mF$coefficients["iv1:iv2"]
            }
            
            if (!is.numeric(m1) && is.numeric(m2)){
              lv1<-levels(m1) 
              vals<-0
              for (i1 in 2:length(lv1)){
                use<-m1==lv1[i1]
                vals<-vals+use*m2*as.numeric(mF$coefficients[paste("iv1", lv1[i1], ":iv2",sep="")])
              }
            }
            
            if (is.numeric(m1) && !is.numeric(m2)){
              lv2<-levels(m2) 
              vals<-0
              for (i2 in 2:length(lv2)){
                use<-m2==lv2[i2]
                vals<-vals+use*m1*as.numeric(mF$coefficients[paste("iv1", ":iv2", lv2[i2], sep="")])
              }
            }
            if (!is.numeric(m1) && !is.numeric(m2)){
              lv1<-levels(m1) 
              lv2<-levels(m2) 
              vals<-0
              for (i1 in 2:length(lv1)){
                use1<-m1==lv1[i1]
                for (i2 in 2:length(lv2)){
                  use2<-m2==lv2[i2]
                  vals<-vals+use1*use2*as.numeric(mF$coefficients[paste("iv1", lv1[i1], ":iv2",lv2[i2], sep="")])
                }
              }
            }
            m<-as.numeric(m1)*as.numeric(m2)
          }
  )
  r<-sd(vals)/DVgain*sign(cor(vals,m))
  ncoeff<-c(ncoeff,r)
  }
  return(ncoeff)
}

r2p<-function(r,n){
  if (any(abs(r)>1)) {
    print(paste("r2p r-exception",format(max(r),digits=3)))
    r[r>1]<-1
  }
  if (any(abs(n)<3)) {
    print("r2p n-exception")
    n[n<3]<-3
  }
  t_vals<-r/r2se(r,n)
  (1-pt(abs(t_vals),n-2))*2
  
}

r2se<-function(r,n){
  if (any(abs(r)>1)) {
    print("r2se r-exception")
    r[abs(r)>1]<-1
  }
  if (any(abs(n)<3)) {
    print("r2se n-exception")
    n[n<3]<-3
  }
  sqrt((1-r^2)/(n-2))
}

r2ci<-function(r,n,s=0){
  if (any(abs(r)>1)) {
    print("r2ci r-exception")
    r[abs(r)>1]<-1
  }
  if (any(abs(n)<3)) {
    print("r2ci n-exception")
    n[n<3]<-3
  }
  z<-atanh(r)
  zci<-qnorm(1-0.05/2)*sqrt(1/(n-3))
  if (s==0){
    tanh(z+c(-1,1)*zci)
  } else {
    tanh(z+s*zci)
  }
}

multipleAnalysis<-function(IV,IV2,DV,effect,design,evidence,n_sims,appendData=FALSE, earlierResult=c(), showProgress=TRUE){
  rho<-effect$rIV
  rho2<-effect$rIV2
  
  pvals=c()
  rvals=c()
  nvals=c()
  if (length(rho)<n_sims) {rho<-rep(rho,n_sims)}
  if (!is.null(IV2)) {
    if (length(rho2)<n_sims) {rho2<-rep(rho2,n_sims)}
  }

  if (appendData) {
    main_res<-earlierResult
  } else {
    main_res<-list(rIV=c(),pIV=c(),rIV2=c(),pIV2=c(),rIVIV2DV=c(),pIVIV2DV=c(),nval=c(),r=list(direct=c(),unique=c(),total=c(),coefficients=c()),showType=design$showType)
  }
  
  for (i in 1:n_sims){
    if (showProgress && (n_sims<=50 || (n_sims>50 && i==round(i/25)*25))) {
      showNotification(paste(format(i),"/",format(n_sims)),id="counting",duration=Inf,closeButton=FALSE,type="message")
    } 
    effect$rIV<-rho[i]
    if (!is.null(IV2)) {effect$rIV2<-rho2[i]}
    
    result<-makeSample(IV,IV2,DV,effect,design)
    res<-analyseSample(IV,IV2,DV,design,evidence,result)
    
    if (is.na(res$rIV)) {
      res$rIV<-0
      res$pIV<-1
      res$nval<-0}
    main_res$rIV<-rbind(main_res$rIV,res$rIV)
    main_res$pIV<-rbind(main_res$pIV,res$pIV)
    main_res$nval<-rbind(main_res$nval,res$nval)

    if (!is.null(IV2)){
      main_res$rIV2<-rbind(main_res$rIV2,res$rIV2)
      main_res$pIV2<-rbind(main_res$pIV2,res$pIV2)
      main_res$rIVIV2DV<-rbind(main_res$rIVIV2DV,res$rIVIV2DV)
      main_res$pIVIV2DV<-rbind(main_res$pIVIV2DV,res$pIVIV2DV)
      
      main_res$r$direct<-rbind(main_res$r$direct,res$r$direct)
      main_res$r$unique<-rbind(main_res$r$unique,res$r$unique)
      main_res$r$total<-rbind(main_res$r$total,res$r$total)
      main_res$r$coefficients<-rbind(main_res$r$coefficients,as.double(res$uModel$coefficients)[2:length(res$uModel$coefficients)])
      
      main_res$p$direct<-rbind(main_res$p$direct,res$p$direct)
      main_res$p$unique<-rbind(main_res$p$unique,res$p$unique)
      main_res$p$total<-rbind(main_res$p$total,res$p$total)
      
    } else {
      main_res$rIV2<-rbind(main_res$rIV2,NA)
      main_res$pIV2<-rbind(main_res$pIV2,NA)
      main_res$rIVIV2DV<-rbind(main_res$rIVIV2DV,NA)
      main_res$pIVIV2DV<-rbind(main_res$pIVIV2DV,NA)
      
      main_res$r$direct<-rbind(main_res$r$direct,res$rIV)
      main_res$r$unique<-rbind(main_res$r$unique,res$rIV)
      main_res$r$total<-rbind(main_res$r$total,res$rIV)
      main_res$r$coefficients<-rbind(main_res$r$coefficients,as.double(res$uModel$coefficients)[2:length(res$uModel$coefficients)])
      
      main_res$p$direct<-rbind(main_res$p$direct,res$pIV)
      main_res$p$unique<-rbind(main_res$p$unique,res$pIV)
      main_res$p$total<-rbind(main_res$p$total,res$pIV)
      
    }
  }
  if (showProgress) removeNotification(id="counting")
  main_res$showType<-evidence$showType
  main_res
}

convert2Interval<-function(var) {
  var$type<-"Interval"
  var$mu<-var$median
  var$sd<-var$iqr*qnorm(0.75)
}

analyseSample<-function(IV,IV2,DV,design,evidence,result){

  if (is.null(IV2)) {no_ivs<-1} else {no_ivs<-2}
  if (IV$type=="Ordinal") {
    IV$type<-"Interval"
    }
  if (!is.null(IV2) && IV2$type=="Ordinal") {
    IV2$type<-"Interval"
    }
  if (!is.null(IV2) && DV$type=="Ordinal") {
    DV$type<-"Interval"
    }
  
  # collect the data
  
  # remove duplicated rows (from covariates of within designs)
  # if (is.null(IV2)){
  #   waste<-duplicated(data.frame(pt=result$participant,iv=result$iv,dv=result$dvplot))
  #   iv1<-result$iv[!waste]
  #   iv2<-result$iv2
  #   dv<-result$dv[!waste]
  # } else {
  #   waste<-duplicated(data.frame(pt=result$participant,iv=result$iv,iv2=result$iv2,dv=result$dvplot))
  #   iv1<-result$iv[!waste]
  #   iv2<-result$iv2[!waste]
  #   dv<-result$dv[!waste]
  # }
  
  iv1<-result$iv
  iv2<-result$iv2
  dv<-result$dv
  
  if (is.factor(iv1) && all(iv1==iv1[1])) {
    result$rIV<-NA
    result$pIV<-NA
    return(result)
    }
  # if (is.factor(iv2) && all(iv2==iv2[1])) {return(NA)}
  
  n<-length(dv)
  
  resultRawData<-data.frame(participant=result$participant,iv1=iv1,iv2=iv2,dv=dv)
  # normalize variables
  # if (IV$type=="Interval")  iv1=(iv1-mean(iv1,na.rm=TRUE))/sd(iv1,na.rm=TRUE)
  # if (!is.null(IV2) && IV2$type=="Interval") iv2=(iv2-mean(iv2,na.rm=TRUE))/sd(iv2,na.rm=TRUE)
  # if (DV$type=="Interval")  dv=(dv-mean(dv,na.rm=TRUE))/sd(dv,na.rm=TRUE)
  if (IV$type=="Interval")  iv1=(iv1-mean(iv1,na.rm=TRUE))
  if (!is.null(IV2) && IV2$type=="Interval") iv2=(iv2-mean(iv2,na.rm=TRUE))
  if (DV$type=="Interval")  dv=(dv-mean(dv,na.rm=TRUE))
  # make data frame
  resultNormData<-data.frame(participant=result$participant,iv1=iv1,iv2=iv2,dv=dv)
  
  # get cases sorted
  if (IV$type=="Categorical"){
    switch (evidence$evidenceCaseOrder,
            "Alphabetic"={ref=1},
            "AsFound"={ref=as.numeric(iv1[1])},
            "Frequency"={ref=which.max(tabulate(match(iv1, IV$cases)))}
    )
    resultNormData$iv1<-relevel(resultNormData$iv1,ref=ref)
  }
  if (!is.null(IV2) && IV2$type=="Categorical"){
    switch (evidence$evidenceCaseOrder,
            "Alphabetic"={ref=1},
            "AsFound"={ref=as.numeric(iv2[1])},
            "Frequency"={ref=which.max(tabulate(match(iv2, IV2$cases)))}
    )
    resultNormData$iv2<-relevel(resultNormData$iv2,ref=ref)
  }
  if (DV$type=="Categorical"){
    switch (evidence$evidenceCaseOrder,
            "Alphabetic"={ref=1},
            "AsFound"={ref=as.numeric(dv[1])},
            "Frequency"={ref=which.max(tabulate(match(dv, DV$cases)))}
    )
    resultNormData$dv<-relevel(resultNormData$dv,ref=ref)
  }
  
  # create formula
  formula<-"dv~iv1"
  if (!is.null(IV2)) {
    formula<-paste(formula,"+iv2",sep="")
    if (evidence$rInteractionOn==1) formula<-paste(formula,"+iv1*iv2",sep="")
  }
  if (design$sIV1Use=="Within" || design$sIV2Use=="Within"){
    doingWithin<-TRUE
    formula<-paste(formula,"+participant",sep="")
  } else {
    doingWithin<-FALSE
  }
  formula<-as.formula(formula)    

  if (IV$type=="Categorical") c1=TRUE else c1=FALSE
  if (!is.null(IV2) && IV2$type=="Categorical") c2=TRUE else c2=FALSE
  if (c1 && c2)       {contrasts<-list(iv1=contr.sum, iv2=contr.sum)}
  else { if (c1)      {contrasts<-list(iv1=contr.sum)}
        else {if (c2) {contrasts<-list(iv2=contr.sum)}
                else  {contrasts<-c()}
        }
  }

    # get linear model and anova
  switch (DV$type,
          "Interval"={
            lmRaw<-lm(formula=formula,data=resultRawData)
            if (!is.null(IV2)){
              if (c1 || c2) {
                lmRaw3<-lm(formula=formula,data=resultRawData,contrasts=contrasts)
              } else {
                lmRaw3<-lmRaw
              }
            } else{ lmRaw3<-lmRaw}
            if (is.null(IV2)) {
              lmNorm<-lm(formula=formula,data=resultNormData)
            } else {
            lmNorm<-lm(formula=formula,data=resultNormData,contrasts=contrasts)
            }
            testMethod<-"F"
            testMethod3<-"F"
            pcol=4;prow=2;
          },
          "Ordinal"={
            lmRaw<-lm(formula=formula,data=resultRawData)
            if (!is.null(IV2)){
              if (c1 || c2) {
                lmRaw3<-lm(formula=formula,data=resultRawData,contrasts=contrasts)
              } else {
                lmRaw3<-lmRaw
              }
            } else{ lmRaw3<-lmRaw}
            lmNorm<-lm(formula=formula,data=resultNormData)
            testMethod<-"F"
            testMethod3<-"F"
            pcol=4;prow=2;
          },
          "Categorical"={
            lmRaw<-glm(formula=formula,data=resultRawData,family="binomial")
            if (!is.null(IV2)){
              if (c1 || c2) {
                lmRaw3<-glm(formula=formula,data=resultRawData,family="binomial",contrasts=contrasts)
              } else {
                lmRaw3<-lmRaw
              }
            } else{ lmRaw3<-lmRaw}
            lmNorm<-glm(formula=formula,data=resultNormData,family="binomial")
            testMethod<-"Chisq"
            testMethod3<-"Wald"
            pcol=3;prow=2
          }
  )
  
  switch (evidence$ssqType,
          "Type1"={an<-Anova(lmRaw,test=testMethod3)},
          "Type2"={an<-Anova(lmRaw,test=testMethod3,type=2)},
          "Type3"={an<-Anova(lmRaw3,test=testMethod3,type=3,singular.ok=TRUE)},
          "Type3w"={an<-Anova(lmRaw,test=testMethod3,type=3,singular.ok=TRUE)}
          )

  # overall model effect-size
  result$rFull<- model2effect(lmNorm,DV$type)
  result$rFullse<-r2se(result$rFull,n)

  switch (DV$type,
          "Interval"={Df<-sum(an$Df)},
          "Ordinal"={Df<-sum(an$Df)},
          "Categorical"={Df<-an$`Resid. Df`[1]}
  )
  switch (no_ivs,
          { 
            result$rIV<-model2effect(lmNorm,DV$type,"iv1")
            result$pIV<-an[prow,pcol]
            rCI<-r2ci(result$rIV,n)
          },
          # 2 ivs
          { if (doingWithin) {
            result$rIV<-model2effect(lmNorm,DV$type,"iv1")
            result$pIV<-r2p(result$rIV,Df+1)
            result$rIV2<-model2effect(lmNorm,DV$type,"iv2")
            result$pIV2<-r2p(result$rIV2,Df+1)
            result$rIVIV2DV<-model2effect(lmNorm,DV$type,"iv1:iv2")
            result$pIVIV2DV<-r2p(result$rIVIV2DV,Df+1)
            
            result$rIVIV2<-0
            
            r.direct<-c(result$rIV,result$rIV2,result$rIVIV2DV)
            r.unique<-r.direct
            r.total<-r.direct
            
          } else {
            # 1. direct effect sizes for individual IVs
            #  IV first
            direct_effects<-model2directeffect(lmNorm);
            result$rIV<-direct_effects[1]
            result$pIV<-r2p(result$rIV,n)
            #  IV2 next    
            result$rIV2<-direct_effects[2]
            result$pIV2<-r2p(result$rIV2,n)
            #  interaction term
            if (evidence$rInteractionOn==1) {
              result$rIVIV2DV<-direct_effects[3]
              result$pIVIV2DV<-r2p(result$rIVIV2DV,n)
            } else {
              result$rIVIV2DV<-NA
              result$pIVIV2DV<-NA
            }
            #  find the covariation
            r12<-result
            r12$dv<-result$iv2
            r12<-analyseSample(IV,NULL,IV2,design,evidence,r12)
            result$rIVIV2<-r12$rIV
            
            # 2. find the unique and total effects
            # total model: with the single term
            # plain model: no interaction
            totalSD<-sd(lmNorm$fitted.values+lmNorm$residuals)
            switch (DV$type,
                    "Interval"={
                      # total effect sizes
                      lm1total<-lm(formula=dv~iv1,data=resultNormData)
                      lm2total<-lm(formula=dv~iv2,data=resultNormData)
                      lm12total<-lm(formula=dv~iv1:iv2,data=resultNormData)
                      a3<-lm(formula=dv~iv1+iv2,data=resultNormData)
                      anUnique<-Anova(lmNorm,test.statistic = "F")
                    },
                    "Categorical"={
                      lm1total<-glm(formula=dv~iv1,data=resultNormData,family="binomial")
                      lm2total<-glm(formula=dv~iv2,data=resultNormData,family="binomial")
                      lm12total<-glm(formula=dv~iv1:iv2,data=resultNormData,family="binomial")
                      a3<-glm(formula=dv~iv1+iv2,data=resultNormData,family="binomial")
                      anUnique<-Anova(lmNorm,test.statistic = "LR")
                    }
            )
            
            rIV1total<-model2effect(lm1total,DV$type,"iv1")
            rIV2total<-model2effect(lm2total,DV$type,"iv2")
            rIVIV2total<-model2effect(lm12total,DV$type,"iv1:iv2")
            
            if (is.factor(iv1) && is.factor(iv2)) {
              c<-lm12total$coefficients[1+(1:length(levels(lm12total$model$iv1)))]
              c <- -c
              rsign<-sign(cor(c,1:length(c)))
              rIVIV2total<-abs(rIVIV2total)*rsign
            }
            if (is.factor(iv1) && !is.factor(iv2)) {
              c<-lm12total$coefficients[1+(1:length(levels(lm12total$model$iv1)))]
              rsign<-sign(cor(c,1:length(c)))
              rIVIV2total<-abs(rIVIV2total)*rsign
            }
            if (!is.factor(iv1) && is.factor(iv2)) {
              c<-lm12total$coefficients[1+(1:length(levels(lm12total$model$iv2)))]
              rsign<-sign(cor(c,1:length(c)))
              rIVIV2total<-abs(rIVIV2total)*rsign
            }
            
            # get the unique effects
            # model without iv1:iv2
            # fitted values due to iv1:iv2
            a3d<-lmNorm$fitted.values-a3$fitted.values
            a3c<-cor(as.numeric(iv1)*as.numeric(iv2),a3d)

            # model without iv1
            # fitted values due to iv1
            a1d<-a3$fitted.values-lm2total$fitted.values
            a1c<-cor(as.numeric(iv1),a1d)

            # model without iv2
            # fitted values due to iv2
            a2d<-a3$fitted.values-lm1total$fitted.values
            a2c<-cor(as.numeric(iv2),a2d)

            # unique effect sizes
            if (is.factor(dv)) {
              rIV1unique<-sqrt(anUnique["iv1",1]/n) * sign(a1c)
              rIV2unique<-sqrt(anUnique["iv2",1]/n) * sign(a2c)
              rIVIV2unique<-sqrt(anUnique["iv1:iv2",1]/n) * sign(a3c)
            } else {
              rIV1unique<-sqrt(anUnique["iv1",1]/sum(anUnique[,1])) * sign(a1c)
              rIV2unique<-sqrt(anUnique["iv2",1]/sum(anUnique[,1])) * sign(a2c)
              rIVIV2unique<-sqrt(anUnique["iv1:iv2",1]/sum(anUnique[,1])) * sign(a3c)
            }

            r.direct<-c(result$rIV,result$rIV2,result$rIVIV2DV)
            r.unique<-c(rIV1unique,rIV2unique,rIVIV2unique)
            r.total<-c(rIV1total,rIV2total,rIVIV2total)
          }
          }
  )

  if (is.null(IV2)) {
    hypothesisType=paste(IV$type,DV$type,sep=" ")
    switch (hypothesisType,
            "Interval Interval"={
              an_name<-"Pearson Correlation"
              t_name<-"r"
              df<-paste("(",format(an$Df[nrow(an)]),")",sep="")
              tval<-result$rIV
            },
            "Ordinal Interval"={
              an_name<-"Pearson Correlation"
              t_name<-"r"
              df<-paste("(",format(an$Df[nrow(an)]),")",sep="")
              tval<-result$rIV
            },
            "Categorical Interval"={
              if (IV$ncats==2){
                if (design$sIV1Use=="Within"){
                  an_name<-"t-test: Paired Samples"
                  df<-paste("(",format(an$Df[nrow(an)]),")")
                  tv<-t.test(dv~iv1,paired=TRUE,var.equal=!evidence$Welch)
                  tval<-tv$statistic
                  result$pIV<-tv$p.value
                } else {
                  an_name<-"t-test: Independent Samples"
                  df<-paste("(",format(an$Df[nrow(an)]),")")
                  if (any(c(sum(iv1==levels(iv1)[1]),sum(iv1==levels(iv1)[2]))<3))
                  {             
                    tval<-0
                    result$pIV<-1
                  } else {
                  tv<-t.test(dv~iv1,var.equal=!evidence$Welch)
                  tval<-tv$statistic
                  result$pIV<-tv$p.value
                  }
                }
                t_name<-"t"
                tval<-sqrt(an$`F value`[2])*sign(result$rIV)
              } else {
                if (design$sIV1Use=="Within"){
                  an_name<-"One-Way ANOVA: Repeated Measures"
                } else {
                  an_name<-"One-Way ANOVA: Independent Measures"
                }
                t_name<-"F"
                df<-paste("(",format(an$Df[2]),",",format(an$Df[nrow(an)]),")",sep="")
                tval<-an$`F value`[2]
              }
            },
            "Interval Ordinal"={
              an_name<-"Spearman Correlation"
              t_name<-"rho"
              df<-paste("(",format(an$Df[nrow(an)]),")",sep="")
              op <- options(warn = (-1))
              tv<-cor.test(iv1, dv, method="spearman")
              options(op)
              tval<-tv$estimate
              result$pIV<-tv$p.value
            },
            "Ordinal Ordinal"={
              an_name<-"Spearman Correlation"
              t_name<-"rho"
              df<-paste("(",format(an$Df[nrow(an)]),")",sep="")
              op <- options(warn = (-1))
              tv<-cor.test(iv1, dv, method="spearman")
              options(op)
              tval<-tv$estimate
              result$pIV<-tv$p.value
            },
            "Categorical Ordinal"={
              if (IV$ncats==2){
                if (design$sIV1Use=="Within"){
                  an_name<-"Wilcoxon signed-rank Test: Paired Samples"
                  df<-paste("(",format(an$Df[nrow(an)]),")")
                  t_name<-"T"
                  op <- options(warn = (-1))
                  tv<-wilcox.test(dv~iv1,paired=TRUE,exact=FALSE)
                  options(op)
                  tval<-tv$statistic
                  result$pIV<-tv$p.value
                } else {
                  an_name<-"Mann Whitney U test: Independent Samples"
                  df<-paste("(",format(an$Df[nrow(an)]),")")
                  t_name<-"U"
                  op <- options(warn = (-1))
                  tv<-wilcox.test(dv~iv1,exact=FALSE)
                  options(op)
                  tval<-tv$statistic
                  result$pIV<-tv$p.value
                }
              } else {
                if (design$sIV1Use=="Within"){
                  an_name<-"Friedman Test: Repeated Measures"
                  op <- options(warn = (-1))
                  tv<-friedman(dv~iv1);
                  options(op)
                  t_name='chi2'
                  tval<-tv$statistic
                  result$pIV<-tv$p.value
                } else {
                  an_name<-"Kruskal Wallace Test: Independent Measures"
                  op <- options(warn = (-1))
                  tv<-kruskal.test(dv~iv1);
                  options(op)
                  t_name='chi2'
                  tval<-tv$statistic
                  result$pIV<-tv$p.value
                }
                df<-paste("(",format(an$Df[2]),",n=",format(length(dv)),")",sep="")
              }
            },
            "Interval Categorical"={
              an_name<-"Logistic Regression"
              t_name<-"chi2"
              df<-paste("(",format(an$Df[2]),",","n=",format(lmNorm$df.null+1),")",sep="")
              tval<-an$Chisq[2]
            },
            "Ordinal Categorical"={
              an_name<-"Logistic Regression"
              t_name<-"chi2"
              df<-paste("(",format(an$Df[2]),",","n=",format(lmNorm$df.null+1),")",sep="")
              tval<-an$Chisq[2]
            },
            "Categorical Categorical"={
              an_name<-"Chi-square test of independence"
              t_name<-"chi2"
              df<-paste("(",format(an$Df[2]),",","n=",format(lmNorm$df.null+1),")",sep="")
              
              chiResult<-chisq.test(iv1,dv,correct = FALSE)
              result$rIV<-sqrt(chiResult$statistic/n)*sign(result$rIV)
              result$pIV<-chiResult$p.value
              result$rFull<-result$rIV
              result$rFullse<-r2se(result$rFull,n)
              tval<-chiResult$statistic
            }
    )
  } else {
    switch (DV$type,
            "Interval"={
              an_name<-"General Linear Model"
              t_name<-"F"
              df<-an$Df
              tval<-an$`F value`
            },
            "Categorical"={
              an_name<-"Generalized Linear Model"
              t_name<-"chi2"
              df<-an$Df
              tval<-an$Deviance
            }
    )
    p.direct<-r2p(r.direct,n)
    p.unique<-r2p(r.unique,n)
    p.total<-r2p(r.total,n)

    result$r=list(direct=r.direct,unique=r.unique,total=r.total)
    result$rse=list(direct=r2se(r.direct,n),unique=r2se(r.unique,n),total=r2se(r.total,n))
    result$p=list(direct=p.direct,unique=p.unique,total=p.total)
  }
  
  # adding fields to existing result
  result$uModel<-lmRaw
  result$model<-lmNorm
  result$anova<-an
  result$nval<-n
  
  result$an_name<-an_name
  result$test_name<-t_name
  result$df<-df
  result$test_val<-tval

  result$showType<-evidence$showType
  result$Heteroscedasticity<-0
  # result$wval<-rn2w(result$rIV,result$nval)
  # result$nwval<-rw2n(result$rIV,0.8)
  result
  
}
