debugHere<-FALSE


wsd<-function(x,w=1) {
  if (length(w)==1) {
    sqrt(mean((x-mean(x))^2))
  } else {
  sqrt(sum((x-mean(x))^2 *w)/sum(w))
  }
}


r2p<-function(r,n){
  if (!is.numeric(r) || !is.numeric(n)) {return(1)}
  if (any(abs(r)>1)) {
    print(paste("r2p r-exception",format(max(abs(r)),digits=3)))
    r[r>1]<-1
  }
  if (any(abs(n)<3)) {
    print("r2p n-exception")
    n[n<3]<-4
  }
  t_vals<-r/r2se(r,n)
  (1-pt(abs(t_vals),n-2))*2
  
}

r2se<-function(r,n){
  if (any(abs(r)>1)) {
    print(paste("r2se r-exception",format(max(abs(r)),digits=3)))
    r[abs(r)>1]<-1
  }
  if (any(abs(n)<3)) {
    print("r2se n-exception")
    n[n<3]<-4
  }
  sqrt((1-r^2)/(n-2))
}

r2ci<-function(r,n,s=0){
  if (any(abs(r)>1)) {
    print(paste("r2ci r-exception",format(max(abs(r)),digits=3)))
    r[abs(r)>1]<-1
  }
  if (any(abs(n)<3)) {
    print("r2ci n-exception")
    n[n<3]<-4
  }
  z<-atanh(r)
  zci<-qnorm(1-0.05/2)*sqrt(1/(n-3))
  if (s==0){
    tanh(z+c(-1,1)*zci)
  } else {
    tanh(z+s*zci)
  }
}

r2llr<-function(r,n) {
  if (any(abs(r)>1)) {
    print(paste("r2llr r-exception",format(max(abs(r)),digits=3)))
    r[abs(r)>1]<-1
  }
  if (any(abs(n)<3)) {
    print("r2llr n-exception")
    n[n<3]<-4
  }
  r<-atan(r)
  if (isempty(llr$e1) || is.na(llr$e1)) { llr1=r }
  else {llr1=llr$e1}
  showVals<-log(dnorm(llr1,mean=r,sd=1/sqrt(n-3))/dnorm(llr$e2,mean=r,sd=1/sqrt(n-3)))
}



model2directeffect<-function(mF){
  if (class(mF)[1]=="lmerMod")
  {
    mTerms<-attr(terms(mF),"term.labels")
    data<-model.frame(mF)
    expected<-fitted(mF)
    residuals<-residuals(mF)
  } else {
    mTerms<-attr(mF$terms,"term.labels")
    data<-mF$model
    expected<-mF$fitted.values
    residuals<-mF$residuals
  }
  dv<-data[,1]
  
  if (is.numeric(dv)) {
    DVgain<-sd(dv,na.rm=TRUE)
  } else {
    DVgain<-sd(expected+residuals)
  }
  
  # single variable model
  if (length(mTerms)==1 && !grepl(":",mTerms)) {
    v1<-data[[mTerms[1]]]
    
    if (is.numeric(v1)) {
      m1<-mean(v1,na.rm=TRUE)
      h1<-c(1,1)
      data1<-data.frame(iv1=m1+c(-1,1)*sd(v1))
    } else {
      h1<-as.numeric(prop.table(table(v1)))
      data1<-data.frame(iv1=levels(v1))
    }
    names(data1)<-mTerms[1]
    # names(data1)<-colnames(mF$model)[2]
    if (class(mF)[1]=="lmerMod") {
      nc1<-predict(mF,cbind(data1,data.frame(participant="1")))
    } else {
      nc1<-predict.lm(mF,data1)
    }
    ncoeff<-wsd(nc1,h1) * sign(sum(diff(nc1)))
    return(ncoeff/DVgain)
  }
  
  # multiple variable model
  v1<-data[,2]
  v2<-data[,3]
  
  if (is.numeric(v1)){
    h1<-c(1,1)
    m1<-mean(v1,na.rm=TRUE)+c(-1,1)*sd(v1,na.rm=TRUE)
  } else {
    h1<-as.numeric(prop.table(table(v1)))
    m1<-levels(v1)
  }
  if (is.numeric(v2)){
    h2<-c(1,1)
    m2<-mean(v2,na.rm=TRUE)+c(-1,1)*sd(v2,na.rm=TRUE)
  } else {
    h2<-as.numeric(prop.table(table(v2)))
    m2<-levels(v2)
  }
  n1<-length(m1)
  n2<-length(m2)
  igrid<-meshgrid(1:n1,1:n2)
  data1<-data.frame(iv1=as.vector(m1[igrid$X]),iv2=as.vector(m2[igrid$Y]))

  # we find effect-size by 
  # looking at the change in prediction around the centre of an interval predictor
  # looking at the sd of predictions for all possible cases of categorical predictors
  
  if (class(mF)[1]=="lmerMod") {
    v<-predict(mF,cbind(data1,data.frame(participant="1")))
  } else {
    v<-predict.lm(mF,data1)
  }
  dim(v)<-c(n2,n1)
  
  nc1<-colMeans(v)
  nc2<-rowMeans(v)
  
  ncoeff1<-wsd(nc1,h1) * sign(sum(diff(nc1)))
  ncoeff2<-wsd(nc2,h2) * sign(sum(diff(nc2)))
  
  if (any(mTerms=="iv1:iv2")) {
    ie<-t(t(v)-nc1)-nc2
    ncoeff3<-wsd(ie,1)*sign(sum(diff(-ie[1,])))
  }

    switch (length(mTerms), 
          {return(ncoeff3/DVgain)},
          {return(c(ncoeff1, ncoeff2)/DVgain)},
          {return(c(ncoeff1, ncoeff2, ncoeff3)/DVgain)}
  )
  
}




multipleAnalysis<-function(IV,IV2,DV,effect,design,evidence,n_sims,appendData=FALSE, earlierResult=c(), showProgress=TRUE,progressPrefix=""){
  rho<-effect$rIV
  rho2<-effect$rIV2
  
  pvals=c()
  rvals=c()
  nvals=c()
  if (length(rho)<n_sims) {rho<-rep(rho,n_sims)}
  if (!is.null(IV2)) {
    if (length(rho2)<n_sims) {rho2<-rep(rho2,n_sims)}
  }

  nterms<-0
  if (IV$type=="Categorical") {nterms<-nterms+IV$ncats-1} else {nterms<-nterms+1}
  if (!is.null(IV2)) {
    if (IV2$type=="Categorical") {nterms<-nterms+IV2$ncats-1} else {nterms<-nterms+1}
  }

    if (appendData) {
    main_res<-earlierResult
  } else {
    main_res<-list(rpIV=c(),rIV=c(),pIV=c(),rIV2=c(),pIV2=c(),rIVIV2DV=c(),pIVIV2DV=c(),nval=c(),r=list(direct=c(),unique=c(),total=c(),coefficients=c()),showType=design$showType)
  }
  
  for (i in 1:n_sims){
    if (showProgress && (n_sims<=50 || (n_sims>50 && i==round(i/25)*25))) {
      showNotification(paste(progressPrefix,format(i),"/",format(n_sims)),id="counting",duration=Inf,closeButton=FALSE,type="message")
    } 
    effect$rIV<-rho[i]
    if (!is.null(IV2)) {effect$rIV2<-rho2[i]}
    
    sample<-makeSample(IV,IV2,DV,effect,design)
    res<-analyseSample(IV,IV2,DV,design,evidence,sample)

    # Replication?
    if (!isempty(design$sReplicationOn) && !is.na(design$sReplicationOn) && design$sReplicationOn) {
      while (design$sReplSigOnly && res$pIV>alpha) {
        sample<-makeSample(IV,IV2,DV,effect,design)
        res<-analyseSample(IV,IV2,DV,design,evidence,sample)
      }
      # note that we do the replication with a one-tailed set up
      rSign<-sign(res$rIV)
      effect1<-effect
      design1<-design
      design1$sN<-rw2n(res$rIV,design$sReplPower,1)
      effect1$rIV<-sample$effectRho
      effect1$populationPDF<-"Single"
      sample<-makeSample(IV,IV2,DV,effect1,design1)
      res<-analyseSample(IV,IV2,DV,design1,evidence,sample)
      if (sign(res$rIV)==rSign) {
      res$pIV<-res$pIV/2
      } else {
        res$pIV<-1
      }
    }
    
    if (is.na(res$rIV)) {
      res$rIV<-0
      res$pIV<-1
      res$nval<-0}
    main_res$rpIV<-rbind(main_res$rpIV,res$rpIV)
    main_res$rIV<-rbind(main_res$rIV,res$rIV)
    main_res$pIV<-rbind(main_res$pIV,res$pIV)
    main_res$nval<-rbind(main_res$nval,res$nval)

    if (class(res$rawModel)[1]=="lmerMod") {
      coeffs<-colMeans(coef(res$rawModel)$participant)
    } else {
      coeffs<-res$rawModel$coefficients
    }
    if (!is.null(IV2)){
      main_res$rIV2<-rbind(main_res$rIV2,res$rIV2)
      main_res$pIV2<-rbind(main_res$pIV2,res$pIV2)
      main_res$rIVIV2DV<-rbind(main_res$rIVIV2DV,res$rIVIV2DV)
      main_res$pIVIV2DV<-rbind(main_res$pIVIV2DV,res$pIVIV2DV)
      
      main_res$r$direct<-rbind(main_res$r$direct,res$r$direct)
      main_res$r$unique<-rbind(main_res$r$unique,res$r$unique)
      main_res$r$total<-rbind(main_res$r$total,res$r$total)
      main_res$r$coefficients<-rbind(main_res$r$coefficients,as.double(coeffs[2:length(coeffs)]))
                                     
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
      main_res$r$coefficients<-rbind(main_res$r$coefficients,as.double(coeffs)[2:length(coeffs)])
      
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
  
  # CONVERT ALL ORDINAL VARIABLES TO INTERVAL  
  if (IV$type=="Ordinal") {
    IV$type<-"Interval"
    }
  if (!is.null(IV2) && IV2$type=="Ordinal") {
    IV2$type<-"Interval"
    }
  if (!is.null(IV2) && DV$type=="Ordinal") {
    DV$type<-"Interval"
    }
  
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
  
  # MAKE MAIN DATA STORAGE
  resultRawData<-data.frame(participant=result$participant,iv1=iv1,iv2=iv2,dv=dv)
  
  #MAKE NORM DATA STORAGE
    # centre variables on zero
    if (IV$type=="Interval")  iv1=(iv1-mean(iv1,na.rm=TRUE))/sd(iv1,na.rm=TRUE)
    if (!is.null(IV2) && IV2$type=="Interval") iv2=(iv2-mean(iv2,na.rm=TRUE))/sd(iv2,na.rm=TRUE)
    if (DV$type=="Interval")  dv=(dv-mean(dv,na.rm=TRUE))/sd(dv,na.rm=TRUE)
    # make data frame
  resultNormData<-data.frame(participant=result$participant,iv1=iv1,iv2=iv2,dv=dv)
  
  # get Categorical cases sorted
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
  
# CREATE FORMULA
  formula<-"dv~iv1"
  if (!is.null(IV2)) {
    formula<-paste(formula,"+iv2",sep="")
    if (evidence$rInteractionOn==1) formula<-paste(formula,"+iv1:iv2",sep="")
  }
  if ((IV$type=="Categorical" && design$sIV1Use=="Within") || (!is.null(IV2) && IV2$type=="Categorical" && design$sIV2Use=="Within")){
    doingWithin<-TRUE
    formula<-paste(formula,"+(1|participant)",sep="")
  } else {
    doingWithin<-FALSE
  }

# SET UP CONTRASTS
  # these are needed to make the anova type 3 work properly
  contrasts<-c()
  if (IV$type=="Categorical")                   contrasts<-c(contrasts,list(iv1=contr.sum))
  if (!is.null(IV2) && IV2$type=="Categorical") contrasts<-c(contrasts,list(iv2=contr.sum))

  # get linear model and anova
  if (DV$type=="Categorical") {
    if (doingWithin) {
      lmRaw<-glmer(formula=as.formula(formula),data=resultRawData,family="binomial")
      # lmNorm to calculate effect sizes
      lmNorm<-glmer(formula=as.formula(formula),data=resultNormData,family="binomial",contrasts=contrasts)
    } else {
      lmRaw<-glm(formula=as.formula(formula),data=resultRawData,family="binomial")
      lmNorm<-glm(formula=as.formula(formula),data=resultNormData,family="binomial",contrasts=contrasts)
    }
    testMethod<-"F"
    pcol=3;prow=2
    
  } else { # Interval DV
    # lmRaw to report model
    if (doingWithin) {
      lmRaw<-lmer(formula=as.formula(formula),data=resultRawData)
      # lmNorm to calculate effect sizes
      lmNorm<-lmer(formula=as.formula(formula),data=resultNormData,contrasts=contrasts)
    } else {
    lmRaw<-lm(formula=as.formula(formula),data=resultRawData)
    # lmNorm to calculate effect sizes
    lmNorm<-lm(formula=as.formula(formula),data=resultNormData,contrasts=contrasts)
    }
    testMethod<-"F"
    pcol=4;prow=2;
  }

  switch (evidence$ssqType,
          "Type1"={
            anRaw<-Anova(lmRaw,test=testMethod)
            anNorm<-Anova(lmNorm,test=testMethod)
          },
          "Type2"={
            anRaw<-Anova(lmRaw,test=testMethod,type=2)
            anNorm<-Anova(lmNorm,test=testMethod,type=2)
          },
          "Type3"={
            anRaw<-Anova(lmRaw,test=testMethod,type=3,singular.ok=TRUE)
            anNorm<-Anova(lmNorm,test=testMethod,type=3,singular.ok=TRUE)
          }
          )
  anU<-Anova(lmNorm,test="F",type=3,singular.ok=TRUE)


  switch (no_ivs,
          { result$rpIV<-result$effectRho
            result$rIV<-model2directeffect(lmNorm)
            result$pIV<-r2p(result$rIV,n)
            rCI<-r2ci(result$rIV,n)
            # overall model effect-size
            result$rFull<-result$rIV
            result$rFullse<-r2se(result$rFull,n)
          },
          # 2 ivs
          { if (doingWithin) {
            # overall model effect-size
            result$rFull<-r.squaredGLMM(lmNorm)[[1]]
            result$rFullse<-r2se(result$rFull,n)
            
            directEffects<-model2directeffect(lmNorm)
            result$rIV<-directEffects[1]
            result$pIV<-r2p(result$rIV,n)
            #  IV2 next    
            result$rIV2<-directEffects[2]
            result$pIV2<-r2p(result$rIV2,n)
            #  interaction term
            if (evidence$rInteractionOn==1) {
              result$rIVIV2DV<-directEffects[3]
              result$pIVIV2DV<-r2p(result$rIVIV2DV,n)
            } else {
              result$rIVIV2DV<-NA
              result$pIVIV2DV<-NA
            }

            #             result$rIV<-model2effect(lmNorm,DV$type,"iv1")
            # result$pIV<-r2p(result$rIV,n)
            # result$rIV2<-model2effect(lmNorm,DV$type,"iv2")
            # result$pIV2<-r2p(result$rIV2,n)
            # result$rIVIV2DV<-model2effect(lmNorm,DV$type,"iv1:iv2")
            # result$pIVIV2DV<-r2p(result$rIVIV2DV,n)
            # a<-model2directeffect(lmNorm)
            # browser()
            
            result$rIVIV2<-0
            
            r.direct<-c(result$rIV,result$rIV2,result$rIVIV2DV)
            r.unique<-r.direct
            r.total<-r.direct
            
          } else {
            # overall model effect-size
            result$rFull<-sqrt(sum(anU$`Sum Sq`[is.element(rownames(anU),c("iv1","iv2","iv1:iv2"))])/sum(anU$`Sum Sq`))
            result$rFullse<-r2se(result$rFull,n)
            
            # 1. direct effect sizes for individual IVs
            #  IV first
            directEffects<-model2directeffect(lmNorm)
            if (any(abs(directEffects)>1)) {print("direct")}
            result$rIV<-directEffects[1]
            result$pIV<-r2p(result$rIV,n)
            #  IV2 next    
            result$rIV2<-directEffects[2]
            result$pIV2<-r2p(result$rIV2,n)
            #  interaction term
            if (evidence$rInteractionOn==1) {
              result$rIVIV2DV<-directEffects[3]
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
                    },
                    "Categorical"={
                      lm1total<-glm(formula=dv~iv1,data=resultNormData,family="binomial")
                      lm2total<-glm(formula=dv~iv2,data=resultNormData,family="binomial")
                      lm12total<-glm(formula=dv~iv1:iv2,data=resultNormData,family="binomial")
                    }
            )
            rIV1total<-model2directeffect(lm1total)
            rIV2total<-model2directeffect(lm2total)
            rIVIV2DVtotal<-model2directeffect(lm12total)
            totalEffects<-c(rIV1total,rIV2total,rIVIV2DVtotal)
            if (any(abs(totalEffects)>1)) {print("total")}
            
            # get the unique effects
            if (grepl("Intercept",rownames(anU)[[1]])) {n1<-2} else {n1<-1}
            n2<-nrow(anU)
            uniqueEffects<-sqrt(anU$`Sum Sq`[n1:(n2-1)]/sum(anU$`Sum Sq`[n1:n2]))
            uniqueEffects<-uniqueEffects*sign(directEffects)
            if (any(abs(uniqueEffects)>1)) {print("unique")}
            
            r.direct<-directEffects
            r.unique<-uniqueEffects
            r.total<-totalEffects
          }
            r.direct[is.na(r.direct)]=0
            r.unique[is.na(r.unique)]=0
            r.total[is.na(r.total)]=0
          }
  )
  
  if (is.null(IV2)) {
    hypothesisType=paste(IV$type,DV$type,sep=" ")
    switch (hypothesisType,
            "Interval Interval"={
              an_name<-"Pearson Correlation"
              t_name<-"r"
              df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")",sep="")
              tval<-result$rIV
            },
            "Ordinal Interval"={
              an_name<-"Pearson Correlation"
              t_name<-"r"
              df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")",sep="")
              tval<-result$rIV
            },
            "Categorical Interval"={
              if (IV$ncats==2){
                if (IV$type=="Categorical" && design$sIV1Use=="Within"){
                  an_name<-"t-test: Paired Samples"
                  df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")")
                  tv<-t.test(dv~iv1,paired=TRUE,var.equal=!evidence$Welch)
                  tval<-tv$statistic
                  result$pIV<-tv$p.value
                } else {
                  an_name<-"t-test: Independent Samples"
                  df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")")
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
                # tval<-sqrt(anRaw$`F value`[2])*sign(result$rIV)
              } else {
                if (IV$type=="Categorical" && design$sIV1Use=="Within"){
                  an_name<-"One-Way ANOVA: Repeated Measures"
                } else {
                  an_name<-"One-Way ANOVA: Independent Measures"
                }
                t_name<-"F"
                df<-paste("(",format(anRaw$Df[2]),",",format(anRaw$Df[nrow(anRaw)]),")",sep="")
                tval<-anRaw$`F value`[2]
              }
            },
            "Interval Ordinal"={
              an_name<-"Spearman Correlation"
              t_name<-"rho"
              df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")",sep="")
              op <- options(warn = (-1))
              tv<-cor.test(iv1, dv, method="spearman")
              options(op)
              tval<-tv$estimate
              result$pIV<-tv$p.value
            },
            "Ordinal Ordinal"={
              an_name<-"Spearman Correlation"
              t_name<-"rho"
              df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")",sep="")
              op <- options(warn = (-1))
              tv<-cor.test(iv1, dv, method="spearman")
              options(op)
              tval<-tv$estimate
              result$pIV<-tv$p.value
            },
            "Categorical Ordinal"={
              if (IV$ncats==2){
                if (IV$type=="Categorical" && design$sIV1Use=="Within"){
                  an_name<-"Wilcoxon signed-rank Test: Paired Samples"
                  df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")")
                  t_name<-"T"
                  op <- options(warn = (-1))
                  tv<-wilcox.test(dv~iv1,paired=TRUE,exact=FALSE)
                  options(op)
                  tval<-tv$statistic
                  result$pIV<-tv$p.value
                } else {
                  an_name<-"Mann Whitney U test: Independent Samples"
                  df<-paste("(",format(anRaw$Df[nrow(anRaw)]),")")
                  t_name<-"U"
                  op <- options(warn = (-1))
                  tv<-wilcox.test(dv~iv1,exact=FALSE)
                  options(op)
                  tval<-tv$statistic
                  result$pIV<-tv$p.value
                }
              } else {
                if (IV$type=="Categorical" && design$sIV1Use=="Within"){
                  an_name<-"Friedman Test: Repeated Measures"
                  op <- options(warn = (-1))
                  tv<-friedman(dv~iv1);
                  options(op)
                  t_name='chi2'
                  tval<-tv$statistic
                  result$pIV<-tv$p.value
                } else {
                  an_name<-"Kruskal Wallis Test: Independent Measures"
                  op <- options(warn = (-1))
                  tv<-kruskal.test(dv~iv1);
                  options(op)
                  t_name='chi2'
                  tval<-tv$statistic
                  result$pIV<-tv$p.value
                }
                df<-paste("(",format(anRaw$Df[2]),",n=",format(length(dv)),")",sep="")
              }
            },
            "Interval Categorical"={
              an_name<-"Logistic Regression"
              t_name<-"chi2"
              df<-paste("(",format(anRaw$Df[2]),",","n=",format(lmNorm$df.null+1),")",sep="")
              tval<-anRaw$Chisq[2]
            },
            "Ordinal Categorical"={
              an_name<-"Logistic Regression"
              t_name<-"chi2"
              df<-paste("(",format(anRaw$Df[2]),",","n=",format(lmNorm$df.null+1),")",sep="")
              tval<-anRaw$Chisq[2]
            },
            "Categorical Categorical"={
              an_name<-"Chi-square test of independence"
              t_name<-"chi2"
              df<-paste("(",format(anRaw$Df[2]),",","n=",format(lmNorm$df.null+1),")",sep="")
              
              chiResult<-chisq.test(iv1,dv,correct = FALSE)
              result$rIV<-sqrt(chiResult$statistic/n)*sign(result$rIV)
              result$pIV<-chiResult$p.value
              result$rFull<-result$rIV
              result$rFullse<-r2se(result$rFull,n)
              tval<-chiResult$statistic
            }
    )
    if (is.na(result$pIV)) {result$pIV<-1}
  } else {
    switch (DV$type,
            "Interval"={
              an_name<-"General Linear Model"
              t_name<-"F"
              df<-anRaw$Df
              tval<-anRaw$`F value`
            },
            "Categorical"={
              an_name<-"Generalized Linear Model"
              t_name<-"chi2"
              df<-anRaw$Df
              tval<-anRaw$Deviance
            }
    )
    p.direct<-r2p(r.direct,n)
    p.unique<-r2p(r.unique,n)
    p.total<-r2p(r.total,n)
    
    if (!evidence$rInteractionOn) {
      r.direct<-r.direct[1:2]
      r.unique<-r.unique[1:2]
      r.total<-r.total[1:2]
      
      p.direct<-p.direct[1:2]
      p.unique<-p.unique[1:2]
      p.total<-p.total[1:2]
      
    }
    
    result$r=list(direct=r.direct,unique=r.unique,total=r.total)
    result$rse=list(direct=r2se(r.direct,n),unique=r2se(r.unique,n),total=r2se(r.total,n))
    result$p=list(direct=p.direct,unique=p.unique,total=p.total)
  }
  
  # adding fields to existing result
  result$rawModel<-lmRaw
  result$normModel<-lmNorm
  result$rawAnova<-anRaw
  result$normAnova<-anNorm
  result$nval<-n
  
  result$model<-result$rawModel
  result$anova<-result$rawAnova
  
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
