model2effect<-function(mF,DVvarType,variable=NULL){
  switch (DVvarType,
          "Interval"=     r<-lm2effect(mF,variable),
          "Categorical"=  r<-glm2effect(mF,variable)
  )
  if (is.null(variable)){
    r
  } else {
    r*signEffect(mF,variable)
  }
}

model2partialeffect<-function(mF,DVvarType,variable=NULL){

  switch (DVvarType,
          "Interval"={DVgain<-sd(mF$model$dv)},
          "Categorical"={DVgain<-sd(mF$fitted.values+mF$residuals)}
          )

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
}

lm2effect<-function(lmF,variable){
  an<-anova(lmF)
  ssq<-an[["Sum Sq"]]
  if (!is.null(variable)) {
    use<-is.element(rownames(an),variable)
  } else {
    use<-!is.element(rownames(an),c("participant","Residuals"))
  }
  sqrt(sum(ssq[use])/sum(ssq))
}

glm2effect<-function(glmF,variable) {
  an<-anova(glmF)
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
  nm<-names(mF$coefficients)
  switch (variable,
          "iv1"= {use<-grepl("iv1[^:]{1}",nm) | grepl("iv1$",nm)},
          "iv2"= {use<-grepl("iv2[^:]{1}",nm) | grepl("iv2$",nm)},
          "iv1:iv2"={use<-grepl("iv1[^:]*:iv2[^:]*",nm)}
          )
  
  sign(mean(mF$coefficients[use],na.rm=TRUE))
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

multipleAnalysis<-function(IV,IV2,DV,effect,design,evidence,n_sims,appendData=FALSE, showProgress=TRUE){
  rho<-effect$rIV
  rho2<-effect$rIV2
  
  pvals=c()
  rvals=c()
  nvals=c()
  main_res<-list(rIV=c(),pIV=c(),rIV2=c(),pIV2=c(),rIVIV2DV=c(),pIVIV2DV=c(),nval=c(),r=list(direct=c(),unique=c(),total=c()),showType=design$showType)
  if (length(rho)<n_sims) {rho<-rep(rho,n_sims)}
  if (!is.null(IV2)) {
    if (length(rho2)<n_sims) {rho2<-rep(rho2,n_sims)}
  }

  for (i in 1:n_sims){
    if (showProgress && (n_sims<=50 || (n_sims>50 && i==round(i/25)*25))) {
      showNotification(paste(format(i),"/",format(n_sims)),id="counting",duration=Inf,closeButton=FALSE,type="message")
    } 
    effect$rIV<-rho[i]
    if (!is.null(IV2)) {effect$rIV2<-rho2[i]}
    
    result<-makeSample(IV,IV2,DV,effect,design)
    res<-analyseSample(IV,IV2,DV,design,evidence,result)
    
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
      
      main_res$p$direct<-rbind(main_res$p$direct,res$p$direct)
      main_res$p$unique<-rbind(main_res$p$unique,res$p$unique)
      main_res$p$total<-rbind(main_res$p$total,res$p$total)
    } else {
      main_res$rIV2<-rbind(main_res$rIV2,0)
      main_res$pIV2<-rbind(main_res$pIV2,1)
      main_res$rIVIV2DV<-rbind(main_res$rIVIV2DV,0)
      main_res$pIVIV2DV<-rbind(main_res$pIVIV2DV,1)
      
      main_res$r$direct<-rbind(main_res$r$direct,res$rIV)
      main_res$r$unique<-rbind(main_res$r$unique,res$rIV)
      main_res$r$total<-rbind(main_res$r$total,res$rIV)
      
      main_res$p$direct<-rbind(main_res$p$direct,res$pIV)
      main_res$p$unique<-rbind(main_res$p$unique,res$pIV)
      main_res$p$total<-rbind(main_res$p$total,res$pIV)
      }
  }
  if (showProgress) {removeNotification(id = "counting")}
  
  if (appendData){
    main_res$rIV<-rbind(expectedResultHold$rIV,main_res$rIV)
    main_res$pIV<-rbind(expectedResultHold$pIV,main_res$pIV)
    main_res$nval<-rbind(expectedResultHold$nval,main_res$nval)
    if (!is.null(IV2)){
      main_res$rIV2<-rbind(expectedResultHold$rIV2,main_res$rIV2)
      main_res$pIV2<-rbind(expectedResultHold$pIV2,main_res$pIV2)
      main_res$rIVIV2DV<-rbind(expectedResultHold$rIVIV2DV,main_res$rIVIV2DV)
      main_res$pIVIV2DV<-rbind(expectedResultHold$pIVIV2DV,main_res$pIVIV2DV)
      
      main_res$r$direct<-rbind(expectedResultHold$r$direct,main_res$r$direct)
      main_res$r$unique<-rbind(expectedResultHold$r$unique,main_res$r$unique)
      main_res$r$total<-rbind(expectedResultHold$r$total,main_res$r$total)
      
      main_res$p$direct<-rbind(expectedResultHold$p$direct,main_res$p$direct)
      main_res$p$unique<-rbind(expectedResultHold$p$unique,main_res$p$unique)
      main_res$p$total<-rbind(expectedResultHold$p$total,main_res$p$total)
    } else {
      main_res$rIV2<-rbind(expectedResultHold$rIV2,0)
      main_res$pIV2<-rbind(expectedResultHold$pIV2,1)
      main_res$rIVIV2DV<-rbind(expectedResultHold$rIVIV2DV,0)
      main_res$pIVIV2DV<-rbind(expectedResultHold$pIVIV2DV,1)
      
      main_res$r$direct<-rbind(expectedResultHold$r$direct,main_res$rIV)
      main_res$r$unique<-rbind(expectedResultHold$r$unique,main_res$rIV)
      main_res$r$total<-rbind(expectedResultHold$r$total,main_res$rIV)
      
      main_res$p$direct<-rbind(expectedResultHold$p$direct,main_res$pIV)
      main_res$p$unique<-rbind(expectedResultHold$p$unique,main_res$pIV)
      main_res$p$total<-rbind(expectedResultHold$p$total,main_res$pIV)
    }
  }  
  main_res$showType<-evidence$showType
  # expectedResult<-list(rval=rvals,pval=pvals,nval=nvals)
  expectedResultHold<<-main_res
  main_res
}


analyseSample<-function(IV,IV2,DV,design,evidence,result){
  
  if (is.null(IV2)) {no_ivs<-1} else {no_ivs<-2}

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
  
  n<-length(dv)
  resultUData<-data.frame(participant=result$participant,iv1=iv1,iv2=iv2,dv=dv)
  
  
  # normalize variables
  if (IV$type=="Interval")  iv1=(iv1-mean(iv1,na.rm=TRUE))/sd(iv1,na.rm=TRUE)
  if (!is.null(IV2) && IV2$type=="Interval") iv2=(iv2-mean(iv2,na.rm=TRUE))/sd(iv2,na.rm=TRUE)
  if (DV$type=="Interval")  dv=(dv-mean(dv,na.rm=TRUE))/sd(dv,na.rm=TRUE)
  # make data frame
  resultData<-data.frame(participant=result$participant,iv1=iv1,iv2=iv2,dv=dv)
  
  # get cases sorted
  if (IV$type=="Categorical"){
    switch (evidence$evidenceCaseOrder,
            "Alphabetic"={ref=1},
            "AsFound"={ref=as.numeric(iv1[1])},
            "Frequency"={ref=which.max(tabulate(match(iv1, IV$cases)))}
    )
    resultData$iv1<-relevel(resultData$iv1,ref=ref)
  }
  if (!is.null(IV2) && IV2$type=="Categorical"){
    switch (evidence$evidenceCaseOrder,
            "Alphabetic"={ref=1},
            "AsFound"={ref=as.numeric(iv2[1])},
            "Frequency"={ref=which.max(tabulate(match(iv2, IV2$cases)))}
    )
    resultData$iv2<-relevel(resultData$iv2,ref=ref)
  }
  if (DV$type=="Categorical"){
    switch (evidence$evidenceCaseOrder,
            "Alphabetic"={ref=1},
            "AsFound"={ref=as.numeric(dv[1])},
            "Frequency"={ref=which.max(tabulate(match(dv, DV$cases)))}
    )
    resultData$dv<-relevel(resultData$dv,ref=ref)
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
            lmU<-lm(formula=formula,data=resultUData)
            if (!is.null(IV2)){
              if (c1 || c2) {
                lmU3<-lm(formula=formula,data=resultUData,contrasts=contrasts)
              } else {
                lmU3<-lmU
              }
            } else{ lmU3<-lmU}
            lmF<-lm(formula=formula,data=resultData)
            testMethod<-"F"
            testMethod3<-"F"
            pcol=4;prow=2;
          },
          "Categorical"={
            lmU<-glm(formula=formula,data=resultUData,family="binomial")
            if (!is.null(IV2)){
              if (c1 || c2) {
                lmU3<-glm(formula=formula,data=resultUData,family="binomial",contrasts=contrasts)
              } else {
                lmU3<-lmU
              }
            } else{ lmU3<-lmU}
            lmF<-glm(formula=formula,data=resultData,family="binomial")
            testMethod<-"Chisq"
            testMethod3<-"Wald"
            pcol=3;prow=2
          }
  )
  
  switch (evidence$ssqType,
          "Type1"={an<-Anova(lmU,test=testMethod3)},
          "Type2"={an<-Anova(lmU,test=testMethod3,type=2)},
          "Type3"={an<-Anova(lmU3,test=testMethod3,type=3)},
          "Type3w"={an<-Anova(lmU,test=testMethod3,type=3)}
          )

  # overall model effect-size
  # result$rFull<- model2effect(lmF,DV$type)
  # result$rFullse<-r2se(result$rFull,n)
  
  switch (DV$type,
          "Interval"={Df<-sum(an$Df)},
          "Categorical"={Df<-an$`Resid. Df`[1]}
  )
  switch (no_ivs,
          {
            result$rIV<-model2effect(lmF,DV$type,"iv1")
            result$pIV<-an[prow,pcol]
            rCI<-r2ci(result$rIV,n)
          },
          # 2 ivs
          { if (doingWithin) {
            result$rIV<-model2effect(lmF,DV$type,"iv1")
            result$pIV<-r2p(result$rIV,Df+1)
            result$rIV2<-model2effect(lmF,DV$type,"iv2")
            result$pIV2<-r2p(result$rIV2,Df+1)
            result$rIVIV2DV<-model2effect(lmF,DV$type,"iv1:iv2")
            result$pIVIV2DV<-r2p(result$rIVIV2DV,Df+1)
            
            result$rIVIV2<-0
            
            r.direct<-c(result$rIV,result$rIV2,result$rIVIV2DV)
            r.unique<-r.direct
            r.total<-r.direct
            
          } else {
            # 1. direct effect sizes for individual IVs
            #  IV first
            result$rIV<-model2partialeffect(lmF,DV$type,"iv1")
            result$pIV<-r2p(result$rIV,n)
            #  IV2 next    
            result$rIV2<-model2partialeffect(lmF,DV$type,"iv2")
            result$pIV2<-r2p(result$rIV2,n)
            #  interaction term
            if (evidence$rInteractionOn==1) {
              result$rIVIV2DV<-model2partialeffect(lmF,DV$type,"iv1:iv2")
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
            totalSD<-sd(lmF$fitted.values+lmF$residuals)
            switch (DV$type,
                    "Interval"={
                      # total effect sizes
                      lm1total<-lm(formula=dv~iv1,data=resultData)
                      lm2total<-lm(formula=dv~iv2,data=resultData)
                      lm12total<-lm(formula=dv~iv1:iv2,data=resultData)
                      rIV1total<-model2effect(lm1total,DV$type,"iv1")
                      rIV2total<-model2effect(lm2total,DV$type,"iv2")
                      rIV1IV2total<-model2effect(lm12total,DV$type,"iv1:iv2")
                      
                      # get the unique effects
                      # model without iv1:iv2
                      a3<-lm(formula=dv~iv1+iv2,data=resultData,contrasts=contrasts)
                      # fitted values due to iv1:iv2
                      a3d<-lmF$fitted.values-a3$fitted.values
                      rIV1IV2unique<-sd(a3d)/totalSD * sign(cor(as.numeric(iv1)*as.numeric(iv2),a3d))
                      
                      # model without iv1
                      # fitted values due to iv1
                      a1d<-a3$fitted.values-lm2total$fitted.values
                      rIV1unique<-sd(a1d)/totalSD * sign(cor(as.numeric(iv1),a1d))

                      # model without iv2
                      # fitted values due to iv2
                      a2d<-a3$fitted.values-lm1total$fitted.values
                      rIV2unique<-sd(a2d)/totalSD * sign(cor(as.numeric(iv2),a2d))
                      
                    },
                    "Categorical"={
                      lm1total<-glm(formula=dv~iv1,data=resultData,family="binomial")
                      lm2total<-glm(formula=dv~iv2,data=resultData,family="binomial")
                      lm12total<-glm(formula=dv~iv1:iv2,data=resultData,family="binomial")
                      rIV1total<-model2effect(lm1total,DV$type,"iv1")
                      rIV2total<-model2effect(lm2total,DV$type,"iv2")
                      rIV1IV2total<-model2effect(lm12total,DV$type,"iv1:iv2")
                      
                      # get the unique effects
                        # model without iv1:iv2
                        a3<-glm(formula=dv~iv1+iv2,data=resultData,family="binomial")
                        # fitted values due to iv1:iv2
                        a3d<-lmF$fitted.values-a3$fitted.values
                        rIV1IV2unique<-sd(a3d)/totalSD * sign(cor(as.numeric(iv1)*as.numeric(iv2),a3d))
                        
                        # model without iv1
                        # fitted values due to iv1
                        a1d<-a3$fitted.values-lm2total$fitted.values
                        rIV1unique<-sd(a1d)/totalSD * sign(cor(as.numeric(iv1),a1d))
                        
                        # model without iv2
                        # fitted values due to iv2
                        a2d<-a3$fitted.values-lm1total$fitted.values
                        rIV2unique<-sd(a2d)/totalSD * sign(cor(as.numeric(iv2),a2d))

                      # # unique effect sizes
                      anUnique<-Anova(lmF,test.statistic = "LR")
                      rIV1unique<-sqrt(anUnique["iv1",]$`LR Chisq`/n) * sign(cor(as.numeric(iv1),a1d))
                      rIV2unique<-sqrt(anUnique["iv2",]$`LR Chisq`/n) * sign(cor(as.numeric(iv2),a2d))
                      rIV1IV2unique<-sqrt(anUnique["iv1:iv2",]$`LR Chisq`/n) * sign(cor(as.numeric(iv1)*as.numeric(iv2),a3d))
                    }
            )

            r.direct<-c(result$rIV,result$rIV2,result$rIVIV2DV)
            r.unique<-c(rIV1unique,rIV2unique,rIV1IV2unique)
            r.total<-c(rIV1total,rIV2total,rIV1IV2total)
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
            "Categorical Interval"={
              if (IV$ncats==2){
                if (design$sIV1Use=="Within"){
                  an_name<-"t-test: Paired Samples"
                  df<-paste("(",format(an$Df[nrow(an)]),")")
                } else {
                  an_name<-"t-test: Independent Samples"
                  df<-paste("(",format(an$Df[nrow(an)]),")")
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
            "Interval Categorical"={
              an_name<-"Logistic Regression"
              t_name<-"chi2"
              df<-paste("(",format(an$Df[2]),",","n=",format(lmF$df.null+1),")",sep="")
              tval<-an$Chisq[2]
            },
            "Categorical Categorical"={
              an_name<-"Chi-square test of independence"
              t_name<-"chi2"
              df<-paste("(",format(an$Df[2]),",","n=",format(lmF$df.null+1),")",sep="")
              
              chiResult<-chisq.test(iv1,dv,correct = FALSE)
              result$rIV<-sqrt(chiResult$statistic/n)
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
  result$uModel<-lmU
  result$model<-lmF
  result$anova<-an
  result$nval<-n
  
  result$an_name<-an_name
  result$test_name<-t_name
  result$df<-df
  result$test_val<-tval

  result$showType<-evidence$showType
  # result$wval<-rn2w(result$rIV,result$nval)
  # result$nwval<-rw2n(result$rIV,0.8)
  result
  
}
