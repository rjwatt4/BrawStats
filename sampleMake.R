outlierValue=4
dependenceVal=0.1
clusterVal=0.25
withinCor=0.5
hsyConstant=1
make_debug=FALSE


makeSampleVar<-function(n,sdv,MV){
  if (MV$type=="Interval" && (MV$skew!=0 || MV$kurtosis!=3)){
    
    a<-f_johnson_M(0,sdv,MV$skew,MV$kurtosis)
    ivr<-rJohnson(n,parms=a)
  } else {
    ivr<-rnorm(n,0,sdv)
  }
}

makeSample<-function(IV,IV2,DV,effect,design){

  n<-design$sN
  rho<-effect$rIV
  if (n==0){
    iv<-array(0,0)  
    dv<-iv
    xplot<-iv
    yplot<-xplot
    sampleRho<-0
    samplePval<-0
    IVs<-IV
    IV2s<-IV2
    DVs<-DV
    
  }  else {
    
    if (design$sMethod=="Resample"){
      use=ceiling(runif(n,min=0,max=1)*n)
      id<-1:n
      iv<-lastSample$iv[use]
      if (!is.null(IV2)){
        iv2<-lastSample$iv2[use]
      } else{
        iv2<-NULL
      }
      dv<-lastSample$dv[use]
      sampleRho<-0
      samplePval<-0
      
    } else{
      
    if (IV$process=="data" && DV$process=="data"){
      useIV<-match(IV$name,variables$name)
      useDV<-match(DV$name,variables$name)

      id<-importedData[[1]]
      iv<-importedData[[useIV+1]]    
      dv<-importedData[[useDV+1]]    
      sampleRho<-0
      samplePval<-0
      
      waste<-(is.na(iv) | is.na(dv))

      if (!is.null(IV2)) {
        useIV2<-match(IV2$name,variables$name)
        iv2<-importedData[[useIV2+1]]    
        waste<-waste | is.na(iv2)
      } else {
        iv2<-rep(0,length(iv))
      }
      keep<-!waste
      iv<-iv[keep]
      iv2<-iv2[keep]
      dv<-dv[keep]
      id<-id[keep]
      
      if (variables[useIV,]$type=="Categorical")
      { problem<-FALSE
       cases<-str_split(variables[useIV,]$cases,",")[[1]]
        for (i in 1:variables[useIV,]$ncats){
          if (sum(iv==cases[i])<3) {
            problem<-TRUE
            errorText<-paste("Not enough samples with ", variables[useIV,]$name, "==", cases[i])
            showModal(modalDialog(title=NULL,errorText))
            showNotification(errorText)
            return(NULL)
          }
        }
        }
      # remove duplicates that arise when there is an unused within variable
      # remove duplicated rows (from covariates of within designs)
        waste<-duplicated(data.frame(pt=id,iv=iv,iv2=iv2,dv=dv))
        iv<-iv[!waste]
        iv2<-iv2[!waste]
        dv<-dv[!waste]
        id<-id[!waste]

      
      # save the result
      lastSample<<-list(participant=id, iv=iv, iv2=iv2, dv=dv)
      
    } else {
      
      # deal with opportunity sampling    
      #  by setting up some anomalies
      if (design$sMethod=="Opportunity"){
        design$sIVRange<-c(rnorm(1)/3-2,rnorm(1)+2)
        design$sDependence<-runif(1,min=0.2,max=0.4)
        design$sClustering<-runif(1,min=0.2,max=0.4)
      }
      
      # make id
      id<-factor(1:n)
      
      # make iv
      if (design$sRangeOn && ((design$sIVRange[1]>-fullRange) || (design$sIVRange[2]<fullRange))) {
        ivr=c();
        while (length(ivr)<n) {
          ivr1<-makeSampleVar(n,1,IV)
          ivr<-c(ivr, ivr1[ivr1>design$sIVRange[1] & ivr1<design$sIVRange[2]])
        }
        ivr<-ivr[1:n]
      } else {
        ivr<-makeSampleVar(n,1,IV)
      }
      
      if (IV$type=="Categorical"){
        pp<-as.numeric(unlist(strsplit(IV$proportions,",")))
        ng<-IV$ncats
        if (length(pp)<ng) {pp<-c(pp,rep(pp[length(pp)],ng-length(pp)))}
        proportions<-c(0,pp)
        breaks<-qnorm(cumsum(proportions)/sum(proportions))
        vals=ivr*0
        cases=1:ng
        for (i in 1:ng) {vals=vals+(ivr>breaks[i])}
        ivr<-(vals-mean(cases))/(sd(cases)*sqrt((ng-1)/ng))
      }
      
      # make iv2 (if needed)
      if (!is.null(IV2)){
        rho2<-effect$rIV2
        rho12<-effect$rIVIV2
        ivr2_resid<-makeSampleVar(n,sqrt(1-rho12^2),IV2)
        iv2r<-ivr*rho12+ivr2_resid
        if (make_debug) {print(cor(ivr,iv2r))}
        
        if (IV2$type=="Categorical"){
          pp<-as.numeric(unlist(strsplit(IV2$proportions,",")))
          ng<-IV2$ncats
          if (length(pp)<ng) {pp<-c(pp,rep(pp[length(pp)],ng-length(pp)))}
          proportions<-c(0,pp)
          breaks<-qnorm(cumsum(proportions)/sum(proportions))
          vals=iv2r*0
          cases=1:ng
          for (i in 1:ng) {vals=vals+(iv2r>breaks[i])}
          iv2r<-(vals-mean(cases))/(sd(cases)*sqrt((ng-1)/ng))
        }
      } else {
        rho2<-0
        rho12<-0
        iv2r<-0
      }
      
      # make the interaction term
      if (!is.null(IV2)){
        rhoInter<-effect$rIVIV2DV
        iv12r<-ivr*iv2r
      } else {
        iv12r<-0
        rhoInter<-0
      }
      
      # make residuals
      variance_explained=rho^2+rho2^2+rhoInter^2+2*rho*rho2*rho12
      residual<-makeSampleVar(n,sqrt(1-variance_explained),DV)

      # non-independence  
      if (design$sDependence>0) {
        change<-round(n*design$sDependence/2)
        ivr[1:change]<-ivr[change+(1:change)]+rnorm(change,0,1)*dependenceVal
        if (!is.null(IV2)) {
        iv2r[1:change]<-iv2r[change+(1:change)]+rnorm(change,0,1)*dependenceVal
        iv12r[1:change]<-iv12r[change+(1:change)]+rnorm(change,0,1)*dependenceVal
        }
        residual[1:change]<-residual[change+(1:change)]+rnorm(change,0,1)*dependenceVal
      }
      
      # do within design
      if (design$sIV1Use=="Within") {
        b<-drawCatPositions(IV$ncats)
        b<-b/(sd(b)*sqrt((IV$ncats-1)/IV$ncats))
        rsd<-residual
        
        ivr_new<-c()
        iv2r_new<-c()
        residual<-c()
        for (i in 1:IV$ncats) {
          ivr_new<-c(ivr_new,rep(b[i],n))
          if (!is.null(IV2)){iv2r_new<-c(iv2r_new,iv2r)} else {iv2r_new<-0}
          residual<-c(residual,rsd*withinCor+sqrt(1-withinCor^2)*rnorm(n,0,sqrt(1-rho^2)))
        }
        ivr<-ivr_new
        iv2r<-iv2r_new
        id<-rep(id,IV$ncats)
        
        n<-n*IV$ncats
      } 
      
      if (!is.null(IV2) && design$sIV2Use=="Within") {
        b<-drawCatPositions(IV2$ncats)
        b<-b/(sd(b)*sqrt((IV2$ncats-1)/IV2$ncats))
        rsd<-residual
        
        ivr_new<-c()
        iv2r_new<-c()
        residual<-c()
        for (i in 1:IV2$ncats) {
          iv2r_new<-c(iv2r_new,rep(b[i],n))
          ivr_new<-c(ivr_new,ivr)
          residual<-c(residual,rsd*withinCor+sqrt(1-withinCor^2)*rnorm(n,0,sqrt(1-rho^2)))
        }
        ivr<-ivr_new
        iv2r<-iv2r_new
        id<-rep(id,IV2$ncats)
        
        n<-n*IV2$ncats
      } 
      
      if (design$sHeteroscedasticity!=0){
        localVar<- abs(ivr/3)^hsyConstant * sign(ivr)
        residual<-residual*(1+localVar*design$sHeteroscedasticity)
      }
      
      # make dv  
      dvr<- rho*ivr + rho2*iv2r + rhoInter*iv12r + residual
      # proceed  
      sampleRho<-0
      samplePval<-1
      # sampleRho<-cor(ivr,dvr)
      # p<-cor.test(ivr,dvr)
      # samplePval<-p$p.value
      
      
      # outliers - as errors
      if (design$sOutliers>0) {
        change<-round(n*design$sOutliers)
        dvr[1:change]<-sign(dvr[1:change])*outlierValue
      }
      
      # trim DV values
      if (design$sRangeOn && ((design$sDVRange[1]>-fullRange) || (design$sDVRange[2]<fullRange))) {
        keep<-dvr<=design$sDVRange[2] & dvr>=design$sDVRange[1]
        dvr<-dvr[keep]
        ivr<-ivr[keep]
        ivr2<-ivr2[keep]
        id<-id[keep]
      }
    
      switch(IV$type,
             "Interval"={
               iv<-ivr*IV$sd+IV$mu
             },
             "Categorical"={
               breaks<-qnorm((0:IV$ncats)/IV$ncats)
               vals=ivr*0
               for (i in 1:IV$ncats) {vals=vals+(ivr>breaks[i])}
               iv<-factor(vals,levels=1:IV$ncats,labels=IV$cases)
             }
      )
      
      if (!is.null(IV2)) {
      switch(IV2$type,
             "Interval"={
               iv2<-iv2r*IV2$sd+IV2$mu
             },
             "Categorical"={
               breaks<-qnorm((0:IV2$ncats)/IV2$ncats)
               vals=iv2r*0
               for (i in 1:IV2$ncats) {vals=vals+(iv2r>breaks[i])}
               iv2<-factor(vals,levels=1:IV2$ncats,labels=IV2$cases)
             }
      )
      } else {
        iv2<-iv2r
      }
      
      switch(DV$type,
             "Interval"={
               dv<-dvr*DV$sd+DV$mu
             },
             "Categorical"={
               pp<-as.numeric(unlist(strsplit(DV$proportions,",")))
               ng<-DV$ncats
               if (length(pp)<ng) {pp<-c(pp,rep(pp[length(pp)],ng-length(pp)))}
               proportions<-c(0,pp)
               breaks<-qnorm(cumsum(proportions)/sum(proportions))
               vals=dvr*0
               for (i in 1:ng) {vals=vals+(dvr>breaks[i])}
               dv<-factor(vals,levels=1:ng,labels=DV$cases)
             }
      )
      lastSample<<-list(participant=id, iv=iv, iv2=iv2, dv=dv)
      
    }
    } # end of simulate
    
    switch(IV$type,
           "Interval"={
             IVs<-list(mu=mean(iv),sd=sd(iv),name=IV$name,type=IV$type,vals=iv)
           },
           "Categorical"={
             IVs<-list(mu=0, sd=1, name=IV$name,type=IV$type,ncats=IV$ncats,cases=IV$cases,vals=iv)
           }
    )
    
    
    if (!is.null(IV2)) {
      switch(IV2$type,
           "Interval"={
             IV2s<-list(name=IV2$name,type=IV2$type,mu=mean(iv2),sd=sd(iv2),vals=iv2)
           },
           "Categorical"={
             IV2s<-list(name=IV2$name,type=IV2$type,mu=0, sd=1, ncats=IV2$ncats,cases=IV2$cases,vals=iv2)
           }
    )
    } else{
      IV2s<-NULL
    }
    
    switch(DV$type,
           "Interval"={
             DVs<-list(mu=mean(dv),sd=sd(dv),name=DV$name,type=DV$type,vals=dv)
           },
           "Categorical"={
             DVs<-list(mu=0, sd=1, name=DV$name,type=DV$type,ncats=DV$ncats,cases=DV$cases,vals=dv)
           }
    )
  
    yplot<-dv

    switch(IV$type,
           "Interval"={xplot<-iv},
           "Categorical"={xplot<-match(iv,levels(iv))}
    )
    
    if (IV$type=="Categorical"){
      xp<-xplot
      xpos<-drawCatPositions(IV$ncats)
      for (i in 1:IV$ncats) {
        use1=(xp==i)
        if (DV$type=="Interval"){
          mn1=mean(dv[use1])
          sd1=sd(dv[use1])
          xplot[use1]<-xpos[i]+rnorm(length(xplot[use1]),mean=0,sd=exp(-0.5*((dv[use1]-mn1)/sd1)^2))*0.15
        } else {
          xplot[use1]<-xpos[i]+rnorm(length(xplot[use1]))*mean(use1)*0.3
        }
      }
      # xplot<-xplot-(IV$ncats+1)/2
    }
    
    if (!is.null(IV2)){
      switch(IV2$type,
             "Interval"={x2plot<-iv2},
             "Categorical"={x2plot<-match(iv2,levels(iv2))}
      )
      if (IV2$type=="Categorical"){
        xpos<-drawCatPositions(IV2$ncats)
        for (i in 1:IV2$ncats) {
          use1=(x2plot==i)
          if (DV$type=="Interval"){
            mn1=mean(dv[use1])
            sd1=sd(dv[use1])
            x2plot[use1]<-xpos[i]+rnorm(length(x2plot[use1]),mean=0,sd=exp(-0.5*((dv[use1]-mn1)/sd1)^2))*0.15
          } else {
            x2plot[use1]<-xpos[i]+rnorm(length(x2plot[use1]))*mean(use1)*0.3
          }
        }
      }
    } else {x2plot=iv2}
    
    switch(DV$type,
           "Interval"={yplot<-dv},
           "Categorical"={yplot<-match(dv,levels(dv))}
    )
    
    if (DV$type=="Categorical"){
      ypos<-drawCatPositions(DV$ncats)
      for (i in 1:DV$ncats) {
        use1=(yplot==i)
        if (IV$type=="Interval"){
          mn1<-mean(iv[use1])
          sd1<-sd(iv[use1])
          jitter<-rnorm(length(yplot[use1]),mean=0,sd=exp(-0.5*((iv[use1]-mn1)/sd1)^2))*0.15
          yplot[use1]<-ypos[i]+jitter
        } else{
          jitter<-rnorm(length(yplot[use1]),0,1)*mean(use1)*0.3
          yplot[use1]<-ypos[i]+jitter
        }
      }
      # yplot<-yplot-(DV$ncats+1)/2
    }
  } 
  
  sample<-list(participant=id, iv=iv,iv2=iv2, dv=dv,ivplot=xplot,iv2plot=x2plot,dvplot=yplot,sampleRho=sampleRho,samplePval=samplePval,IVs=IVs,IV2s=IV2s, DVs=DVs)
  sample
}
