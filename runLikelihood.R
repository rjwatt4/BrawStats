max_bins=51
max_range=0.95
npops=51
use_prior=FALSE

zdens2rdens<-function(zdens,rvals){
  zdens/(1-rvals^2)
}

zSamplingDistr<-function(zvals,zmu,n){
  s=1/sqrt(n-3)
  1/s/sqrt(2*pi)*exp(-0.5*((zvals-zmu)/s)^2)
}

zpriorDistr<-function(zvals,Population_distr,k){
  switch (Population_distr,
          "Single_r"={
            rvals<-tanh(zvals)
            zdens<-rvals*0
            zdens[which.min(abs(k-rvals))]<-1
            zdens
          },
          "Single_z"={
            zdens<-zvals*0
            zdens[which.min(abs(k-zvals))]<-1
            zdens
          },
          "Uniform_r"={
            rvals<-tanh(zvals)
            rvals*0+1*(1-rvals^2)
          },
          "Uniform_z"={
            zdens<-zvals*0+1
            zdens/5
          },
          "Exp_r"={
            rvals<-tanh(zvals)
            exp(-abs(rvals)/k)
          },
          "Exp_z"={
            zdens<-exp(-abs(zvals)/k)
          },
          "Gauss_r"={
            rvals<-tanh(zvals)
            exp(-0.5*(abs(rvals)/k)^2)
          },
          "Gauss_z"={
            zdens<-exp(-0.5*(abs(zvals)/k)^2)
          }
  )
}

rSamplingDistr<-function(rvals,rmu,n){
  # map to Fisher-z
  zvals<-atanh(rvals)
  zmu<-atanh(rmu)
  zdens<-zSamplingDistr(zvals,zmu,n)
  zdens2rdens(zdens,rvals)
}

rpriorDistr<-function(rvals,Population_distr,k){
  # this is a bit odd, but...
  # the uniform option means uniform in r
  # the exp option means exp in z
  switch (Population_distr,
          "Single_r"={
            rdens<-rvals*0
            rdens[which.min(abs(k-rvals))]<-1
            rdens
          },
          "Single_z"={
            zvals<-atanh(rvals)
            zdens<-zvals*0
            zdens[which.min(abs(k-zvals))]=1
            zdens<-zdens2rdens(zdens,rvals)
            zdens
          },
          "Uniform_r"={
            rvals*0+1
          },
          "Uniform_z"={
            zvals<-atanh(rvals)
            zdens<-zvals*0+1
            zdens<-zdens2rdens(zdens,rvals)
            zdens/5
          },
          "Exp_r"={
            exp(-abs(rvals)/k)
          },
          "Exp_z"={
            zvals<-atanh(rvals)
            zdens<-exp(-abs(zvals)/k)
            zdens2rdens(zdens,rvals)
          },
          "Gauss_r"={
            exp(-0.5*(abs(rvals)/k)^2)
          },
          "Gauss_z"={
            zvals<-atanh(rvals)
            zdens<-exp(-0.5*(abs(zvals)/k)^2)
            zdens2rdens(zdens,rvals)
          }
  )
}

populationDensityFunction<-function(rpw,likelihood){
  rpw_dens<-rpriorDistr(rpw,likelihood$priorDist,likelihood$priorDistK)
  rpw_dens    
}


densityFunctionStats<-function(dens_r,rp){
  use<-!is.na(dens_r)
  cum_dens_r<-cumsum(dens_r[use])/sum(dens_r[use])
  cum_rp<-rp[use]
  if (length(unique(cum_dens_r))<5) {
    ci<-c(-1,1)
  } else {
    if (any(cum_dens_r==0)) {
      use1<-max(which(cum_dens_r==0))
    } else {use1<-1}
    if (any(cum_dens_r==1)) {
      use2<-min(which(cum_dens_r==1))
    } else {use2<-length(cum_rp)}
    keep<-use1:use2
    if (length(keep)>=2) {
      ci<-approx(cum_dens_r[keep],cum_rp[keep]+(rp[2]-rp[1])/2,c(0.025,0.975))$y
    } else {
      ci<-c(-1,1)
    }
  }

  list(
    peak=rp[which.max(dens_r)],
    sd=sqrt(sum((rp)^2*dens_r,na.rm=TRUE)/sum(dens_r,na.rm=TRUE)),
    ci=ci
  )


}

likelihood_run <- function(IV,DV,effect,design,evidence,likelihood,doSample=TRUE){
  n<-likelihood$sampleN
  design$sN<-n

  # make the theoretical distributions
  # overall Sampling Distribution
  if (substr(likelihood$priorDist,1,6)=="Single") {
    if (substr(likelihood$priorDist,8,8)=="r") {
      pRho<-likelihood$priorDistK
    } else {
      pRho<-tanh(likelihood$priorDistK)
    }
    pRhogain<-1
  } else {
    pRho<-seq(-1,1,length.out=npops)*max_range
    pRhogain<-zdens2rdens(zpriorDistr(atanh(pRho),likelihood$priorDist,likelihood$priorDistK),pRho)
  }
  rs<-seq(-1,1,length=201)*max_range
  # sampling distribution from specified populations (pRho)
  sDens_r<-c()
  for (ei in 1:length(pRho)){
    d<-rSamplingDistr(rs,pRho[ei],n)
    d<-d/sum(d)
    # print(c(max(d),sum(d)))
    sDens_r<-rbind(sDens_r,d*pRhogain[ei])
  }
  dr_gain<-max(sDens_r,na.rm=TRUE)
  sDens_r<-sDens_r/dr_gain
  sDens_r_sum<-colMeans(sDens_r)
  if (length(pRho)>1) {
    use<-seq(5,length(pRho)-4,3)
    pRho<-pRho[use]
    sDens_r<-sDens_r[use,]
  }
  sDens_r<-sDens_r/max(sDens_r)
  
  switch (likelihood$type,
          "Samples"={
            rs_stats<-densityFunctionStats(sDens_r_sum,rs)
          },
          "Populations"={
            sRho<-likelihood$sampleES
            if (likelihood$likelihoodCorrection)
              correction<-seq(-1,1,length.out=5)*likelihood$likelihoodSimSlice
            else
              correction<-0
            rp<-seq(-1,1,length=201)*max_range
            
            # likelihood function for each sample (there's really only 1)
            pDens_z<-1
            for (ei in 1:length(sRho)){
              zDens<-0
              for (ci in 1:length(correction)) {
              zDens<-zDens+zSamplingDistr(atanh(rp),atanh(sRho+correction[ci]),n)
              }
              pDens_z <- pDens_z * zDens/length(correction)
            }
            pDens_z<-pDens_z
            # times the a-priori distribution
            if (likelihood$likelihoodUsePrior) {
            apDens_z<-zpriorDistr(atanh(rp),likelihood$priorDist,likelihood$priorDistK)
            if (likelihood$priorNullp>0) {
              use1<-which(rp==0)
              gain<-sum(apDens_z)*likelihood$priorNullp
              apDens_z[use1]<-apDens_z[use1]+gain
            }
            pDens_z<-pDens_z*apDens_z
            }
            
            # convert from z to r
            pDens_r<-zdens2rdens(pDens_z,rp)
            dr_gain<-max(pDens_r,na.rm=TRUE)
            pDens_r<-pDens_r/dr_gain
            rp_stats<-densityFunctionStats(pDens_r,rp)
            
            dens_at_peak=1
            # dens_at_sample<-prod(rSamplingDistr(sRho,sRho,n))*rpriorDistr(mean(sRho),likelihood$priorDist,likelihood$priorDistK)/dr_gain
            # if (likelihood$priorNullp>0) {
            #   dens_at_sample<-dens_at_sample*(1-likelihood$priorNullp)
            # }
            dens_at_sample<-approx(rp,pDens_r,sRho)$y
          }
  )
  
  

  # simulations
  sr_effects<-NULL
  sSimBins<-NULL
  sSimDens<-NULL
  rsSim_sd<-NULL
  rsSim_ci=NULL
  rsSim_peak=NULL
  
  pr_effectS<-NULL
  pr_effectP<-NULL
  pSimBins<-NULL
  pSimDens<-NULL
  pSimDensS<-NULL
  pSimDensP<-NULL
  rpSim_sd<-NULL
  rpSim_ci=NULL
  rpSim_peak=NULL
  
  # make the samples
  nsims=likelihood$Likelihood_length
  
  s=1/sqrt(n-3)
  
  switch (likelihood$type,
          "Samples"={
            if (doSample) {
              r_effects<-c()
            for (i in 1:length(pRho)) {
              if (likelihood$longHandLikelihood){
                effect$rIV<-pRho[i]
                res<-multipleAnalysis(IV,NULL,DV,effect,design,evidence,nsims,appendData=FALSE, earlierResult=c(), showProgress=TRUE,progressPrefix=paste0("Possible Samples ",format(i),"/",format(length(pRho)),":"))
                r_effects<-rbind(r_effects,t(res$rIV))
              } else {
                r_effects<-rbind(r_effects,tanh(rnorm(nsims,mean=atanh(pRho[i]),sd=s)))
              }
            }
            if (likelihood$appendSim){
              sr_effects<-cbind(likelihood_S_ResultHold$sSims,r_effects)
            } else {
              sr_effects<-r_effects
            }
            binWidth<-2*IQR(sr_effects)/length(sr_effects)^(1/3)
            nbins=round(2/binWidth)
            sSimBins<-seq(-1,1,length.out=nbins+1)
            sSimDens<-c()
            for (i in 1:length(pRho)) {
              h<-hist(sr_effects[i,],sSimBins,plot=FALSE)$counts
            sSimDens<-rbind(sSimDens,h*pRhogain[i])
            }
            likelihood_S_ResultHold<<-list(sSims=sr_effects,sSimBins=sSimBins,sSimDens=sSimDens)
            rsSim_ci=quantile(sr_effects,c(0.025,0.975))
            rsSim_peak=sSimBins[which.max(sSimDens)]+sSimBins[2]-sSimBins[1]
            rsSim_sd<-sd(sr_effects,na.rm=TRUE)
            } else {
              sr_effects<-likelihood_S_ResultHold$sSims
              sSimBins<-likelihood_S_ResultHold$sSimBins
              sSimDens<-likelihood_S_ResultHold$sSimDens
            }
          },
          
          "Populations"={
            if (doSample) {
              sample_increase=1;
              sRho<-likelihood$sampleES
              # make some population values according to the specified a priori distribution
              switch (likelihood$priorDist,
                      "Single_r"={
                        pops<-rep(likelihood$priorDistK,nsims*sample_increase)
                      },
                      "Single_z"={
                        pops<-rep(likelihood$priorDistK,nsims*sample_increase)
                        pops<-tanh(pops)
                      },
                      "Exp_z"={
                        pops<-rexp(nsims*sample_increase,rate=1/likelihood$priorDistK)*sign(rnorm(nsims))
                        pops<-tanh(pops)
                      },
                      "Exp_r"={
                        pops<-rexp(nsims*1.5*sample_increase,rate=1/likelihood$priorDistK)
                        pops<-pops[pops<1]
                        if (length(pops)>nsims*sample_increase) {
                          pops<-pops[1:(nsims*sample_increase)]
                        }
                        pops<-pops*sign(rnorm(length(pops)))
                      },
                      "Gauss_z"={
                        pops<-rnorm(nsims*sample_increase,sd=likelihood$priorDistK)
                        pops<-tanh(pops)
                      },
                      "Gauss_r"={
                        pops<-rnorm(nsims*sample_increase,sd=likelihood$priorDistK)
                        pops<-pops[abs(pops)<1]
                      },
                      "Uniform_r"={
                        pops<-runif(nsims*sample_increase,min=-1,max=1)
                      },
                      "Uniform_z"={
                        pops<-runif(nsims*sample_increase,min=-100,max=100)
                        pops<-tanh(pops)
                      }
              )
              if (likelihood$priorNullp>0) {
                change<-round(likelihood$priorNullp*length(pops))
                pops[1:change]<-0
              }
              
              if (likelihood$longHandLikelihood){
                effect$rIV<-pops
                res<-multipleAnalysis(IV,NULL,DV,effect,design,evidence,length(pops),appendData=FALSE, earlierResult=c(), showProgress=TRUE,progressPrefix=paste0("Possible Populations :"))
                r_effects<-res$rIV
              } else {
                r_effects<-tanh(rnorm(nsims*sample_increase,mean=atanh(pops),sd=s))
              }
              if (likelihood$appendSim){
                pr_effectP<-c(likelihood_P_ResultHold$pSims,pops)
                pr_effectS<-c(likelihood_P_ResultHold$sSims,r_effects)
              } else {
                pr_effectP<-pops
                pr_effectS<-r_effects
              }
              likelihood_P_ResultHold$pSims<<-pr_effectP
              likelihood_P_ResultHold$sSims<<-pr_effectS
            } else {
              pr_effectP<-likelihood_P_ResultHold$pSims
              pr_effectS<-likelihood_P_ResultHold$sSims
            }
            if (!isempty(pr_effectS)) {
              keep<-abs(pr_effectS-sRho)<likelihood$likelihoodSimSlice
              pr_effectP_use<-pr_effectP[keep]
              if (likelihood$priorDist=="Single_r" || likelihood$priorDist=="Single_z") {
                binWidth<-0.05
              } else {
                binWidth<-2*IQR(pr_effectP_use)/length(pr_effectP_use)^(1/3)
              }
              nbins=max(10,round(2/binWidth))
              pSimBins<-seq(-1,1,length.out=nbins+1)
              pSimDens<-hist(pr_effectP_use,pSimBins,plot=FALSE)
              rpSim_ci=quantile(pr_effectP_use,c(0.025,0.975))
              rpSim_peak=pSimBins[which.max(pSimDens$counts)]+pSimBins[2]-pSimBins[1]
              rpSim_sd<-sd(pr_effectP_use,na.rm=TRUE)
              
              pSimDensS<-hist(pr_effectS,pSimBins,plot=FALSE)
              pSimDensP<-hist(pr_effectP,pSimBins,plot=FALSE)
            }
          }
  )

  switch (likelihood$type,
          "Samples"={
            likelihoodResult<-list(likelihood=likelihood,pRho=pRho,sRho=likelihood$sampleES,sr=sr_effects,
                                   rs=rs,sDens_r=sDens_r,sDens_r_sum=sDens_r_sum,
                                   rs_sd=rs_stats$sd,
                                   rs_ci=rs_stats$ci,
                                   rs_peak=rs_stats$peak,
                                   sSims=sr_effects,sSimBins=sSimBins,sSimDens=sSimDens,
                                   rsSim_sd=rsSim_sd,
                                   rsSim_ci=rsSim_ci,
                                   rsSim_peak=rsSim_peak
            )
          },
          "Populations"={
            likelihoodResult<-list(likelihood=likelihood,pRho=likelihood$populationES,sRho=likelihood$sampleES,
                                   sr=pr_effectS,pr=pr_effectP,
                                   rs=rs,sDens_r=sDens_r,sDens_r_sum=sDens_r_sum,
                                   rp=rp,pDens_r=pDens_r,
                                   rp_sd=rp_stats$sd,
                                   rp_ci=rp_stats$ci,
                                   rp_peak=rp_stats$peak,
                                   dens_at_peak=dens_at_peak,dens_at_sample=dens_at_sample,
                                   pSims=pr_effectS,pSimBins=pSimBins,pSimDens=pSimDens,
                                   pSimDensS=pSimDensS,pSimDensP=pSimDensP,
                                   rpSim_sd=rpSim_sd,
                                   rpSim_ci=rpSim_ci,
                                   rpSim_peak=rpSim_peak
            )
          }
  )
}
