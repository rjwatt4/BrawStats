longHand=FALSE # do real simulations
max_bins=51

zdens2rdens<-function(zdens,rvals){
  zdens/(1-rvals^2)
}

zSamplingDistr<-function(zvals,zmu,n){
  s=1/sqrt(n-3)
  1/s/sqrt(2*pi)*exp(-0.5*((zvals-zmu)/s)^2)
}

rSamplingDistr<-function(rvals,rmu,n){
  # map to Fisher-z
  zvals<-atanh(rvals)
  zmu<-atanh(rmu)
  zdens<-zSamplingDistr(zvals,zmu,n)
  zdens2rdens(zdens,rvals)
}

rPopulationDistr<-function(rvals,Population_distr,k){
  # this is a bit odd, but...
  # the uniform option means uniform in r
  # the exp option means exp in z
  switch (Population_distr,
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
  rpw_dens<-rPopulationDistr(rpw,likelihood$populationDist,likelihood$populationDistK)
  rpw_dens    
}


densityFunctionStats<-function(dens_r,rp){

  use<-!is.na(dens_r)
  cum_dens_r<-cumsum(dens_r[use])/sum(dens_r[use])
  cum_rp<-rp[use]
  keep<-(cum_dens_r>0 & cum_dens_r<1)
  
  list(
    peak=rp[which.max(dens_r)],
    sd=sqrt(sum((rp)^2*dens_r,na.rm=TRUE)/sum(dens_r,na.rm=TRUE)),
    ci=approx(cum_dens_r[keep],cum_rp[keep]+(rp[2]-rp[1])/2,c(0.025,0.975))
  )


}

likelihoodRun <- function(IV,DV,effect,design,evidence,likelihood,doSample=TRUE){
  n<-design$sN
  
  # make the theoretical distribution        
  switch (likelihood$type,
          "Samples"={
            pRho<-likelihood$populationES
            rs<-seq(-1,1,length=201)    
            
            # sampling distribution from specified populations (pRho)
            sDens_r<-0
            for (ei in 1:length(pRho)){
              sDens_r<-sDens_r+rSamplingDistr(rs,pRho,n)
            }
            
            dr_gain<-max(sDens_r,na.rm=TRUE)
            sDens_r<-sDens_r/dr_gain
            rs_stats<-densityFunctionStats(sDens_r,rs)
          },
          "Populations"={
            sRho<-likelihood$sampleES
            rp<-seq(-1,1,length=201)
            
            # likelihood function for each sample (there's really only 1)
            pDens_r<-1
            for (ei in 1:length(sRho)){
              pDens_r <- pDens_r * rSamplingDistr(rp,sRho[ei],n)
            }
            # times the a-priori distribution
            apDens<-rPopulationDistr(rp,likelihood$populationDist,likelihood$populationDistK)
            if (likelihood$likelihoodNullp>0) {
              use1<-which(rp==0)
              gain<-1/sum(apDens)*(1-likelihood$likelihoodNullp)
              rp<-rp[c(1:use1,use1,use1:length(rp))]
              apDens<-c(apDens[1:use1]*gain,likelihood$likelihoodNullp,apDens[use1:length(apDens)]*gain)
              pDens_r<-pDens_r[c(1:use1,use1,use1:length(pDens_r))]
            }
            pDens_r<-pDens_r*apDens
            dr_gain<-max(pDens_r,na.rm=TRUE)
            pDens_r<-pDens_r/dr_gain
            rp_stats<-densityFunctionStats(pDens_r,rp)
            
            r_at_peak_dens=1
            expected_r_at_peak_dens<-prod(rSamplingDistr(sRho,sRho,n))*rPopulationDistr(mean(sRho),likelihood$populationDist,likelihood$populationDistK)/dr_gain
          }
  )
  
  

  # simulations
  sr_effects<-NULL
  sSimBins<-NULL
  sSimDens<-NULL
  rsSim_sd<-NULL
  rsSim_ci=NULL
  rsSim_peak=NULL
  
  pr_effects<-NULL
  pSimBins<-NULL
  pSimDens<-NULL
  rpSim_sd<-NULL
  rpSim_ci=NULL
  rpSim_peak=NULL
  
  if (doSample) {
  nsims=likelihood$Likelihood_length
  
  s=1/sqrt(n-3)
  
  switch (likelihood$type,
          "Samples"={
            if (longHand){
              effect$rIV<-pRho
              res<-multipleAnalysis(IV,NULL,DV,effect,design,evidence,nsims)
              r_effects<-res$rIV
            } else {
              r_effects<-tanh(rnorm(nsims,mean=atanh(pRho),sd=s))
            }
            if (likelihood$appendSim){
              sr_effects<-c(likelihood_S_ResultHold,r_effects)
            } else {
              sr_effects<-r_effects
            }
            binWidth<-2*IQR(sr_effects)/length(sr_effects)^(1/3)
            nbins=round(2/binWidth)
            sSimBins<-seq(-1,1,length.out=nbins+1)
            sSimDens<-hist(sr_effects,sSimBins,plot=FALSE)
            likelihood_S_ResultHold<<-sr_effects
            rsSim_ci=quantile(sr_effects,c(0.025,0.975))
            rsSim_peak=sSimBins[which.max(sSimDens$counts)]+sSimBins[2]-sSimBins[1]
            rsSim_sd<-sd(sr_effects,na.rm=TRUE)
            
          },
          
          "Populations"={
            sample_increase=1;
            # make some population values according to the specified a priori distribution
            switch (likelihood$populationDist,
                    "Exp_z"={
                      pops<-rexp(nsims*sample_increase,rate=1/likelihood$populationDistK)*sign(rnorm(nsims))
                      pops<-tanh(pops)
                    },
                    "Exp_r"={
                      pops<-rexp(nsims*sample_increase,rate=1/likelihood$populationDistK)
                      pops<-pops[pops<1]
                      pops<-pops*sign(rnorm(nsims))
                    },
                    "Gauss_z"={
                      pops<-rnorm(nsims*sample_increase,sd=likelihood$populationDistK)
                      pops<-tanh(pops)
                    },
                    "Gauss_r"={
                      pops<-rnorm(nsims*sample_increase,sd=likelihood$populationDistK)
                      pops<-pops[abs(pops)<1]
                    },
                    "Uniform_r"={
                      pops<-runif(nsims*sample_increase,min=-1,max=1)
                    },
                    "Uniform_z"={
                      pops<-runif(nsims*sample_increase,min=-100,max=1100)
                      pops<-tanh(pops)
                    }
            )
            if (likelihood$likelihoodNullp>0) {
              change<-round(likelihood$likelihoodNullp*length(pops))
              pops[1:change]<-0
            }

            if (longHand){
              effect$rIV<-pops
              res<-multipleAnalysis(IV,NULL,DV,effect,design,evidence,length(pops))
              r_effects<-res$rIV
            } else {
              r_effects<-tanh(rnorm(nsims*sample_increase,mean=atanh(pops),sd=s))
            }
            keep<-abs(r_effects-sRho)<0.1
            if (likelihood$appendSim){
              pr_effects<-c(likelihood_P_ResultHold,pops[keep])
            } else {
              pr_effects<-pops[keep]
            }
            binWidth<-2*IQR(pr_effects)/length(pr_effects)^(1/3)
            nbins=round(2/binWidth)
            pSimBins<-seq(-1,1,length.out=nbins+1)
            pSimDens<-hist(pr_effects,pSimBins,plot=FALSE)
            likelihood_P_ResultHold<<-pr_effects
            rpSim_ci=quantile(pr_effects,c(0.025,0.975))
            rpSim_peak=pSimBins[which.max(pSimDens$counts)]+pSimBins[2]-pSimBins[1]
            rpSim_sd<-sd(pr_effects,na.rm=TRUE)
          }
  )
  }

  switch (likelihood$type,
          "Samples"={
            likelihoodResult<-list(likelihood=likelihood,pRho=likelihood$populationES,sRho=likelihood$sampleES,sr=sr_effects,
                                   rs=rs,sDens_r=sDens_r,
                                   rs_sd=rs_stats$sd,
                                   rs_ci=rs_stats$ci$y,
                                   rs_peak=rs_stats$peak,
                                   sSims=sr_effects,sSimBins=sSimBins,sSimDens=sSimDens,
                                   rsSim_sd=rsSim_sd,
                                   rsSim_ci=rsSim_ci,
                                   rsSim_peak=rsSim_peak
            )
          },
          "Populations"={
            likelihoodResult<-list(likelihood=likelihood,pRho=likelihood$populationES,sRho=likelihood$sampleES,sr=pr_effects,
                                   rp=rp,pDens_r=pDens_r,
                                   rp_sd=rp_stats$sd,
                                   rp_ci=rp_stats$ci$y,
                                   rp_peak=rp_stats$peak,
                                   r_at_peak_dens=r_at_peak_dens,expected_r_at_peak_dens=expected_r_at_peak_dens,
                                   pSims=pr_effects,pSimBins=pSimBins,pSimDens=pSimDens,
                                   rpSim_sd=rpSim_sd,
                                   rpSim_ci=rpSim_ci,
                                   rpSim_peak=rpSim_peak
            )
          }
  )
}
