longHand=TRUE
max_bins=51

# zdens2rdens<-function(z1,rvals,Population_distr){
#   if (Population_distr=="Uniform")
#   {z1}
#   else
#   {z1/(1-rvals^2)}
# }
# 
zdens2rdens<-function(zdens,rvals){
  zdens/(1-rvals^2)
}

zSamplingDistr<-function(zvals,zmu,n){
  s=1/sqrt(n-3)
  1/s/sqrt(2*pi)*exp(-0.5*((zvals-zmu)/s)^2)
}

zPopulationDistr<-function(zvals,Population_distr,k){
  if (Population_distr=="Uniform")
  {zvals*0+0.5}
  else
  {exp(-abs(zvals)/k)}
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
  if (Population_distr=="Uniform")
  {rvals*0+0.5}
  else
  {zvals<-atanh(rvals)
   zdens<-zPopulationDistr(zvals,Population_distr,k)
   zdens2rdens(zdens,rvals)
  }
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
  rho<-effect$rIV
  n<-design$sN
  pRho<-likelihood$populationES
  sRho<-likelihood$sampleES
  
# make the theoretical distribution        
  # samples
  rs<-seq(-1,1,length=200)    
  dens_r1<-rSamplingDistr(rs,pRho,n)
  sDens_r<-dens_r1
  dr_gain<-max(sDens_r,na.rm=TRUE)
  
  sDens_r<-sDens_r/dr_gain
  rs_stats<-densityFunctionStats(sDens_r,rs)
  
  # populations
  rp<-seq(-1,1,length=200)    
  dens_r1<-1
  for (ei in 1:length(sRho)){
    dens_r1 <- dens_r1 * rSamplingDistr(rp,sRho[ei],n)
  }
  pDens_r<-dens_r1*rPopulationDistr(rp,likelihood$populationDist,likelihood$populationDistK)
  dr_gain<-max(pDens_r,na.rm=TRUE)
  
  pDens_r<-pDens_r/dr_gain
  rp_stats<-densityFunctionStats(pDens_r,rp)
  
  r_at_peak_dens=1
  expected_r_at_peak_dens<-prod(rSamplingDistr(sRho,sRho,n))*rPopulationDistr(mean(sRho),likelihood$populationDist,likelihood$populationDistK)/dr_gain

  if (doSample) {
# simulations
  nsims=likelihood$Likelihood_length
  
  s=1/sqrt(n-3)
  
  switch (likelihood$type,
          "Samples"={
            pr_effects<-NULL
            pSimBins<-NULL
            pSimDens<-NULL
            rpSim_sd<-NULL
            rpSim_ci=NULL
            rpSim_peak=NULL
            
            if (longHand){
              effect$rIV<-pRho
              res<-multipleAnalysis(IV,NULL,DV,effect,design,evidence,nsims)
              sr<-res$rIV
            } else {
              sr<-tanh(rnorm(nsims,mean=atanh(pRho),sd=s))
            }
            if (likelihood$appendSim){
              sr_effects<-c(likelihoodSResultHold,sr)
            } else {
              sr_effects<-sr
            }
            binWidth<-2*IQR(sr_effects)/length(sr_effects)^(1/3)
            nbins=round(2/binWidth)
            sSimBins<-seq(-1,1,length.out=nbins+1)
            sSimDens<-hist(sr_effects,sSimBins,plot=FALSE)
            likelihoodSResultHold<<-sr_effects
            rsSim_ci=quantile(sr_effects,c(0.025,0.975))
            rsSim_peak=sSimBins[which.max(sSimDens$counts)]+sSimBins[2]-sSimBins[1]
            rsSim_sd<-sd(sr_effects,na.rm=TRUE)
            
          },
          "Populations"={
            sr_effects<-NULL
            sSimBins<-NULL
            sSimDens<-NULL
            rsSim_sd<-NULL
            rsSim_ci=NULL
            rsSim_peak=NULL
            
            sample_increase=10;
            switch (likelihood$populationDist,
                    "Exp"={
                      pops<-tanh(rexp(nsims*sample_increase,rate=1/likelihood$populationDistK))*sign(rnorm(nsims))
                    },
                    "Uniform"={
                      pops<-runif(nsims*sample_increase,min=-1,max=1)
                    }
                    )
            if (longHand){
              effect$rIV<-pops
              res<-multipleAnalysis(IV,NULL,DV,effect,design,evidence,length(pops))
              r_effects<-res$rIV
            } else {
              r_effects<-tanh(rnorm(nsims*sample_increase,mean=atanh(pops),sd=s))
            }
            keep<-abs(r_effects-sRho)<0.1
            if (likelihood$appendSim){
              pr_effects<-c(likelihoodPResultHold,pops[keep])
            } else {
              pr_effects<-pops[keep]
            }
            binWidth<-2*IQR(pr_effects)/length(pr_effects)^(1/3)
            nbins=round(2/binWidth)
            pSimBins<-seq(-1,1,length.out=nbins+1)
            pSimDens<-hist(pr_effects,pSimBins,plot=FALSE)
            likelihoodPResultHold<<-pr_effects
            rpSim_ci=quantile(pr_effects,c(0.025,0.975))
            rpSim_peak=pSimBins[which.max(pSimDens$counts)]+pSimBins[2]-pSimBins[1]
            rpSim_sd<-sd(pr_effects,na.rm=TRUE)
          }
  )

  likelihoodResult<-list(likelihood=likelihood,pRho=pRho,sRho=sRho,
                         rp=rp,rs=rs,pDens_r=pDens_r,sDens_r=sDens_r,
                         rp_sd=rp_stats$sd,rs_sd=rs_stats$sd,
                         rp_ci=rp_stats$ci$y,rs_ci=rs_stats$ci$y,
                         rp_peak=rp_stats$peak,rs_peak=rs_stats$peak,
                         r_at_peak_dens=r_at_peak_dens,expected_r_at_peak_dens=expected_r_at_peak_dens,
                         sSims=sr_effects,sSimBins=sSimBins,sSimDens=sSimDens,
                         pSims=pr_effects,pSimBins=pSimBins,pSimDens=pSimDens,
                         rpSim_sd=rpSim_sd,rsSim_sd=rsSim_sd,
                         rpSim_ci=rpSim_ci,rsSim_ci=rsSim_ci,
                         rpSim_peak=rpSim_peak,rsSim_peak=rsSim_peak
  )
  } else{
    likelihoodResult<-list(likelihood=likelihood,pRho=pRho,sRho=sRho,
                           rp=rp,rs=rs,pDens_r=pDens_r,sDens_r=sDens_r,
                           rp_sd=rp_stats$sd,rs_sd=rs_stats$sd,
                           rp_ci=rp_stats$ci$y,rs_ci=rs_stats$ci$y,
                           rp_peak=rp_stats$peak,rs_peak=rs_stats$peak,
                           r_at_peak_dens=r_at_peak_dens,expected_r_at_peak_dens=expected_r_at_peak_dens,
                           sSims=NULL,sSimBins=NULL,sSimDens=NULL,
                           pSims=NULL,pSimBins=NULL,pSimDens=NULL,
                           rpSim_sd=NULL,rsSim_sd=NULL,
                           rpSim_ci=NULL,rsSim_ci=NULL,
                           rpSim_peak=NULL,rsSim_peak=NULL
    )
  }
}
