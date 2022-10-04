max_bins=51
max_range=0.975
npops=79
use_prior=FALSE

zdens2rdens<-function(zdens,rvals){
  zdens/(1-rvals^2)
}

zSamplingDistr<-function(zvals,zmu,n){
  s=1/sqrt(n-3)
  1/s/sqrt(2*pi)*exp(-0.5*((zvals-zmu)/s)^2)
}

zpriorDistr<-function(zvals,Population_distr,PopulationRZ,k){
  switch (paste0(Population_distr,"_",PopulationRZ),
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

rpopDistr<-function(rvals,Population_distr,PopulationRZ,k){
  switch (paste0(Population_distr,"_",PopulationRZ),
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
            rvals*0+1*0.2
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

rSamp2Pop<-function(r_s,n,world=NULL) {
  if (is.null(world)) {world<-list(populationPDF="Uniform",populationRZ="r",populationPDFk<-0)}
  k<-world$populationPDFk
  z_s<-atanh(r_s)
  switch(world$populationPDF,
         "Uniform"={mlEst<-z_s},
         "Single"={mlEst<-z_s},
         "Gauss"={mlEst<-z_s*k^2*(n-3)/(k^2*(n-3) + 1)},
         "Exp"={
           overEst<-1/k/(n-3)
           if (z_s<0) {
             mlEst<-min(z_s+overEst,0)
           } else {
             mlEst<-max(z_s-overEst,0)
           }
           }
         )
  tanh(mlEst)
}
  
populationDensityFunction<-function(rpw,likelihood){
  if (likelihood$type=="Populations") {
    switch (likelihood$Use,
            "none" ={Prior<-list(populationPDF="Uniform",populationRZ="r",populationPDFk=0,populationNullp=0)},
            "world"={Prior<-likelihood$world},
            "prior"={Prior<-likelihood$prior}
            )
  } else {
    Prior<-likelihood$world
  }
  rpw_dens<-rpopDistr(rpw,Prior$populationPDF,Prior$populationRZ,Prior$populationPDFk)
  rpw_dens<-rpw_dens*(1-Prior$populationNullp)
  if ((Prior$populationPDF=="Single") && Prior$populationNullp>0) {
    use<-which.min(abs(rpw-0))
    rpw_dens[use]<-Prior$populationNullp
  }
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

get_pRho<-function(world) {
  if (!world$worldOn) {
    world$populationPDF="Single"
    world$populationRZ="r"
  }
  if (world$populationPDF=="Single") {
    if (world$populationRZ=="r") {
      pRho<-world$populationPDFk
    } else {
      pRho<-tanh(world$populationPDFk)
    }
    pRhogain<-1
    if (world$populationNullp) {
      pRho<-c(0,pRho)
      pRhogain<-c(world$populationNullp,1-world$populationNullp)
    }
  } else {
    pRho<-seq(-1,1,length.out=npops)*max_range
    pRhogain<-zdens2rdens(zpriorDistr(atanh(pRho),world$populationPDF,world$populationRZ,world$populationPDFk),pRho)
  }
 list(pRho=pRho,pRhogain=pRhogain)  
}

getNDist<-function(nvals,design,logScale) {
  n<-design$sN
  if (design$sNRand) {
    ng<-dgamma(nvals-5,shape=design$sNRandK,scale=(design$sN-5)/design$sNRandK)
  } else {
    ng<-nvals*0
    use<-which.min(abs(nvals-design$sN))
    ng[use]<-1
  }
  if (logScale) ng<-ng*nvals
  ng
}

fullRPopulationDist<-function(rvals,world) {
  rpopDistr(rvals,world$populationPDF,world$populationRZ,world$populationPDFk)
}

fullRSamplingDist<-function(vals,world,design,doStat="r",logScale=FALSE) {
  # sampling distribution from specified populations (pRho)
  pR<-get_pRho(world)
  # distribution of sample sizes
  n<-design$sN
  ng<-1
  if (design$sNRand) {
    n<-5+seq(0,5,1/n)*n
    ng<-dgamma(n-5,shape=design$sNRandK,scale=(design$sN-5)/design$sNRandK)
  }
  
  sDens_r<-c()
  for (ei in 1:length(pR$pRho)){
      d<-0
      for (ni in 1:length(n)) {
        switch (doStat,
                "r"={
                  addition<-rSamplingDistr(vals,pR$pRho[ei],n[ni])
                  if (logScale) addition<-addition*vals
                },
                "p"={
                  rp<-tanh(qnorm(1-vals/2)/sqrt(n[ni]-3))
                  addition<-rSamplingDistr(rp,pR$pRho[ei],n[ni])+rSamplingDistr(-rp,pR$pRho[ei],n[ni])
                  dzp<-exp(-erfcinv(vals)^2)
                  a<-addition[1]
                  addition<-addition/dzp*(1-rp^2)
                  addition[1]<-a
                  if (logScale) addition<-addition*vals
                },
                "log(lr)"={
                  # z^2*(n-3)/2
                  rp<-tanh(sqrt(vals*2/(n[ni]-3)))
                  addition<-rSamplingDistr(rp,pR$pRho[ei],n[ni])+rSamplingDistr(-rp,pR$pRho[ei],n[ni])
                  dzs<-vals*(n[ni]-3)
                  a<-addition[1]
                  addition<-addition/dzs*(1-rp^2)
                  addition[1]<-a
                  if (logScale) addition<-addition*vals
                },
                "w"={
                  rp<-seq(0,1,length.out=101)
                  zp<-atanh(rp)
                  wp<-pnorm(qnorm(alpha/2)+zp*sqrt(n[ni]-3)) + pnorm(qnorm(alpha/2)-zp*sqrt(n[ni]-3))
                  addition<-rSamplingDistr(rp,pR$pRho[ei],n[ni])+rSamplingDistr(-rp,pR$pRho[ei],n[ni])
                  dwz<-dnorm(zp,qnorm(alpha/2)/sqrt(n[ni]-3),1/sqrt(n[ni]-3)) -
                    dnorm(zp,-qnorm(alpha/2)/sqrt(n[ni]-3),1/sqrt(n[ni]-3))
                  a<-addition[1]
                  addition<-addition/dwz*(1-rp^2)
                  addition[1]<-a
                  use<-which(diff(wp)!=0)
                  addition<-approx(wp[c(1,use+1)],addition[c(1,use+1)],vals)$y
                  if (logScale) addition<-addition*vals
                },
                "nw"={ 
                  zp<-(qnorm(0.8)-qnorm(alpha))/sqrt(vals-3)
                  rp<-tanh(zp)
                  addition<-rSamplingDistr(rp,pR$pRho[ei],n[ni])+rSamplingDistr(-rp,pR$pRho[ei],n[ni])
                  dznw<- -zp/(vals-3)/2
                  addition<-addition*dznw*(1-rp^2)
                  if (logScale) addition<-addition*vals
                },
                "wp"={
                  rp<-seq(0,1,length.out=101)
                  zp<-atanh(rp)
                  wp<-pnorm(qnorm(alpha/2)+zp*sqrt(n[ni]-3)) + pnorm(qnorm(alpha/2)-zp*sqrt(n[ni]-3))
                  addition<-fullRPopulationDist(rp,world)
                  dwz<-dnorm(zp,qnorm(alpha/2)/sqrt(n[ni]-3),1/sqrt(n[ni]-3)) -
                    dnorm(zp,-qnorm(alpha/2)/sqrt(n[ni]-3),1/sqrt(n[ni]-3))
                  a<-addition[1]
                  addition<-addition/dwz*(1-rp^2)
                  addition[1]<-a
                  use<-which(diff(wp)!=0)
                  addition<-approx(wp[c(1,use+1)],addition[c(1,use+1)],vals)$y
                  if (logScale) addition<-addition*vals
                }
        )
        d<-d+addition*ng[ni]
      }
    d<-d/sum(d,na.rm=TRUE)
    sDens_r<-rbind(sDens_r,d*pR$pRhogain[ei])
  }
  
  dr_gain<-max(sDens_r,na.rm=TRUE)
  sDens_r<-sDens_r/dr_gain
  sDens_r_sum<-colMeans(sDens_r)
  sDens_r_sum
}

likelihood_run <- function(IV,DV,effect,design,evidence,likelihood,doSample=TRUE){
  n<-likelihood$design$sampleN
  design$sN<-n

  # make the theoretical distributions
  # overall Sampling Distribution
  if (likelihood$world$populationPDF=="Single") {
    if (likelihood$world$populationRZ=="r") {
      pRho<-likelihood$world$populationPDFk
    } else {
      pRho<-tanh(likelihood$world$populationPDFk)
    }
    pRhogain<-1
    if (likelihood$world$populationNullp) {
      pRho<-c(0,pRho)
      pRhogain<-c(likelihood$world$populationNullp,1-likelihood$world$populationNullp)
    }
  } else {
    pRho<-seq(-1,1,length.out=npops)*max_range
    pRhogain<-zdens2rdens(zpriorDistr(atanh(pRho),likelihood$world$populationPDF,likelihood$world$populationRZ,likelihood$world$populationPDFk),pRho)
  }
  rs<-seq(-1,1,length=201)*max_range
  
  # sampling distribution from specified populations (pRho)
  sDens_r<-c()
  for (ei in 1:length(pRho)){
    if (design$sNRand) {
     d<-0
     for (ni in 5+seq(0,5,1/n)*n) {
       g<-dgamma(ni-5,shape=design$sNRandK,scale=(n-5)/design$sNRandK)
       d<-d+rSamplingDistr(rs,pRho[ei],ni)*g
     }
    } else {
      d<-rSamplingDistr(rs,pRho[ei],n)
    }
    d<-d/sum(d)
    sDens_r<-rbind(sDens_r,d*pRhogain[ei])
  }
  
  dr_gain<-max(sDens_r,na.rm=TRUE)
  sDens_r<-sDens_r/dr_gain
  sDens_r_sum<-colMeans(sDens_r)
  if (length(pRho)>21) {
    use<-seq(5,length(pRho)-4,3)
    use<-seq(4,length(pRho)-3,4)
    pRho<-pRho[use]
    pRhogain<-pRhogain[use]
    sDens_r<-sDens_r[use,]
  }
  sDens_r<-sDens_r/max(sDens_r)
  
  switch (likelihood$type,
          "Samples"={
            rs_stats<-densityFunctionStats(sDens_r_sum,rs)
          },
          "Populations"={
            if (is.null(likelihood$ResultHistory)) {
              sRho<-likelihood$targetSample
              n<-likelihood$design$sampleN
            } else {
              sRho<-likelihood$ResultHistory$r
              n<-likelihood$ResultHistory$n
            }
            if (likelihood$likelihoodCorrection) {
              nout<-ceil(likelihood$likelihoodSimSlice*sqrt(likelihood$design$sampleN-3))*20+1
              correction<-seq(-1,1,length.out=nout)*likelihood$likelihoodSimSlice
            }  else {
              correction<-0
            }
            rp<-seq(-1,1,length=201)*max_range
            
            # likelihood function for each sample (there's usually only 1)
            pDens_z<-1
            sDens_z<-c()
            for (ei in 1:length(sRho)){
              zDens<-0
              for (ci in 1:length(correction)) {
              zDens<-zDens+zSamplingDistr(atanh(rp),atanh(sRho[ei]+correction[ci]),n[ei])
              }
              sDens_z<-rbind(sDens_z,zDens/length(correction))
              pDens_z <- pDens_z * zDens/length(correction)
            }
            pDens_z<-pDens_z
            # times the a-priori distribution
            switch(likelihood$Use,
                   "none"={
                     apDens_z<-zpriorDistr(atanh(rp),"Uniform","r",0)
                     rMLE<-rSamp2Pop(sRho[1],n)
                   },
                   "world"={
                     apDens_z<-zpriorDistr(atanh(rp),likelihood$world$populationPDF,likelihood$world$populationRZ,likelihood$world$populationPDFk)
                     if (likelihood$world$populationNullp>0) {
                       use1<-which(rp==0)
                       gain<-sum(apDens_z)*likelihood$world$populationNullp
                       apDens_z[use1]<-apDens_z[use1]+gain
                     }
                     rMLE<-rSamp2Pop(sRho[1],n, likelihood$world)
                     # apDens_z<-zpriorDistr(atanh(rp),likelihood$prior$populationPDF,likelihood$prior$populationPDFk)
                     # if (likelihood$prior$populationNullp>0) {
                     #   use1<-which(rp==0)
                     #   gain<-sum(apDens_z)*likelihood$prior$populationNullp
                     #   apDens_z[use1]<-apDens_z[use1]+gain
                     # }
                   },
                   "prior"={
                     apDens_z<-zpriorDistr(atanh(rp),likelihood$prior$populationPDF,likelihood$prior$populationRZ,likelihood$prior$populationPDFk)
                     if (likelihood$prior$populationNullp>0) {
                       use1<-which(rp==0)
                       gain<-sum(apDens_z)*likelihood$prior$populationNullp
                       apDens_z[use1]<-apDens_z[use1]+gain
                     }
                     rMLE<-rSamp2Pop(sRho[1],n, likelihood$prior)
                   }
                   )
            if (likelihood$Use!="none") {
              # apDens_z<-zpriorDistr(atanh(rp),likelihood$prior$populationPDF,likelihood$prior$populationRZ,likelihood$prior$populationPDFk)
              # if (likelihood$prior$populationNullp>0) {
              #   use1<-which(rp==0)
              #   gain<-sum(apDens_z)*likelihood$prior$populationNullp
              #   apDens_z[use1]<-apDens_z[use1]+gain
              # }
              pDens_z<-pDens_z*apDens_z
            } else {apDens_z<-1}
            
            # convert from z to r
            spDens_r<-sDens_z
            for (ei in 1:length(sRho)){
              spDens_r[ei,]<-zdens2rdens(sDens_z[ei,]*apDens_z,rp)
              spDens_r[ei,]<-spDens_r[ei,]/max(spDens_r[ei,])
            }
            pDens_r<-zdens2rdens(pDens_z,rp)
            if (!is.na(sRho[1])) {
              dr_gain<-max(pDens_r,na.rm=TRUE)
              pDens_r<-pDens_r/dr_gain
            }
            rp_stats<-densityFunctionStats(pDens_r,rp) 
            rp_stats$peakC<-rMLE

            dens_at_peak=1
            # dens_at_sample<-prod(rSamplingDistr(sRho,sRho,n))*rpopDistr(mean(sRho),likelihood$world$populationPDF,likelihood$world$populationRZ,likelihood$world$populationPDFk)/dr_gain
            # if (likelihood$world$populationNullp>0) {
            #   dens_at_sample<-dens_at_sample*(1-likelihood$world$populationNullp)
            # }
            if (is.na(sRho[1])) {
              dens_at_sample<-NA
              dens_at_population<-NA
              dens_at_zero<-NA
            } else {
            dens_at_sample<-approx(rp,pDens_r,sRho[1])$y
            dens_at_population<-approx(rp,pDens_r,ResultHistory$rp[1])$y
            dens_at_zero<-approx(rp,pDens_r,0)$y
            }
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
              if (likelihood$likelihoodLongHand){
                effect1<-effect
                effect1$rIV<-pRho[i]
                effect1$populationPDF<-"Single"
                res<-multipleAnalysis(IV,NULL,DV,effect1,design,evidence,nsims,appendData=FALSE, earlierResult=c(),sigOnly=FALSE,
                                      showProgress=TRUE,progressPrefix=paste0("Possible Samples ",format(i),"/",format(length(pRho)),":"))
                r_effects<-rbind(r_effects,t(res$rIV))
              } else {
                if (design$sNRand) {
                  ns<-5+rgamma(nsims,shape=design$sNRandK,scale=(design$sN-5)/design$sNRandK)
                  s1<-1/sqrt(ns-3)
                  r_effects<-rbind(r_effects,tanh(rnorm(nsims,mean=atanh(pRho[i]),sd=s1)))
                } else {
                  r_effects<-rbind(r_effects,tanh(rnorm(nsims,mean=atanh(pRho[i]),sd=s)))
                }
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
              sRho<-likelihood$targetSample
              
              if (likelihood$likelihoodLongHand){
                res<-multipleAnalysis(IV,NULL,DV,effect,design,evidence,nsims*sample_increase,appendData=FALSE, earlierResult=c(),sigOnly=FALSE,
                                      showProgress=TRUE,progressPrefix=paste0("Possible Populations :"))
                r_effects<-res$rIV
                pops<-res$rpIV
              } else {
                # make some population values according to the specified a priori distribution
                switch (paste0(likelihood$world$populationPDF,"_",likelihood$world$populationRZ),
                        "Single_r"={
                          pops<-rep(likelihood$world$populationPDFk,nsims*sample_increase)
                        },
                        "Single_z"={
                          pops<-rep(likelihood$world$populationPDFk,nsims*sample_increase)
                          pops<-tanh(pops)
                        },
                        "Exp_z"={
                          pops<-rexp(nsims*sample_increase,rate=1/likelihood$world$populationPDFk)
                          pops<-tanh(pops)
                          pops<-pops*sign(rnorm(length(pops)))
                        },
                        "Exp_r"={
                          pops<-rexp(nsims*1.5*sample_increase,rate=1/likelihood$world$populationPDFk)
                          pops<-pops[pops<1]
                          if (length(pops)>nsims*sample_increase) {
                            pops<-pops[1:(nsims*sample_increase)]
                          }
                          pops<-pops*sign(rnorm(length(pops)))
                        },
                        "Gauss_z"={
                          pops<-rnorm(nsims*sample_increase,sd=likelihood$world$populationPDFk)
                          pops<-tanh(pops)
                        },
                        "Gauss_r"={
                          pops<-rnorm(nsims*sample_increase,sd=likelihood$world$populationPDFk)
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
                if (likelihood$world$populationNullp>0) {
                  change<-round(likelihood$world$populationNullp*length(pops))
                  pops[1:change]<-0
                }
                # make some sample sizes
                if (design$sNRand) {
                  ns<-5+rgamma(nsims*sample_increase,shape=design$sNRandK,scale=(design$sN-5)/design$sNRandK)
                  s1<-1/sqrt(ns-3)
                  r_effects<-tanh(rnorm(nsims*sample_increase,mean=atanh(pops),sd=s1))
                } else {
                  r_effects<-tanh(rnorm(nsims*sample_increase,mean=atanh(pops),sd=s))
                }
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
              # pr_effectP<-likelihood_P_ResultHold$pSims
              # pr_effectS<-likelihood_P_ResultHold$sSims
            }
            if (!isempty(pr_effectS)) {
              keep<-abs(pr_effectS-sRho)<likelihood$likelihoodSimSlice
              pr_effectP_use<-pr_effectP[keep]
              if (likelihood$world$populationPDF=="Single") {
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
            likelihoodResult<-list(likelihood=likelihood,
                                   pRho=pRho,
                                   sRho=likelihood$targetSample,
                                   n=n,
                                   Theory=list(
                                     rs=rs,sDens_r=sDens_r,sDens_r_sum=sDens_r_sum,
                                     rs_sd=rs_stats$sd,
                                     rs_ci=rs_stats$ci,
                                     rs_peak=rs_stats$peak
                                   ),
                                   Sims=list(
                                     sSims=sr_effects,sSimBins=sSimBins,sSimDens=sSimDens,
                                     rsSim_sd=rsSim_sd,
                                     rsSim_ci=rsSim_ci,
                                     rsSim_peak=rsSim_peak
                                   )
            )
          },
          "Populations"={
            likelihoodResult<-list(likelihood=likelihood,
                                   pRho=likelihood$targetPopulation,
                                   sRho=sRho,
                                   n=n,
                                   Theory=list(
                                     rs=rs,sDens_r=sDens_r,sDens_r_sum=sDens_r_sum,
                                     rp=rp,pDens_r=pDens_r,spDens_r=spDens_r,
                                     rp_sd=rp_stats$sd,
                                     rp_ci=rp_stats$ci,
                                     rp_peak=rp_stats$peak,
                                     rp_peakC=rp_stats$peakC,
                                     dens_at_peak=dens_at_peak,dens_at_sample=dens_at_sample,
                                     dens_at_population=dens_at_population,dens_at_zero=dens_at_zero
                                   ),
                                   Sims=list(
                                     pSims=pr_effectS,pSimsP=pr_effectP,pSimBins=pSimBins,pSimDens=pSimDens,
                                     pSimDensS=pSimDensS,pSimDensP=pSimDensP,
                                     rpSim_sd=rpSim_sd,
                                     rpSim_ci=rpSim_ci,
                                     rpSim_peak=rpSim_peak
                                   )
            )
          }
  )
}
