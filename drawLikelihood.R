colSdark="#227733"
colSsim="#66BB77"
colS="#88DD99"
colPdark="#223377"
colPsim="#6677BB"
colP="#8899DD"
highTransparency=0.25

addTransparency <- function(col,alpha) {
  col<-col2rgb(col)/255
  rgb(col[1],col[2],col[3],alpha)
}

drawLikelihood <- function(IV,DV,effect,design,likelihood,likelihoodResult){
  # make the distribution        
  n<-design$sN

  switch (likelihood$type,
  "Samples"={
    likelihoodResult<-likelihoodResult$samples
    ylab<-"Probability"
    },
  "Populations"={
    likelihoodResult<-likelihoodResult$populations
    ylab<-"Likelihood"
  }
  )

  pRho<-likelihoodResult$pRho
  sRho<-mean(likelihood$sampleES)

  switch (likelihood$type,
          "Samples"={
            col=colP
            col2=colS
          },
          "Populations"={
            col=colS
            col2=colP
          }
          )
  
  # graph frame
  switch (likelihood$view,
          "3D"= {
  # make the floor
            x<-seq(-1,1,length=2)    
            y<-x
            f <- function(x, y) { x*0+y*0 }
            z <- outer(x, y, f)
            z[is.na(z)] <- 0
            par(bg=maincolours$graphC,mar=c(0,5,0,0),font.lab=2)
            persp(x, y, z, zlim = range(c(0,1), na.rm = TRUE),
                  theta = likelihood$azimuth, phi = likelihood$elevation, r=likelihood$range, 
                  ticktype = "detailed", 
                  expand = 0.5, col = "#aaaaaa",
                  cex.axis=0.6,
                  xlab = "Populations", ylab = "Samples", zlab = ylab
            )->mapping
            
            
            # lines on the floor
            lines(trans3d(x=c(-1,1),y=c(0,0),z=c(0,0),pmat=mapping),col="black",lty=3)
            lines(trans3d(x=c(0,0),y=c(-1,1),z=c(0,0),pmat=mapping),col="black",lty=3)
            lines(trans3d(x=c(-1,1),y=c(sRho,sRho),z=c(0,0),pmat=mapping),col=colSdark)
            for (i in 1:length(pRho)) {
            lines(trans3d(x=c(pRho[i],pRho[i]),y=c(-1,1),z=c(0,0),pmat=mapping),col=colP)
            }
            
            # make the back walls
            rpw<-seq(-1,1,length=200)*0.99
            rpw_dens<-populationDensityFunction(rpw,likelihood)
            rpw_dens[rpw_dens>1 | is.na(rpw_dens)]<-1
            if (is.null(likelihoodResult$pSims)) {
              polygon(trans3d(x=c(rpw[1],rpw,rpw[length(rpw)]),y=c(1,rpw*0+1,1),z=c(0,rpw_dens,0)*(1-likelihood$priorNullp),pmat=mapping),col=colPdark)
            } else {
              polygon(trans3d(x=c(rpw[1],rpw,rpw[length(rpw)]),y=c(1,rpw*0+1,1),z=c(0,rpw_dens,0)*(1-likelihood$priorNullp),pmat=mapping),col=NA)
            }
            
            # if (likelihood$type=="Samples") {
              likelihoodResult$rs
              rsw<-likelihoodResult$rs
              rsw_dens<-likelihoodResult$sDens_r_sum
              x <- -c(1,rsw*0+1,1)
              y <- c(rsw[1],rsw,rsw[length(rsw)])
              z <- c(0,rsw_dens/max(rsw_dens,na.rm=TRUE),0)*(1-likelihood$priorNullp)
              polygon(trans3d(x=x,y=y,z=z,pmat=mapping),col=colSdark)
            # }
              
              if (likelihood$showTheory){
                # horizontal lines
                switch (likelihood$type,
                        "Samples"={
                          rs<-likelihoodResult$rs
                        },
                        "Populations"={
                          rp_peak<-likelihoodResult$rp_peak
                          rp_ci<-likelihoodResult$rp_ci
                          dens_at_peak<-likelihoodResult$dens_at_peak
                          dens_at_sample<-likelihoodResult$dens_at_sample
                          rp<-likelihoodResult$rp
                          
                          lines(trans3d(x=c(rp_ci[1],rp_ci[1]),y=c(-1,1),z=c(0,0),pmat=mapping),col="red",lty=3,lwd=2)
                          lines(trans3d(x=c(rp_ci[2],rp_ci[2]),y=c(-1,1),z=c(0,0),pmat=mapping),col="red",lty=3,lwd=2)
                          lines(trans3d(x=c(rp_peak,rp_peak),y=c(-1,1),z=c(0,0),pmat=mapping),col="red")
                          if (likelihood$priorDist!="Single"){
                            lines(trans3d(x=c(sRho,sRho),y=c(-1,1),z=c(0,0),pmat=mapping),col=colPdark)
                          }
                        }
                )
              }
              
            theoryAlpha=1
            # simulations
            switch (likelihood$type,
                    "Samples"={
                      if (!is.null(likelihoodResult$sSims)) {
                        bins<-likelihoodResult$sSimBins
                        dens<-likelihoodResult$sSimDens

                        if (!is.null(dens)){
                          dens<-dens/max(dens)
                          for (i in 1:nrow(dens)) {
                          x<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
                          y1<-c(0,as.vector(matrix(c(dens[i,],dens[i,]),2,byrow=TRUE)),0)
                          
                          polygon(trans3d(x=x*0+pRho[i],y=x,z=y1,pmat=mapping),col=colSsim)
                          }
                          theoryAlpha=0.5
                        }
                      }
                    },
                    "Populations"={
                      if (!is.null(likelihoodResult$pSims)) {
                        bins<-likelihoodResult$pSimBins
                        dens<-likelihoodResult$pSimDens$counts

                        if (!is.null(dens)){
                          gainSim<-sum(dens)*(bins[2]-bins[1])
                          gainTheory<-sum(likelihoodResult$pDens_r)*(likelihoodResult$rp[2]-likelihoodResult$rp[1])
                          dens<-dens/(gainSim/gainTheory)
                          x<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
                          y1<-c(0,as.vector(matrix(c(dens,dens),2,byrow=TRUE)),0)
                          polygon(trans3d(x=x,y=x*0+sRho,z=y1,pmat=mapping),col=colPsim)
                        
                          densP<-likelihoodResult$pSimDensP$counts
                          gainSim<-sum(densP)*(bins[2]-bins[1])
                          gainTheory<-sum(rpw_dens)*(rpw[2]-rpw[1])
                          densP<-densP/(gainSim/gainTheory)
                          yP<-c(0,as.vector(matrix(c(densP,densP),2,byrow=TRUE)),0)
                          polygon(trans3d(x=x,y=x*0+1,z=yP,pmat=mapping),col = addTransparency(colPdark,0.25),border=NA)
                          
                          densS<-likelihoodResult$pSimDensS$counts
                          gainSim<-sum(densS)*(bins[2]-bins[1])
                          densS<-densS/(gainSim/gainTheory)
                          yS<-c(0,as.vector(matrix(c(densS,densS),2,byrow=TRUE)),0)
                          polygon(trans3d(x=x*0-1,y=x,z=yS,pmat=mapping),col = addTransparency(colSdark,0.25),border=NA)
                          
                          theoryAlpha=0.5
                        }
                      }
                    }
            )
            
            if (likelihood$showTheory){
              # main distribution
              # vertical lines
              switch (likelihood$type,
                      "Samples"={
                        dens_r<-likelihoodResult$sDens_r
                          doConnecting<-TRUE
                          if (length(pRho)>1) theoryAlpha<-0.85
                          for (i in 1:length(pRho)) {
                            if (!is.null(dens_r)){
                              use<-dens_r[i,]>0.005
                              if (likelihood$cutaway) {
                                use<-use & rs>=sRho
                              }
                              use_r<-rs[use]
                              use_r<-use_r[c(1,1:length(use_r),length(use_r))]
                              if (is.null(likelihoodResult$sSims)) {
                              polygon (trans3d(x = use_r*0+pRho[i], y = use_r, z = c(0,dens_r[i,use],0), pmat = mapping), col = addTransparency(colS,theoryAlpha), lwd=1)
                              } else {
                                polygon (trans3d(x = use_r*0+pRho[i], y = use_r, z = c(0,dens_r[i,use],0), pmat = mapping), col = addTransparency(colS,highTransparency), lwd=1)
                              }
                            }
                            z<-approx(rs,dens_r[i,],sRho)$y
                            lines(trans3d(x=c(pRho[i],pRho[i]),y=c(sRho,sRho),z=c(0,z),pmat=mapping),col=colSdark, lwd=2)
                            if (doConnecting && i<length(pRho)) {
                              z1<-approx(rs,dens_r[i+1,],sRho)$y
                              lines(trans3d(x=c(pRho[i],pRho[i+1]),y=c(sRho,sRho),z=c(z,z1),pmat=mapping),col=colSdark, lwd=3)
                            }
                          }
                      },
                      "Populations"={
                        dens_r<-likelihoodResult$pDens_r
                        if (!is.null(dens_r)){
                          if (is.null(likelihoodResult$pSims)) {
                            polygon (trans3d(x = rp, y = rp*0+sRho, z = dens_r, pmat = mapping), col = addTransparency(colP,theoryAlpha), lwd=1)
                          } else {
                            polygon (trans3d(x = rp, y = rp*0+sRho, z = dens_r, pmat = mapping), col = addTransparency(colP,highTransparency), lwd=1)
                          }
                        }
                        lines(trans3d(x=c(rp_ci[1],rp_ci[1]),y=c(sRho,sRho),z=c(0,approx(rp,dens_r,rp_ci[1])$y-0.01),pmat=mapping),col="red", lwd=2,lty=3)
                        lines(trans3d(x=c(rp_ci[2],rp_ci[2]),y=c(sRho,sRho),z=c(0,approx(rp,dens_r,rp_ci[2])$y-0.01),pmat=mapping),col="red", lwd=2,lty=3)
                        lines(trans3d(x=c(rp_peak,rp_peak),y=c(sRho,sRho),z=c(0,dens_at_peak-0.01),pmat=mapping),col="red", lwd=3)
                        if (likelihood$priorDist!="Single"){
                          lines(trans3d(x=c(sRho,sRho),y=c(sRho,sRho),z=c(0,dens_at_sample-0.01),pmat=mapping),col=colPdark,lty=3,lwd=2)
                        }
                        text(trans3d(x=-1,y=1,z=1.05,pmat=mapping),labels=bquote(
                          italic(r)[mle]== bold(.(format(rp_peak,digits=2)))
                          ),col="red",adj=1.5,cex=0.75)
                        text(trans3d(x=-1,y=1,z=1.05,pmat=mapping),labels=bquote(
                          llr(italic(r)[mle]/italic(r)[0])==bold(.(format(log(1/approx(rp,dens_r,0)$y),digits=2)))~";"~llr(italic(r)[s]/italic(r)[mle])== bold(.(format(log(dens_at_sample),digits=2)))~";"~llr(italic(r)[s]/italic(r)[0])==bold(.(format(log(dens_at_sample/approx(rp,dens_r,0)$y),digits=2)))
                        ),col=colPdark,adj=-0.02,cex=0.75)
                                                                         # "   llr(r[s] ! 0)=",format(log10(dens_at_sample/approx(rp,dens_r,0)$y),digits=2)),col=colPdark,adj=-0.1)
                        # text(trans3d(x=-1,y=1,z=1.05,pmat=mapping),paste0("  mle(r)=",format(rp_peak,digits=2)),col="red",adj=1.1)
                      #   text(trans3d(x=-1,y=1,z=1.05,pmat=mapping),paste0("llr(r[s] ! mle)=",format(log10(dens_at_sample),digits=2),
                      #                                                     "   llr(r[s] ! 0)=",format(log10(dens_at_sample/approx(rp,dens_r,0)$y),digits=2)),col=colPdark,adj=-0.1)
                      }
              )

            }
            
            # finish off plot box
            lines(trans3d(x=c(-1, 1, 1),y=c(-1,-1,1),z=c(1,1,1),pmat=mapping), col="#888888", lty=3)        
            lines(trans3d(x=c(1,1),y=c(-1,-1),z=c(0,1),pmat=mapping),col="#888888",lty=3)
          },
  "Samples"={
    par(bg=maincolours$graphC)
    
  },
  "2D"={
    par(bg=maincolours$graphC,pin=c(1.33,1)*3,mar=c(5,5,1,0))
    
    # make the back wall
    rpw<-seq(-1,1,length=200)
    switch (likelihood$type,
            "Populations"={rpw_dens<-populationDensityFunction(rpw,likelihood)},
            "Samples"={rpw_dens<-rpw*0}
    )
    rpw<-c(-1,rpw,1)
    rpw_dens<-c(0,rpw_dens,0)
    plot(x=rpw,y=rpw_dens,xlab="Populations",ylab=ylab,type="n",xlim=c(-1,1),ylim=c(0,1))
    u <- par("usr") # The coordinates of the plot area
    rect(u[1], u[3], u[2], u[4], col="#AAAAAA", border=NA)
    
    # make the back wall
    polygon(x=rpw,y=rpw_dens,col="lightgrey")
    
    theoryAlpha=1
    # simulations
    switch (likelihood$type,
            "Populations"={
              if (!is.null(likelihoodResult$pSims)) {
                bins<-likelihoodResult$pSimBins
                dens<-likelihoodResult$pSimDens
                dens<-dens$counts
                
                if (!is.null(dens)){
                  dens<-dens/max(dens)
                  x<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
                  y1<-c(0,as.vector(matrix(c(dens,dens),2,byrow=TRUE)),0)
                  
                  polygon(x=x,y=y1,col=colP)
                  theoryAlpha=0.5
                }
              }
            },
            "Samples"={
              if (!is.null(likelihoodResult$sSims)) {
                bins<-likelihoodResult$sSimBins
                dens<-likelihoodResult$sSimDens
                dens<-dens$counts
                
                if (!is.null(dens)){
                  dens<-dens/max(dens)
                  x<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
                  y1<-c(0,as.vector(matrix(c(dens,dens),2,byrow=TRUE)),0)
                  
                  polygon(x=x,y=y1,col=colS)
                  theoryAlpha=0.5
                  
                  lines(c(0,0)+abs(sRho),c(0,1),col="black")
                  text(abs(sRho),1,format(mean(likelihoodResult$sr>abs(sRho)),digits=2),adj=c(0,1))
                  lines(c(0,0)-abs(sRho),c(0,1),col="black")
                  text(-abs(sRho),1,format(mean(likelihoodResult$sr<(-abs(sRho))),digits=2),adj=c(1,1))
                  
                }
              }
            }
    )
    
    if (likelihood$showTheory){
      rs<-likelihoodResult$rs
      rp<-likelihoodResult$rp
      rp_peak<-likelihoodResult$rp_peak
      
      dens_at_peak<-likelihoodResult$dens_at_peak
      dens_at_sample<-likelihoodResult$dens_at_sample

      # main distribution
      switch (likelihood$type,
              "Samples"={
                dens_r<-likelihoodResult$sDens_r
                if (!is.null(dens_r)){
                  polygon (x = rs, y = dens_r, col = addTransparency(colS,theoryAlpha), lwd=1)
                }
              },
              "Populations"={
                dens_r<-likelihoodResult$pDens_r
                if (!is.null(dens_r)){
                  polygon (x = rp, y = dens_r, col = addTransparency(colP,theoryAlpha), lwd=1)
                }
              }
      )
      
      # vertical lines
      switch (likelihood$type,
              "Samples"={
                lines(x=c(sRho,sRho),y=c(0,rSamplingDistr(sRho,pRho,n)/rSamplingDistr(0,0,n)-0.01),col="black", lwd=1.5)
              },
              "Populations"={
                lines(x=c(sRho,sRho),y=c(0,dens_at_sample-0.01),col="black",lwd=1.5)
                if (likelihood$priorDist!="Uniform_r" && !is.null(rp_peak)){
                  lines(x=c(rp_peak,rp_peak),y=c(0,dens_at_peak-0.01),col="white",lwd=1)
                  lines(x=c(rp_peak,rp_peak),y=c(0,dens_at_peak-0.01),col="red",lty=3,lwd=1)
                }
              }
      )
      
    }
  }
)
}
