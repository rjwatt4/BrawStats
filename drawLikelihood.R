colSdark="#227733"
colSsim="#66BB77"
colS="#88DD99"
colPdark="#223377"
colPsim="#6677BB"
colP="#8899DD"
highTransparency=0.25
doConnecting<-TRUE
colVline="#FF2200"

addTransparency <- function(col,alpha) {
  col<-col2rgb(col)/255
  rgb(col[1],col[2],col[3],alpha)
}

drawLikelihood <- function(IV,DV,effect,design,likelihood,likelihoodResult){
  # make the distribution        

  switch (likelihood$type,
          "Samples"={
            likelihoodResult<-likelihoodResult$samples
            ylab<-"Probability Density"
            col=colP
            col2=colS
          },
          "Populations"={
            likelihoodResult<-likelihoodResult$populations
            ylab<-"Likelihood"
            col=colS
            col2=colP
          }
  )

  pRho<-likelihoodResult$pRho
  sRho<-likelihoodResult$sRho
  n<-likelihoodResult$n[1]
  si<-1
  
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
                  ticktype = "simple", 
                  axes = FALSE,
                  expand = 0.5, col = "#aaaaaa",
                  cex.axis=0.6,
                  xlab = "Populations", ylab = "Samples", zlab = ylab
            )->mapping
            
            
            tick_length<-0.05
            view_lims<-c(-1,1)
            plot_ticks=seq(-1,1,0.2)
            charExp<-1.3
            
            tick.x.start <- trans3d(plot_ticks, view_lims[1], 0.0, mapping)
            tick.x.end <- trans3d(plot_ticks , view_lims[1]-tick_length, 0.0, mapping)
            tick.y.start <- trans3d(view_lims[2], plot_ticks, 0.0, mapping)
            tick.y.end <- trans3d(view_lims[2]+ tick_length, plot_ticks , 0.0, mapping)
            
            ticks.x<-trans3d(plot_ticks+tick_length,view_lims[1]- tick_length*2*charExp,0,mapping)
            pos.x<-trans3d(0,1.3*view_lims[1],0,mapping)
            
            ticks.y<-trans3d(view_lims[2]+tick_length*charExp+0.02,plot_ticks-0.02,0,mapping)
            pos.y<-trans3d(1.3*view_lims[2],0,0,mapping)
            
            pos.z<-trans3d(-1*view_lims[2],-1.1*view_lims[2],0.5,mapping)
            
            segments(tick.x.start$x, tick.x.start$y, tick.x.end$x, tick.x.end$y)
            segments(tick.y.start$x, tick.y.start$y, tick.y.end$x, tick.y.end$y)
            
            text(ticks.x$x,ticks.x$y,plot_ticks,cex=0.6*charExp,adj=c(1,NA))
            text(ticks.y$x,ticks.y$y,plot_ticks,cex=0.6*charExp,adj=c(0,NA))
            
            text(pos.x$x,pos.x$y,"Population(r)",font=2,adj=c(1,1),cex=0.8*charExp)
            text(pos.y$x,pos.y$y,"Sample(r)",font=2,adj=c(0,1),cex=0.8*charExp)
            
            text(pos.z$x,pos.z$y,ylab,font=2,srt=90,cex=0.8*charExp)
            
            # lines on the floor
            lines(trans3d(x=c(-1,1),y=c(0,0),z=c(0,0),pmat=mapping),col="black",lty=3)
            lines(trans3d(x=c(0,0),y=c(-1,1),z=c(0,0),pmat=mapping),col="black",lty=3)
            lines(trans3d(x=c(-1,1),y=c(sRho[si],sRho[si]),z=c(0,0),pmat=mapping),col=colSdark)
            for (i in 1:length(pRho)) {
            lines(trans3d(x=c(pRho[i],pRho[i]),y=c(-1,1),z=c(0,0),pmat=mapping),col=colP)
            }
            
            # make the back walls
            # population wall
            rpw<-seq(-1,1,length=200)*0.99
            rpw_dens<-populationDensityFunction(rpw,likelihood)
            rpw_dens[rpw_dens>1 | is.na(rpw_dens)]<-1
            if (is.null(likelihoodResult$Sims$pSims)) {
              if (likelihood$Use=="none" && likelihood$type=="Populations") {
                polygon(trans3d(x=c(rpw[1],rpw,rpw[length(rpw)]),y=c(1,rpw*0+1,1),z=c(0,rpw_dens,0),pmat=mapping),col=addTransparency(colPdark,0.05))
              } else {
                polygon(trans3d(x=c(rpw[1],rpw,rpw[length(rpw)]),y=c(1,rpw*0+1,1),z=c(0,rpw_dens,0),pmat=mapping),col=addTransparency(colPdark,0.25))
              }
            } else {
              polygon(trans3d(x=c(rpw[1],rpw,rpw[length(rpw)]),y=c(1,rpw*0+1,1),z=c(0,rpw_dens,0),pmat=mapping),col=addTransparency(colPdark,0.25))
            }
            if (likelihood$type=="Populations" && nrow(likelihoodResult$Theory$spDens_r)>1) {
              # show the joint likelihood function
              pDens_r<-likelihoodResult$Theory$pDens_r
              yP<-c(0,pDens_r,0)
              rs<-likelihoodResult$Theory$rs
              x<-c(1, 1:length(rs),1)
              x<-rs[x]
              polygon(trans3d(x=x,y=x*0+1,z=yP,pmat=mapping),col = addTransparency(colPdark,0.5),border=NA)
            }
            
            # sample wall
              rsw<-likelihoodResult$Theory$rs
              rsw_dens<-likelihoodResult$Theory$sDens_r_sum
              rsw_dens<-rsw_dens/max(rsw_dens,na.rm=TRUE)
              x <- -c(1,rsw*0+1,1)
              y <- c(rsw[1],rsw,rsw[length(rsw)])
              z <- c(0,rsw_dens,0)# *(1-likelihood$world$populationNullp)
              if (is.null(likelihoodResult$Sims$pSimDens$counts)) {
                if (likelihood$Use=="none" && likelihood$type=="Populations") {
                  polygon(trans3d(x=x,y=y,z=z,pmat=mapping),col=addTransparency(colSdark,0.05))
                } else {
                  polygon(trans3d(x=x,y=y,z=z,pmat=mapping),col=colSdark)
                }
              } else {
                polygon(trans3d(x=x,y=y,z=z,pmat=mapping),col=addTransparency(colSdark,0.25))
              }
              
              
              if (likelihood$likelihoodTheory){
                # horizontal lines
                switch (likelihood$type,
                        "Samples"={
                          rs<-likelihoodResult$Theory$rs
                        },
                        "Populations"={
                          if (!is.na(likelihood$targetSample)) {
                            rp_peak<-likelihoodResult$Theory$rp_peak
                            rp_peakC<-likelihoodResult$Theory$rp_peakC
                            rp_ci<-likelihoodResult$Theory$rp_ci
                          dens_at_peak<-likelihoodResult$Theory$dens_at_peak
                          dens_at_sample<-likelihoodResult$Theory$dens_at_sample
                          rp<-likelihoodResult$Theory$rp
                          
                          lines(trans3d(x=c(rp_ci[1],rp_ci[1]),y=c(-1,1),z=c(0,0),pmat=mapping),col="red",lty=3,lwd=2)
                          lines(trans3d(x=c(rp_ci[2],rp_ci[2]),y=c(-1,1),z=c(0,0),pmat=mapping),col="red",lty=3,lwd=2)
                          lines(trans3d(x=c(rp_peak,rp_peak),y=c(-1,1),z=c(0,0),pmat=mapping),col="red")
                          if (likelihood$world$populationPDF!="Single"){
                            lines(trans3d(x=c(sRho[si],sRho[si]),y=c(-1,1),z=c(0,0),pmat=mapping),col=colPdark)
                          }
                          }
                        }
                )
              }
              
            theoryAlpha=1
            # simulations
            switch (likelihood$type,
                    "Samples"={
                      simAlpha<-1
                      if (likelihood$likelihoodTheory){
                        dens_r<-likelihoodResult$Theory$sDens_r
                      } else {dens_r<-NULL}
                      if (length(pRho)>1) {
                        theoryAlpha<-0.5
                        simAlpha<-0.5
                      }
                      if (length(pRho)>11) {
                        theoryAlpha<-theoryAlpha/2
                        simAlpha<-simAlpha/2
                      }
                      if (!is.null(likelihoodResult$Sims$sSimDens)) {
                        bins<-likelihoodResult$Sims$sSimBins
                        dens<-likelihoodResult$Sims$sSimDens
                        dens<-dens/max(dens)
                        theoryAlpha=0.25
                        if (likelihood$cutaway) {
                          waste<-sum(bins<=sRho[si])
                          use_s<-(waste):length(bins)
                          bins<-bins[use_s]
                          bins[1]<-sRho[si]
                          use_s<-use_s[1:(length(use_s)-1)]
                        } else {use_s<-(1:ncol(dens))}
                        x1<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
                      } else {dens<-NULL}

                      
                      for (i in 1:length(pRho)) {
                        # draw simulations
                        if (!is.null(dens)){
                          y1<-c(0,as.vector(matrix(c(dens[i,use_s],dens[i,use_s]),2,byrow=TRUE)),0)
                          polygon(trans3d(x=x1*0+pRho[i],y=x1,z=y1,pmat=mapping),col=addTransparency(colSsim,simAlpha))
                        }
                        
                        # now draw theory
                        if (!is.null(dens_r)){
                          use_r<-dens_r[i,]>0.005
                          if (likelihood$cutaway) {
                            use_r<-use_r & rs>=sRho[si]
                          }
                          rs_use<-rs[use_r]
                          rs_use<-rs_use[c(1,1:length(rs_use),length(rs_use))]
                          if (is.null(likelihoodResult$Sims$sSims) && is.na(sRho)) {
                            col<-addTransparency(colS,1)
                          } else {
                            col<-addTransparency(colS,highTransparency)
                          }
                          polygon (trans3d(x = rs_use*0+pRho[i], y = rs_use, z = c(0,dens_r[i,use_r],0), pmat = mapping), col = col, lwd=1)
                        }
                        # vertical lines
                        z<-approx(rs,dens_r[i,],sRho[si])$y
                        lines(trans3d(x=c(pRho[i],pRho[i]),y=c(sRho[si],sRho[si]),z=c(0,z),pmat=mapping),col=colVline, lwd=3)
                        # connecting lines
                        if (doConnecting && length(pRho)>5 && i<length(pRho)) {
                          z1<-approx(rs,dens_r[i+1,],sRho[si])$y
                          lines(trans3d(x=c(pRho[i],pRho[i+1]),y=c(sRho[si],sRho[si]),z=c(z,z1),pmat=mapping),col=colVline, lwd=3)
                        }
                      }
                    },
                    "Populations"={
                      if (!is.null(likelihoodResult$Sims$pSims)) {
                        bins<-likelihoodResult$Sims$pSimBins
                        dens<-likelihoodResult$Sims$pSimDens$counts
                        
                        pr_effectS<-likelihoodResult$Sims$pSims
                        pr_effectP<-likelihoodResult$Sims$pSimsP
                        keep<-abs(pr_effectS-likelihood$targetSample)<likelihood$likelihoodSimSlice
                        pr_effectP_use<-pr_effectP[keep]
                        dens<-hist(pr_effectP_use,bins,plot=FALSE)$counts
                        
                        if (!is.null(dens)){
                          x<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
                          
                          #population wall
                          densP<-likelihoodResult$Sims$pSimDensP$counts
                          gainSim<-sum(densP)*(bins[2]-bins[1])
                          gainTheory<-sum(rpw_dens)*(rpw[2]-rpw[1])
                          densP<-densP/(gainSim/gainTheory)
                          if (max(densP)>1.2) {densP<-densP/max(densP)*1.2}
                          yP<-c(0,as.vector(matrix(c(densP,densP),2,byrow=TRUE)),0)
                          polygon(trans3d(x=x,y=x*0+1,z=yP,pmat=mapping),col = addTransparency(colPdark,0.5),border=NA)
                          
                          # sample wall
                          densS<-likelihoodResult$Sims$pSimDensS$counts
                          gainSim<-sum(densS)*(bins[2]-bins[1])
                          gainTheory<-sum(rsw_dens)*(rsw[2]-rsw[1])
                          densS<-densS/(gainSim/gainTheory)
                          if (max(densS)>1.2) {densS<-densS/max(densS)*1.2}
                          yS<-c(0,as.vector(matrix(c(densS,densS),2,byrow=TRUE)),0)
                          polygon(trans3d(x=x*0-1,y=x,z=yS,pmat=mapping),col = addTransparency(colSdark,0.25),border=NA)
                          
                          #slice of interest
                          gainSim<-sum(dens)*(bins[2]-bins[1])
                          gainTheory<-sum(likelihoodResult$Theory$pDens_r)*(likelihoodResult$Theory$rp[2]-likelihoodResult$Theory$rp[1])
                          dens<-dens/(gainSim/gainTheory)
                          if (max(dens)>1.2) {dens<-dens/max(dens)*1.2}
                          y1<-c(0,as.vector(matrix(c(dens,dens),2,byrow=TRUE)),0)
                          polygon(trans3d(x=x,y=x*0+sRho[si],z=y1,pmat=mapping),col=colPsim,border=NA)
                        }
                      }
                      # draw theory main distribution & lines
                      if (likelihood$likelihoodTheory){
                        theoryAlpha=0.5
                        if (!is.na(likelihood$targetSample)) {
                          dens_r<-likelihoodResult$Theory$spDens_r
                          pDens_r<-likelihoodResult$Theory$pDens_r
                          if (!is.null(dens_r)){
                            use_si<-order(-sRho)
                            # main distribution
                            for (si in use_si) {
                              if (is.null(likelihoodResult$Sims$pSims)) {
                                polygon (trans3d(x = rp, y = rp*0+sRho[si], z = dens_r[si,], pmat = mapping), col = addTransparency(colP,theoryAlpha), lwd=1)
                              } else {
                                polygon (trans3d(x = rp, y = rp*0+sRho[si], z = dens_r[si,], pmat = mapping), col = addTransparency(colP,highTransparency), lwd=1)
                              }
                            }
                          }
                          # vertical lines
                          if (length(sRho)==1) {
                            lines(trans3d(x=c(rp_ci[1],rp_ci[1]),y=c(sRho[si],sRho[si]),z=c(0,approx(rp,dens_r[si,],rp_ci[1])$y-0.01),pmat=mapping),col="red", lwd=2,lty=3)
                            lines(trans3d(x=c(rp_ci[2],rp_ci[2]),y=c(sRho[si],sRho[si]),z=c(0,approx(rp,dens_r[si,],rp_ci[2])$y-0.01),pmat=mapping),col="red", lwd=2,lty=3)
                            # lines(trans3d(x=c(rp_peak,rp_peak),y=c(sRho[si],sRho[si]),z=c(0,dens_at_peak-0.01),pmat=mapping),col="red", lwd=3)
                            lines(trans3d(x=c(rp_peakC,rp_peakC),y=c(sRho[si],sRho[si]),z=c(0,dens_at_peak-0.01),pmat=mapping),col="red", lwd=3)
                            if (likelihood$world$populationPDF!="Single"){
                              lines(trans3d(x=c(sRho[si],sRho[si]),y=c(sRho[si],sRho[si]),z=c(0,dens_at_sample[si]-0.01),pmat=mapping),col=colPdark,lty=3,lwd=2)
                            }
                          }
                          if (likelihood$textResult) {
                            text(trans3d(x=-1,y=1,z=1.05,pmat=mapping),labels=bquote(
                              italic(r)[mle]== bold(.(format(rp_peak,digits=2)))
                            ),col="red",adj=1.5,cex=0.9)
                            text(trans3d(x=-1,y=1,z=1.05,pmat=mapping),labels=bquote(
                              llr(italic(r)[mle]/italic(r)[0])==bold(.(format(log(1/approx(rp,pDens_r,0)$y),digits=2)))~";"~
                                llr(italic(r)[s]/italic(r)[0])==bold(.(format(log(dens_at_sample/approx(rp,pDens_r,0)$y),digits=2)))
                            ),col=colPdark,adj=-0.02,cex=0.9)
                          }
                        }
                      }
                    }
            )

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
              if (!is.null(likelihoodResult$Sims$pSims)) {
                bins<-likelihoodResult$Sims$pSimBins
                dens<-likelihoodResult$Sims$pSimDens
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
              if (!is.null(likelihoodResult$Sims$sSims)) {
                bins<-likelihoodResult$Sims$sSimBins
                dens<-likelihoodResult$Sims$sSimDens
                dens<-dens$counts
                
                if (!is.null(dens)){
                  dens<-dens/max(dens)
                  x<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
                  y1<-c(0,as.vector(matrix(c(dens,dens),2,byrow=TRUE)),0)
                  
                  polygon(x=x,y=y1,col=colS)
                  theoryAlpha=0.5
                  
                  lines(c(0,0)+abs(sRho[si]),c(0,1),col="black")
                  text(abs(sRho[si]),1,format(mean(likelihoodResult$sr>abs(sRho)),digits=2),adj=c(0,1))
                  lines(c(0,0)-abs(sRho[si]),c(0,1),col="black")
                  text(-abs(sRho[si]),1,format(mean(likelihoodResult$sr<(-abs(sRho[si]))),digits=2),adj=c(1,1))
                  
                }
              }
            }
    )
    
    if (likelihood$likelihoodTheory){
      rs<-likelihoodResult$Theory$rs
      rp<-likelihoodResult$Theory$rp
      rp_peak<-likelihoodResult$Theory$rp_peak
      
      dens_at_peak<-likelihoodResult$Theory$dens_at_peak
      dens_at_sample<-likelihoodResult$Theory$dens_at_sample

      # main distribution
      switch (likelihood$type,
              "Samples"={
                dens_r<-likelihoodResult$Theory$sDens_r
                if (!is.null(dens_r)){
                  polygon (x = rs, y = dens_r, col = addTransparency(colS,theoryAlpha), lwd=1)
                }
              },
              "Populations"={
                dens_r<-likelihoodResult$Theory$pDens_r
                if (!is.null(dens_r)){
                  polygon (x = rp, y = dens_r, col = addTransparency(colP,theoryAlpha), lwd=1)
                }
              }
      )
      
      # vertical lines
      switch (likelihood$type,
              "Samples"={
                lines(x=c(sRho[si],sRho[si]),y=c(0,rSamplingDistr(sRho[si],pRho,n)/rSamplingDistr(0,0,n)-0.01),col="black", lwd=1.5)
              },
              "Populations"={
                lines(x=c(sRho[si],sRho[si]),y=c(0,dens_at_sample[si]-0.01),col="black",lwd=1.5)
                if (likelihood$world$populationPDF!="Uniform_r" && !is.null(rp_peak)){
                  lines(x=c(rp_peak,rp_peak),y=c(0,dens_at_peak-0.01),col="white",lwd=1)
                  lines(x=c(rp_peak,rp_peak),y=c(0,dens_at_peak-0.01),col="red",lty=3,lwd=1)
                }
              }
      )
      
    }
  }
)
}
