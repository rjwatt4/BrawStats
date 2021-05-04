colS="#88DD99"
colP="#8899DD"

drawLikelihood <- function(IV,DV,effect,design,likelihood,likelihoodResult){
  rho<-effect$rIV
  
  # make the distribution        
  n<-design$sN

  pRho<-likelihood$populationES
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
            par(bg=maincolours$graphC)
            persp(x, y, z, zlim = range(c(0,1), na.rm = TRUE),
                  theta = likelihood$azimuth, phi = likelihood$elevation, r=likelihood$range, 
                  ticktype = "detailed", 
                  expand = 0.5, col = "#aaaaaa",
                  cex.axis=0.6,
                  xlab = "Populations", ylab = "Samples", zlab = "Likelihood"
            )->mapping
            
            
            # lines on the floor
            lines(trans3d(x=c(-1,1),y=c(0,0),z=c(0,0),pmat=mapping),col="black",lty=3)
            lines(trans3d(x=c(0,0),y=c(-1,1),z=c(0,0),pmat=mapping),col="black",lty=3)
            lines(trans3d(x=c(-1,1),y=c(sRho,sRho),z=c(0,0),pmat=mapping),col=colS)
            lines(trans3d(x=c(pRho,pRho),y=c(-1,1),z=c(0,0),pmat=mapping),col=colP)
            
            # make the back wall
            rpw<-seq(-1,1,length=200)
            rpw_dens<-populationDensityFunction(rpw,likelihood)
            
            polygon(trans3d(x=rpw,y=rpw*0+1,z=rpw_dens,pmat=mapping),col="lightgrey")

            theoryAlpha=1
            # simulations
            switch (likelihood$type,
                    "Samples"={
                      if (!is.null(likelihoodResult$sSims)) {
                        bins<-likelihoodResult$sSimBins
                        dens<-likelihoodResult$sSimDens
                        dens<-dens$counts
                        
                        if (!is.null(dens)){
                          dens<-dens/max(dens)
                          x<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
                          y1<-c(0,as.vector(matrix(c(dens,dens),2,byrow=TRUE)),0)
                          
                          polygon(trans3d(x=x*0+pRho,y=x,z=y1,pmat=mapping),col=colS)
                          theoryAlpha=0.5
                        }
                      }
                    },
                    "Populations"={
                      if (!is.null(likelihoodResult$pSims)) {
                        bins<-likelihoodResult$pSimBins
                        dens<-likelihoodResult$pSimDens
                        dens<-dens$counts
                        
                        if (!is.null(dens)){
                          dens<-dens/max(dens)
                          x<-as.vector(matrix(c(bins,bins),2,byrow=TRUE))
                          y1<-c(0,as.vector(matrix(c(dens,dens),2,byrow=TRUE)),0)
                          
                          polygon(trans3d(x=x,y=x*0+sRho,z=y1,pmat=mapping),col=colP)
                          theoryAlpha=0.5
                        }
                      }
                    }
            )
            
            
            if (likelihood$showTheory){
              rs<-likelihoodResult$rs
              rp<-likelihoodResult$rp
              rp_peak<-likelihoodResult$rp_peak
              rp_ci<-likelihoodResult$rp_ci
              
              r_at_peak_dens<-likelihoodResult$r_at_peak_dens
              expected_r_at_peak_dens<-likelihoodResult$expected_r_at_peak_dens
              
              # horizontal lines
              switch (likelihood$type,
                      "Samples"={
                      },
                      "Populations"={
                        lines(trans3d(x=c(rp_ci[1],rp_ci[1]),y=c(-1,1),z=c(0,0),pmat=mapping),col="red",lty=3)
                        lines(trans3d(x=c(rp_ci[2],rp_ci[2]),y=c(-1,1),z=c(0,0),pmat=mapping),col="red",lty=3)
                        lines(trans3d(x=c(rp_peak,rp_peak),y=c(-1,1),z=c(0,0),pmat=mapping),col="red")
                        if (likelihood$populationDist=="Exp"){
                          lines(trans3d(x=c(sRho,sRho),y=c(-1,1),z=c(0,0),pmat=mapping),col="black")
                        }
                      }
              )
              # main distribution
              switch (likelihood$type,
                      "Samples"={
                        dens_r<-likelihoodResult$sDens_r
                        if (!is.null(dens_r)){
                          col<-col2rgb(colS)/255
                          polygon (trans3d(x = rs*0+pRho, y = rs, z = dens_r, pmat = mapping), col = rgb(col[1],col[2],col[3],theoryAlpha), lwd=1)
                        }
                      },
                      "Populations"={
                        dens_r<-likelihoodResult$pDens_r
                        if (!is.null(dens_r)){
                          col<-col2rgb(colP)/255
                          polygon (trans3d(x = rp, y = rp*0+sRho, z = dens_r, pmat = mapping), col = rgb(col[1],col[2],col[3],theoryAlpha), lwd=1)
                        }
                      }
              )
              # vertical lines
              switch (likelihood$type,
                      "Samples"={
                        lines(trans3d(x=c(pRho,pRho),y=c(sRho,sRho),z=c(0,rSamplingDistr(sRho,pRho,n)/rSamplingDistr(0,0,n)-0.01),pmat=mapping),col="black", lwd=1)
                      },
                      "Populations"={
                        if (likelihood$populationDist=="Exp"){
                          lines(trans3d(x=c(rp_peak,rp_peak),y=c(sRho,sRho),z=c(0,r_at_peak_dens-0.01),pmat=mapping),col="red", lwd=1)
                          lines(trans3d(x=c(sRho,sRho),y=c(sRho,sRho),z=c(0,expected_r_at_peak_dens-0.01),pmat=mapping),col="black",lty=3,lwd=1)
                        }
                        else
                        {
                          lines(trans3d(x=c(rp_peak,rp_peak),y=c(sRho,sRho),z=c(0,r_at_peak_dens-0.01),pmat=mapping),col="red", lwd=1)
                        }
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
    par(bg=maincolours$graphC,pin=c(1.33,1)*3)
    
    # make the back wall
    rpw<-seq(-1,1,length=200)
    switch (likelihood$type,
            "Populations"={rpw_dens<-populationDensityFunction(rpw,likelihood)},
            "Samples"={rpw_dens<-rpw*0}
    )
    rpw<-c(-1,rpw,1)
    rpw_dens<-c(0,rpw_dens,0)
    plot(x=rpw,y=rpw_dens,xlab="Populations",ylab="Likelihood",type="n",xlim=c(-1,1),ylim=c(0,1))
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
                }
              }
            }
    )
    
    if (likelihood$showTheory){
      rs<-likelihoodResult$rs
      rp<-likelihoodResult$rp
      rp_peak<-likelihoodResult$rp_peak
      
      r_at_peak_dens<-likelihoodResult$r_at_peak_dens
      expected_r_at_peak_dens<-likelihoodResult$expected_r_at_peak_dens

      # main distribution
      switch (likelihood$type,
              "Samples"={
                dens_r<-likelihoodResult$sDens_r
                if (!is.null(dens_r)){
                  col<-col2rgb(colS)/255
                  polygon (x = rs, y = dens_r, col = rgb(col[1],col[2],col[3],theoryAlpha), lwd=1)
                }
              },
              "Populations"={
                dens_r<-likelihoodResult$pDens_r
                if (!is.null(dens_r)){
                  col<-col2rgb(colP)/255
                  polygon (x = rp, y = dens_r, col = rgb(col[1],col[2],col[3],theoryAlpha), lwd=1)
                }
              }
      )
      
      # vertical lines
      switch (likelihood$type,
              "Samples"={
                lines(x=c(sRho,sRho),y=c(0,rSamplingDistr(sRho,pRho,n)/rSamplingDistr(0,0,n)-0.01),col="black", lwd=1)
              },
              "Populations"={
                if (likelihood$populationDist=="Exp"){
                  lines(x=c(rp_peak,rp_peak),y=c(0,r_at_peak_dens-0.01),col="red",lty=3,lwd=1)
                  lines(x=c(sRho,sRho),y=c(0,expected_r_at_peak_dens-0.01),col="black",lty=3,lwd=1)
                }
                else
                {
                  lines(x=c(rp_peak,rp_peak),y=c(0,r_at_peak_dens-0.01),col="red", lwd=1)
                }
              }
              # "Populations"={
              #   if (likelihood$populationDist=="Exp"){
              #     lines(trans3d(x=c(rp_peak,rp_peak),y=c(sRho,sRho),z=c(0,r_at_peak_dens-0.01),pmat=mapping),col="red", lwd=1)
              #     lines(trans3d(x=c(sRho,sRho),y=c(sRho,sRho),z=c(0,expected_r_at_peak_dens-0.01),pmat=mapping),col="black",lty=3,lwd=1)
              #   }
              #   else
              #   {
              #     lines(trans3d(x=c(rp_peak,rp_peak),y=c(sRho,sRho),z=c(0,r_at_peak_dens-0.01),pmat=mapping),col="red", lwd=1)
              #   }
      )
      
    }
  }
)
}
