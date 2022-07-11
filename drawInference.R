drawInference<-function(IV,IV2,DV,effect,design,evidence,result,disp){
  
  r<-effect$rIV
  if (!is.null(IV2)){
    r<-c(r,effect$rIV2,effect$rIVIV2DV)
  }
  
  switch (disp,
          "p"={g<-p_plot(result,IV,IV2,DV,r)},
          "r"={g<-r_plot(result,IV,IV2,DV,r)},
          "w"={g<-w_plot(result,IV,IV2,DV,r)},
          "nw"={g<-nw_plot(result,IV,IV2,DV,r)},
          "e1"={g<-e1_plot(result,IV,IV2,DV,r)},
          "e2"={g<-e2_plot(result,IV,IV2,DV,r)},
          "ci1"={g<-ci1_plot(result,IV,IV2,DV,r)},
          "ci2"={g<-ci2_plot(result,IV,IV2,DV,r)},
          "ln(lr)"={g<-llr_plot(result,IV,IV2,DV,r)}
  )
  
  g+ggtitle(result$an_name)
}


draw2Inference<-function(IV,IV2,DV,effect,design,evidence,result,disp1,disp2){
  
  r<-effect$rIV
  if (!is.null(IV2)){
    r<-c(r,effect$rIV2,effect$rIVIV2DV)
  }
  pvals<-result$pIV
  
  xsc<-0
  switch (disp1,
          "p"={
            d1<-result$pIV
            if (pPlotScale=="log10"){xsc<-1}
            xlim<-c(0,1)
          },
          "r"={
            d1<-result$rIV
            xlim<-c(-1, 1)
          },
          "w"={
            d1<-result$rIV
            d1<-rn2w(d1,result$nval)
            if (wPlotScale=="log10"){ xsc<-1}
            xlim<-c(0,1)
          },
          "ln(lr)"={
            d1<-atanh(result$rIV)
            d1<-r2llr(d1,result$nval)
            xlim<-c(-0.1, 10)
            disp1<-bquote(log[e](lr))
          }
  )
  
  ysc<-0
  switch (disp2,
          "p"={
            d2<-result$pIV
            if (pPlotScale=="log10"){
              ysc<-1
              }
            ylim<-c(0,1)
          },
          "r"={
            d2<-result$rIV
            ylim<-c(-1, 1)
          },
          "w"={
            d2<-rn2w(result$rIV,result$nval)
            if (wPlotScale=="log10"){ ysc<-1}
            ylim<-c(0,1)
          },
          "ln(lr)"={
            d2<-r2llr(result$rIV,result$nval)
            ylim<-c(-0.1, 10)
            disp2<-bquote(log[e](lr))
          }
  )
  
  if (xsc==1) {
    d1<-log10(d1)
    xlim<-c(log10(min_p), 0)
    disp1<-bquote(log[10] (.(disp1)))
  }
  if (ysc==1) {
    d2<-log10(d2)
    ylim<-c(log10(min_p), 0)
    disp2<-bquote(log[10] (.(disp2)))
  }
  pts<-data.frame(x=d1,y=d2)
  
  g<-ggplot(pts,aes(x=x, y=y))
  
  dotsize=max(3.5,sqrt(500/length(d1)))
  
  if (useSignificanceCols){
    c1=plotcolours$infer_sigC
    c2=plotcolours$infer_nsigC
  } else {
    c1=plotcolours$descriptionC
    c2=plotcolours$descriptionC
  }
  use<-(pvals>=alpha)
  pts1=pts[use,]
  g<-g+geom_point(data=pts1,aes(x=x, y=y),shape=21, colour = "black", fill = c2, size = dotsize)
  pts2=pts[!use,]
  g<-g+geom_point(data=pts2,aes(x=x, y=y),shape=21, colour = "black", fill = c1, size = dotsize)
  
  g<-g+theme(legend.position = "none")+plotTheme
  if (xsc==0) {
    g<-g+scale_x_continuous(limits = xlim)
  } else {
    g<-g+scale_x_continuous(limits = xlim,breaks=c(-4,-3,-2,-1,0),labels=c(0.0001,0.001,0.01,0.1,1))
  }
  if (ysc==0) {
    g<-g+scale_y_continuous(limits = ylim)
  } else {
    g<-g+scale_y_continuous(limits = ylim,breaks=c(-4,-3,-2,-1,0),labels=c(0.0001,0.001,0.01,0.1,1))
  }
  
  g<-g+xlab(disp1)+ylab(disp2)
  g+ggtitle(result$an_name)
}
