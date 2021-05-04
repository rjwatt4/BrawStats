drawInference<-function(IV,IV2,DV,effect,design,result,disp){
  
  r<-effect$rIV
  if (!is.null(IV2)){
    r<-c(r,effect$rIV2,effect$rIVIV2DV)
  }

      switch (disp,
          "p"={g<-p_plot(result,IV,IV2,DV,r,design$sN)},
          "r"={g<-r_plot(result,IV,IV2,DV,r,design$sN)},
          "w"={g<-w_plot(result,IV,IV2,DV,r,design$sN)},
          "nw"={g<-nw_plot(result,IV,IV2,DV,r,design$sN)},
          "e1"={g<-e1_plot(result,IV,IV2,DV,r,design$sN)},
          "e2"={g<-e2_plot(result,IV,IV2,DV,r,design$sN)},
          "ci1"={
            result$rIV<-r2ci(result$rIV,result$nval[1],-1)
            if (!is.null(IV2)) {
              result$r$direct<-r2ci(result$r$direct,result$nval[1],-1)
              result$r$unique<-r2ci(result$r$unique,result$nval[1],-1)
              result$r$total<-r2ci(result$r$total,result$nval[1],-1)
            }
            g<-r_plot(result,IV,IV2,DV,r,design$sN)
            },
          "ci2"={
            result$rIV<-r2ci(result$rIV,result$nval,+1)
            if (!is.null(IV2)) {
              result$r$direct<-r2ci(result$r$direct,result$nval[1],+1)
              result$r$unique<-r2ci(result$r$unique,result$nval[1],+1)
              result$r$total<-r2ci(result$r$total,result$nval[1],+1)
            }
            g<-r_plot(result,IV,IV2,DV,r,design$sN)
            }
      )

  g+ggtitle(result$an_name)
}
